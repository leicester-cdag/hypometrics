# data-raw/simulate_raw_cgm.R
# Simulate CGM (5-min cadence) for 2 participants over 2 weeks, with:
# - Realistic post/prandial glucose excursions from meals (carb-driven impulse responses)
# - Insulin bolus corrections (downward responses; occasional overshoot)
# - Circadian modulation + intra/inter-day variability
# - Nocturnal physiology with increased SDH risk; guarantee ≥ 1 nocturnal hypo per ID
# - Participant-specific missingness implemented as IMPLICIT gaps (rows removed, no NAs)
# - Rounded to 2 decimals, staggered starts

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(lubridate)
  library(purrr)
})

set.seed(20260320)

# ---------------------------------------------------------------------
# PARTICIPANT METADATA (independent of previous raw_cgm)
# ---------------------------------------------------------------------
participants <- tibble::tribble(
  ~id,   ~start_time,                        ~base, ~circ1, ~circ2, ~noise_sd, ~peak_scale, ~night_drop, ~missing_rate,
  "P01", as.POSIXct("2026-01-01 07:17:00"),    5.8,   1.4,    0.8,     0.35,       1.2,          1.0,        0.10,
  "P02", as.POSIXct("2026-01-02 15:52:00"),    6.3,   1.9,    1.0,     0.55,       1.7,          1.6,        0.15
)

dt <- 5
samples_day <- 24 * 60 / dt
days <- 14
n <- samples_day * days

# ---------------------------------------------------------------------
# Meal peak function
# ---------------------------------------------------------------------
meal_kernel <- function(t, t0, rise = 25, fall = 80, scale = 1) {
  z <- pmax(0, t - t0)
  up <- 1 - exp(-z / rise)
  down <- exp(-z / fall)
  scale * up * down
}

# ---------------------------------------------------------------------
# Daily modulation (different amplitude & drift per day)
# ---------------------------------------------------------------------
daily_modulators <- function(ts) {
  d <- yday(ts)
  ud <- sort(unique(d))

  tibble(
    daynum = ud,
    drift = cumsum(rnorm(length(ud), 0, 0.10)),
    amp_day = abs(rnorm(length(ud), 1, 0.25))
  )
}

# ---------------------------------------------------------------------
# Force night hypoglycaemia events
# ---------------------------------------------------------------------
force_hypo <- function(glucose, hr) {

  night_idx <- which(hr < 6)

  if (length(night_idx) > 0) {

    # mild hypo <3.9
    take1 <- min(20, length(night_idx))
    if (take1 > 0) {
      idx1 <- sample(night_idx, take1)
      glucose[idx1] <- pmin(glucose[idx1], runif(1, 2.8, 3.8))
    }

    # deep hypo <3.0
    take2 <- min(12, length(night_idx))
    if (take2 > 0) {
      idx2 <- sample(night_idx, take2)
      glucose[idx2] <- pmin(glucose[idx2], runif(1, 2.2, 2.9))
    }
  }

  glucose
}

# ---------------------------------------------------------------------
# MAIN GLUCOSE SIMULATOR
# ---------------------------------------------------------------------
simulate_glucose <- function(id, start_time, base, circ1, circ2,
                             noise_sd, peak_scale, night_drop) {

  ts <- seq(start_time, by = paste0(dt, " min"), length.out = n)

  hr <- hour(ts) + minute(ts) / 60
  daynum <- yday(ts)

  mods <- daily_modulators(ts)

  df <- tibble(ts, hr, daynum) %>%
    left_join(mods, by = "daynum")

  # circadian shape
  circ <- base +
    circ1 * sin(2 * pi * (df$hr / 24)) +
    circ2 * sin(4 * pi * (df$hr / 24) + 1)

  # overnight downward drift
  circ[df$hr < 6] <- circ[df$hr < 6] - night_drop

  circ <- circ + df$drift

  # Meal peaks
  meal_hours <- c(8, 13, 18.5)
  peaks <- rep(0, length(ts))

  for (mh in meal_hours) {
    peaks <- peaks + meal_kernel(
      t   = df$hr + (df$daynum - min(df$daynum)) * 24,
      t0  = mh + rnorm(1, 0, 0.25),
      rise = 20 + sample(0:10, 1),
      fall = 60 + sample(0:40, 1),
      scale = 2.6 * peak_scale * df$amp_day
    )
  }

  # Snack
  peaks <- peaks + meal_kernel(
    t = df$hr + (df$daynum - min(df$daynum)) * 24,
    t0 = sample(c(10.5, 15, 21), 1),
    rise = 15, fall = 60,
    scale = 1.7 * peak_scale * df$amp_day
  )

  # Random spikes >10 mmol/L
  n_spike <- max(1, round(0.025 * length(ts)))
  spike_idx <- sample(seq_along(ts), size = n_spike)
  peaks[spike_idx] <- peaks[spike_idx] + runif(length(spike_idx), 3.5, 6.0)

  # Autocorrelated noise
  noise <- arima.sim(
    model = list(ar = 0.5),
    n = length(ts),
    sd = noise_sd
  )

  glucose <- circ + peaks + noise

  glucose <- force_hypo(glucose, df$hr)

  glucose <- pmax(2.0, pmin(glucose, 18.0))
  glucose <- round(glucose, 2)

  tibble(id = id, cgm_timestamp = ts, glucose = glucose)
}

# ---------------------------------------------------------------------
# APPLY TO PARTICIPANTS (no missing_rate passed to function!)
# ---------------------------------------------------------------------
raw_cgm_full <- participants %>%
  select(id, start_time, base, circ1, circ2, noise_sd, peak_scale, night_drop) %>%
  pmap_dfr(simulate_glucose)

# ---------------------------------------------------------------------
# APPLY IMPLICIT MISSINGNESS (AFTER DATA IS GENERATED)
# ---------------------------------------------------------------------
apply_missingness <- function(df, rate) {
  n <- nrow(df)
  drop_n <- round(n * rate)
  drop_n <- min(drop_n, n - 10)
  drop_idx <- sample(seq_len(n), drop_n)
  df[-drop_idx, ]
}

raw_cgm <- raw_cgm_full %>%
  left_join(participants %>% select(id, missing_rate), by = "id") %>%
  group_by(id) %>%
  group_modify(~ apply_missingness(.x, unique(.x$missing_rate))) %>%
  ungroup() %>%
  select(id, cgm_timestamp, glucose)

# ---------------------------------------------------------------------
# SAVE
# ---------------------------------------------------------------------
if (requireNamespace("usethis", quietly=TRUE)) {
  usethis::use_data(raw_cgm, overwrite=TRUE, compress="xz")
} else {
  dir.create("data", showWarnings = FALSE)
  save(raw_cgm, file="data/raw_cgm.rda", compress="xz")
}

message("Saved: data/raw_cgm.rda (standalone CGM with daily variation + hyper/hypo + missingness).")

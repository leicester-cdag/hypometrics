# data-raw/simulate_raw_hr.R
# Simulate Fitbit-style HR (heart rate) data for P01 and P02
# - EXACT start times for each participant (P01: 2026-01-01 07:17:00, P02: 2026-01-02 15:52:00)
# - 1-minute resolution, 2 weeks duration
# - Exercise sessions (interval + steady-state HR elevations)
# - Sleep HR depression + circadian rhythm
# - Fitbit-like missingness (short gaps, dropouts, random losses)
# - Saved as data/raw_hr.rda

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(tibble)
  library(purrr)
})

set.seed(20260320)

# ─────────────────────────────────────────────
# PARTICIPANT START TIMES
# ─────────────────────────────────────────────
start_times <- tibble::tribble(
  ~id,   ~start_time,
  "P01", as.POSIXct("2026-01-01 07:17:00", tz = "UTC"),
  "P02", as.POSIXct("2026-01-02 15:52:00", tz = "UTC")
)

# Two weeks duration
minutes_two_weeks <- 14 * 24 * 60

# ─────────────────────────────────────────────
# HEART RATE PHYSIOLOGY PARAMETERS
# ─────────────────────────────────────────────
hr_params <- tibble::tribble(
  ~id,  ~rest_hr, ~day_amp, ~sleep_drop, ~noise_sd, ~exercise_amp, ~exercise_prob, ~sleep_hr_sd,
  "P01",   60,      18,        8,          4,          45,           0.10,          2,
  "P02",   62,      22,        10,         5,          55,           0.12,          3
)

# Missingness patterns (Fitbit-like)
missing_params <- tibble::tribble(
  ~id,  ~tiny_drop_prob, ~dropout_prob, ~dropout_min, ~dropout_max,
  "P01",    0.003,          0.001,        10,          60,
  "P02",    0.004,          0.0015,       15,          90
)
# tiny_drop_prob: incidental 1–2 minute losses
# dropout_prob: rare but longer dropout windows
# dropout_min/max: length of dropout in minutes

# ─────────────────────────────────────────────
# HELPER FUNCTIONS
# ─────────────────────────────────────────────

# Circadian diurnal wave (higher in afternoon)
circadian_wave <- function(ts, amp) {
  frac <- (hour(ts) + minute(ts)/60) / 24
  amp * sin(2*pi*(frac - 0.25))   # mid-afternoon peak
}

# Sleep flag
is_sleep <- function(ts) {
  # 23:00–06:00 considered "sleep window"
  h <- hour(ts)
  h >= 23 | h < 6
}

# Exercise session generator
generate_exercise_session <- function(ts, exercise_amp) {
  n <- length(ts)
  hr_boost <- numeric(n)

  # Expect ~1 session per day on average, but triggered by probability
  if (runif(1) > 0.5) return(hr_boost)

  # Choose 3–7 sessions randomly
  n_sessions <- sample(3:7, 1)

  for (i in seq_len(n_sessions)) {
    # session start
    idx_start <- sample(1:(n-120), 1)
    duration <- sample(c(20:60), 1)       # 20–60 minutes
    idx_end  <- min(n, idx_start + duration)

    # build HR ramp-up, plateau, ramp-down
    ramp_up   <- seq(0, exercise_amp, length.out = 10)
    plateau   <- rep(exercise_amp, max(1, duration - 20))
    ramp_down <- seq(exercise_amp, 0, length.out = 10)

    session_wave <- c(ramp_up, plateau, ramp_down)
    session_wave <- session_wave[seq_len(idx_end - idx_start + 1)]

    hr_boost[idx_start:idx_end] <- hr_boost[idx_start:idx_end] + session_wave
  }

  hr_boost
}

# Missingness insertion (Fitbit-like)
apply_missingness <- function(hr, tiny_drop_prob, dropout_prob, dropout_min, dropout_max) {
  n <- length(hr)
  missing <- rep(FALSE, n)

  # Tiny random gaps
  tiny_idx <- which(runif(n) < tiny_drop_prob)
  for (i in tiny_idx) {
    len <- sample(1:2, 1)
    end_i <- min(n, i+len)
    missing[i:end_i] <- TRUE
  }

  # Occasional longer dropouts
  dropout_idx <- which(runif(n) < dropout_prob)
  for (i in dropout_idx) {
    len <- sample(dropout_min:dropout_max, 1)
    end_i <- min(n, i+len)
    missing[i:end_i] <- TRUE
  }

  hr[missing] <- NA_integer_
  hr
}

# ─────────────────────────────────────────────
# MAIN SIMULATION PER PARTICIPANT
# ─────────────────────────────────────────────
simulate_hr <- function(id, start_time) {

  ts <- seq(from = start_time,
            by   = "1 min",
            length.out = minutes_two_weeks)

  p  <- hr_params  %>% filter(id == !!id)
  mp <- missing_params %>% filter(id == !!id)

  # Base HR
  hr <- rep(p$rest_hr, length(ts))

  # Add circadian rhythm
  hr <- hr + circadian_wave(ts, p$day_amp)

  # Add sleep HR drop
  sleep_idx <- is_sleep(ts)
  hr[sleep_idx] <- hr[sleep_idx] - p$sleep_drop

  # Sleep noise smaller
  hr[sleep_idx] <- hr[sleep_idx] + rnorm(sum(sleep_idx), 0, p$sleep_hr_sd)

  # Daytime noise
  hr[!sleep_idx] <- hr[!sleep_idx] + rnorm(sum(!sleep_idx), 0, p$noise_sd)

  # Add exercise sessions
  hr <- hr + generate_exercise_session(ts, p$exercise_amp)

  # Clamp to plausible range
  hr <- pmin(200, pmax(35, hr))

  # Insert Fitbit-like missingness
  hr <- apply_missingness(
    hr,
    mp$tiny_drop_prob,
    mp$dropout_prob,
    mp$dropout_min,
    mp$dropout_max
  )

  tibble(
    id = id,
    hr_timestamp = ts,
    heart_rate = round(hr)
  )
}

# ─────────────────────────────────────────────
# RUN FOR P01 & P02
# ─────────────────────────────────────────────
raw_hr <- start_times %>%
  pmap_dfr(~ simulate_hr(..1, ..2)) %>%
  arrange(id, hr_timestamp)

# ─────────────────────────────────────────────
# SAVE
# ─────────────────────────────────────────────
if (requireNamespace("usethis", quietly = TRUE)) {
  usethis::use_data(raw_hr, overwrite = TRUE, compress = "xz")
} else {
  dir.create("data", showWarnings = FALSE)
  save(raw_hr, file = "data/raw_hr.rda", compress = "xz")
}

message("Saved: data/raw_hr.rda (Fitbit-style HR, 1-min cadence, exercise + sleep + missingness)")

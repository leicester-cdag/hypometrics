# data-raw/simulate_raw_step.R
# Simulate Fitbit-style step counts for 2 participants at 1-minute cadence.
# FEATURES:
# - P01 starts at: "2026-01-01 07:17:00"
# - P02 starts at: "2026-01-02 15:52:00"
# - Two weeks of data (14 days)
# - Day/night activity patterns
# - Exercise sessions
# - Fitbit-like missingness (random gaps + dropouts)
# - Steps always whole numbers
# SAVES: data/raw_step.rda

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(tibble)
  library(purrr)
  library(glue)
})

set.seed(20260320)

# ─────────────────────────────────────────────
# PARTICIPANT START TIMES (fixed)
# ─────────────────────────────────────────────
start_times <- tibble::tribble(
  ~id,   ~start_time,
  "P01", as.POSIXct("2026-01-01 07:17:00", tz = "UTC"),
  "P02", as.POSIXct("2026-01-02 15:52:00", tz = "UTC")
)

# 2 weeks of minute data
n_minutes <- 14 * 24 * 60  # = 20160 mins

# ─────────────────────────────────────────────
# STEP BEHAVIOUR PARAMETERS
# ─────────────────────────────────────────────

step_params <- tibble::tribble(
  ~id,   ~base_steps_day, ~noise_sd, ~exercise_amp,
  "P01",        20,          8,         150,
  "P02",        25,         10,         180
)

missing_params <- tibble::tribble(
  ~id,   ~tiny_drop_prob, ~dropout_prob, ~dropout_min, ~dropout_max,
  "P01",      0.002,         0.001,         10,           40,
  "P02",      0.003,         0.0012,        15,           60
)

# Sleep definition (very little steps)
is_sleep <- function(ts) {
  h <- hour(ts)
  h >= 23 | h < 6
}

# ─────────────────────────────────────────────
# Exercise session generator
# ─────────────────────────────────────────────

generate_exercise_steps <- function(ts, exercise_amp) {
  n <- length(ts)
  boost <- numeric(n)

  if (runif(1) > 0.5) return(boost)   # maybe no exercise in this simulation

  n_sessions <- sample(3:6, 1)

  for (i in seq_len(n_sessions)) {
    start_idx <- sample(1:(n-60), 1)
    duration  <- sample(20:50, 1)
    end_idx   <- min(n, start_idx + duration)

    ramp_up    <- seq(0, exercise_amp, length.out = 8)
    plateau    <- rep(exercise_amp, max(1, duration - 16))
    ramp_down  <- seq(exercise_amp, 0, length.out = 8)

    pattern <- c(ramp_up, plateau, ramp_down)
    pattern <- pattern[seq_len(end_idx - start_idx + 1)]

    boost[start_idx:end_idx] <- boost[start_idx:end_idx] + pattern
  }

  boost
}

# ─────────────────────────────────────────────
# Missingness generator
# ─────────────────────────────────────────────

apply_missingness <- function(counts, tiny_prob, drop_prob, drop_min, drop_max) {
  n <- length(counts)
  missing <- rep(FALSE, n)

  # Tiny 1-2 minute micro-gaps
  tiny_idx <- which(runif(n) < tiny_prob)
  for (i in tiny_idx) {
    len <- sample(1:2, 1)
    missing[i:min(n, i+len)] <- TRUE
  }

  # Larger dropouts
  dropout_idx <- which(runif(n) < drop_prob)
  for (i in dropout_idx) {
    len <- sample(drop_min:drop_max, 1)
    missing[i:min(n, i+len)] <- TRUE
  }

  counts[missing] <- NA_integer_
  counts
}

# ─────────────────────────────────────────────
# MAIN SIMULATOR
# ─────────────────────────────────────────────

simulate_steps_one <- function(id, start_time) {

  # Time sequence (1-min cadence)
  ts <- seq(from = start_time, by = "1 min", length.out = n_minutes)

  # Extract parameters
  p <- step_params %>% filter(id == !!id)
  if (nrow(p) == 0) stop(glue("Missing step_params for id={id}"))

  mp <- missing_params %>% filter(id == !!id)
  if (nrow(mp) == 0) stop(glue("Missing missing_params for id={id}"))

  # Base steps
  steps <- rep(p$base_steps_day, length(ts))

  # NIGHT-TIME: low activity
  sleep_idx <- is_sleep(ts)
  n_sleep <- sum(sleep_idx)
  if (n_sleep > 0) {
    steps[sleep_idx] <- round(rnorm(n_sleep, mean = 0.3, sd = 0.25))
  }

  # DAYTIME variability
  day_idx <- !sleep_idx
  n_day <- sum(day_idx)
  if (n_day > 0) {
    steps[day_idx] <- steps[day_idx] + rnorm(
      n_day,
      mean = p$base_steps_day,
      sd   = p$noise_sd
    )
  }

  # Exercise bursts
  steps <- steps + generate_exercise_steps(ts, p$exercise_amp)

  # Clean up
  steps <- pmax(0, round(steps))

  # Missingness
  steps <- apply_missingness(
    steps,
    mp$tiny_drop_prob,
    mp$dropout_prob,
    mp$dropout_min,
    mp$dropout_max
  )

  tibble(
    id = id,
    step_timestamp = ts,
    count = steps
  )
}

# ─────────────────────────────────────────────
# RUN FOR BOTH PARTICIPANTS
# ─────────────────────────────────────────────

raw_step <- start_times %>%
  pmap_dfr(~ simulate_steps_one(..1, ..2)) %>%
  arrange(id, step_timestamp)

# ─────────────────────────────────────────────
# SAVE
# ─────────────────────────────────────────────

if (requireNamespace("usethis", quietly = TRUE)) {
  usethis::use_data(raw_step, overwrite = TRUE, compress = "xz")
} else {
  dir.create("data", showWarnings = FALSE)
  save(raw_step, file = "data/raw_step.rda", compress = "xz")
}

message("Saved: data/raw_step.rda (Fitbit-style steps, exercise, sleep, missingness, 1-min cadence)")

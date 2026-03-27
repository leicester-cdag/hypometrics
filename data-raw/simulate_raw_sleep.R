# data-raw/simulate_raw_sleep.R
# Simulated raw_sleep dataset with consistent duration & sleep metrics

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(lubridate)
  library(purrr)
  library(vctrs)
})

set.seed(20260320)

# PARAMETERS --------------------------------------------------------------

n_nights <- 13

start_dates <- tibble::tribble(
  ~id,   ~first_sleep_date,
  "P01", as.Date("2026-01-01"),
  "P02", as.Date("2026-01-02")
)

# HELPERS --------------------------------------------------------------

rand_log_id <- function(n) {
  sample(1e9:9.99e9, n, replace = FALSE)
}

# Random bedtime: 22:00–02:00 with proper date roll-over
random_sleep_start <- function(date_of_sleep) {

  hr <- sample(c(22, 23, 0, 1, 2), 1)
  mn <- sample(0:59, 1)
  sc <- sample(0:59, 1)

  # hr >= 22 means previous calendar night
  date_use <- if (hr >= 22) date_of_sleep - days(1) else date_of_sleep

  as.POSIXct(
    sprintf("%s %02d:%02d:%02d", date_use, hr, mn, sc),
    tz = "UTC"
  )
}

# Random wake-time: 06:00–10:00
random_sleep_end <- function(date_of_sleep) {

  hr <- sample(6:9, 1)
  mn <- sample(0:59, 1)
  sc <- sample(0:59, 1)

  as.POSIXct(
    sprintf("%s %02d:%02d:%02d", date_of_sleep, hr, mn, sc),
    tz = "UTC"
  )
}

# MAIN SIMULATOR --------------------------------------------------------------

simulate_sleep_one <- function(id, first_date) {

  # 1) Generate 13 consecutive nights
  nights <- tibble(
    id = id,
    dateOfSleep = first_date + days(0:(n_nights - 1))
  )

  # 2) Randomly remove 2–3 nights
  n_missing <- sample(2:3, 1)
  drop_rows <- sample(seq_len(n_nights), n_missing)
  nights <- nights[-drop_rows, ]

  # 3) Unique 10-digit nightly ID
  nights$logId <- rand_log_id(nrow(nights))

  # 4) POSIXct-safe startTime / endTime assembly
  nights$startTime <- list_c(map(
    nights$dateOfSleep,
    random_sleep_start
  ))

  nights$endTime <- list_c(map(
    nights$dateOfSleep,
    random_sleep_end
  ))

  # 5) Sleep metrics — EXACT consistency guaranteed
  nights <- nights %>%
    mutate(
      # exact duration in ms (integer)
      duration_raw = as.numeric(difftime(endTime, startTime, units = "secs")),
      duration = round(duration_raw * 1000),

      # derive time in bed (minutes) EXACTLY
      timeInBed = floor(duration / (60 * 1000)),

      # enforce duration consistency
      duration = timeInBed * 60 * 1000,

      # minutesAsleep: 70–90% of total
      minutesAsleep = ifelse(
        timeInBed > 0,
        floor(timeInBed * runif(n(), 0.7, 0.9)),
        0
      ),

      # minutesAwake = remainder
      minutesAwake = timeInBed - minutesAsleep
    ) %>%
    select(-duration_raw)

  # 6) Ensure dateOfSleep is character "YYYY-MM-DD"
  nights$dateOfSleep <- format(nights$dateOfSleep, "%Y-%m-%d")

  nights
}

# RUN FOR BOTH PARTICIPANTS ----------------------------------------------------

raw_sleep <- start_dates %>%
  pmap_dfr(~ simulate_sleep_one(..1, ..2)) %>%
  arrange(id, dateOfSleep)

# SAVE -------------------------------------------------------------------------

if (requireNamespace("usethis", quietly = TRUE)) {
  usethis::use_data(raw_sleep, overwrite = TRUE, compress = "xz")
} else {
  dir.create("data", showWarnings = FALSE)
  save(raw_sleep, file = "data/raw_sleep.rda", compress = "xz")
}

message("Saved: data/raw_sleep.rda (duration-ms & sleep metrics consistent).")

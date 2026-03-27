# data-raw/simulate_raw_eq5d5l.R
# Simulate EQ-5D-5L dataset for P01 and P02, across 3 stages:
# Registration, Day-7, Day-14
# Columns:
# userid, stage, localTimestamp (ISO-8601 w/ milliseconds)
# SC, AD, UA, MB, PD (each categorical with levels 1–5 encoded SC1–SC5, etc.)
#
# Saves: data/raw_eq5d5l.rda

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(tibble)
  library(purrr)
})

set.seed(20260320)

# ─────────────────────────────────────────────
# Participant baseline timestamps
# ─────────────────────────────────────────────
start_times <- tibble::tribble(
  ~userid, ~start_ts,
  "P01", as.POSIXct("2026-01-01 07:17:00", tz = "UTC"),
  "P02", as.POSIXct("2026-01-02 15:52:00", tz = "UTC")
)

# 3 assessment stages
stages <- c("Registration", "Day-7", "Day-14")

# ─────────────────────────────────────────────
# Helper: ISO8601 with milliseconds
# ─────────────────────────────────────────────
format_iso_millis <- function(x) {
  paste0(format(x, "%Y-%m-%dT%H:%M:%OS3"), "+00:00")
}

# Possible EQ-5D-5L categories (1–5)
SC_levels <- paste0("SC", 1:5)
AD_levels <- paste0("AD", 1:5)
UA_levels <- paste0("UA", 1:5)
MB_levels <- paste0("MB", 1:5)
PD_levels <- paste0("PD", 1:5)

# ─────────────────────────────────────────────
# Generate EQ-5D-5L rows for one participant
# ─────────────────────────────────────────────
simulate_eq5d5l_one <- function(userid, baseline_ts) {

  df <- tibble(
    stage = stages,
    localTimestamp = c(
      baseline_ts,
      baseline_ts + days(7),
      baseline_ts + days(14)
    )
  ) %>%
    mutate(
      userid = userid,
      localTimestamp = map_chr(localTimestamp, format_iso_millis),
      SC = sample(SC_levels, nrow(.), replace = TRUE),
      AD = sample(AD_levels, nrow(.), replace = TRUE),
      UA = sample(UA_levels, nrow(.), replace = TRUE),
      MB = sample(MB_levels, nrow(.), replace = TRUE),
      PD = sample(PD_levels, nrow(.), replace = TRUE)
    ) %>%
    select(userid, stage, localTimestamp, SC, AD, UA, MB, PD)

  df
}

# ─────────────────────────────────────────────
# Generate dataset for all participants
# ─────────────────────────────────────────────
raw_eq5d5l <- start_times %>%
  mutate(data = pmap(list(userid, start_ts), simulate_eq5d5l_one)) %>%
  select(data) %>%
  tidyr::unnest(data) %>%
  arrange(userid, stage)

# ─────────────────────────────────────────────
# Save into package
# ─────────────────────────────────────────────
if (requireNamespace("usethis", quietly = TRUE)) {
  usethis::use_data(raw_eq5d5l, overwrite = TRUE, compress = "xz")
} else {
  dir.create("data", showWarnings = FALSE)
  save(raw_eq5d5l, file = "data/raw_eq5d5l.rda", compress = "xz")
}

message("Saved: data/raw_eq5d5l.rda (Simulated EQ-5D-5L dataset for P01 and P02)")

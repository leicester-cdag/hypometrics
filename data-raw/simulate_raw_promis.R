# data-raw/simulate_raw_promis.R
# Create simulated PROMIS sleep dataset for two participants:
# Columns:
# userid (P01, P02)
# stage  ("Registration", "Day-7", "Day-14")
# localTimestamp (ISO-8601 with milliseconds and timezone)
# 8 PROMIS sleep variables (1–5 Likert): Sleep115, Sleep116, Sleep90,
# Sleep110, Sleep44, Sleep87, Sleep108, Sleep109
#
# Saves as data/raw_promis.rda

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(lubridate)
  library(purrr)
  library(stringr)
})

set.seed(20260320)

# ─────────────────────────────────────────────
# Participant baseline timestamps
# ─────────────────────────────────────────────
start_times <- tibble::tribble(
  ~userid, ~start_timestamp,
  "P01", as.POSIXct("2026-01-01 07:17:00", tz = "UTC"),
  "P02", as.POSIXct("2026-01-02 15:52:00", tz = "UTC")
)

# Stages
stages <- c("Registration", "Day-7", "Day-14")

# ─────────────────────────────────────────────
# Helper to format ISO8601 with milliseconds
# Example: 2020-11-18T22:52:22.565+00:00
# ─────────────────────────────────────────────
format_iso_millis <- function(x) {
  # Format: Y-m-dTH:M:S.mmm+00:00
  ms <- format(x, "%OS3")  # seconds with milliseconds
  paste0(format(x, "%Y-%m-%dT%H:%M:"), ms, "+00:00")
}

# PROMIS items (Likert 1–5)
promis_items <- c("Sleep115","Sleep116","Sleep90","Sleep110",
                  "Sleep44","Sleep87","Sleep108","Sleep109")

simulate_promis_one <- function(userid, baseline_ts) {

  # compute timestamps for each stage
  ts_list <- tibble(
    stage = stages,
    localTimestamp = c(
      baseline_ts,
      baseline_ts + days(7),
      baseline_ts + days(14)
    )
  )

  # convert to ISO8601 with ms
  ts_list <- ts_list %>%
    mutate(
      localTimestamp = map_chr(localTimestamp, format_iso_millis),
      userid = userid
    )

  # add PROMIS items (random Likert 1–5)
  for (item in promis_items) {
    ts_list[[item]] <- sample(1:5, size = nrow(ts_list), replace = TRUE)
  }

  ts_list %>% select(userid, stage, localTimestamp, all_of(promis_items))
}

# ─────────────────────────────────────────────
# Generate dataset for all participants
# ─────────────────────────────────────────────

raw_promis <- start_times %>%
  mutate(data = pmap(list(userid, start_timestamp), simulate_promis_one)) %>%
  select(data) %>%
  unnest(data) %>%
  arrange(userid, stage)

# ─────────────────────────────────────────────
# Save dataset
# ─────────────────────────────────────────────

if (requireNamespace("usethis", quietly = TRUE)) {
  usethis::use_data(raw_promis, overwrite = TRUE, compress = "xz")
} else {
  dir.create("data", showWarnings = FALSE)
  save(raw_promis, file = "data/raw_promis.rda", compress = "xz")
}

message("Saved: data/raw_promis.rda (PROMIS sleep dataset for P01 and P02)")

# data-raw/simulate_raw_wpai.R
# Simulated WPAI dataset for "P01" and "P02"
# 9 columns:
# userid, stage, localTimestamp (ISO-8601 w/ ms), plus 6 WPAI responses
#
# Stages:
#   Registration: baseline timestamp
#   Day-7: baseline + 7 days
#   Day-14: baseline + 14 days
#
# Outputs: data/raw_wpai.rda

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(tibble)
  library(purrr)
  library(stringr)
})

set.seed(20260320)

# ─────────────────────────────────────────────
# Participant start times (from user)
# ─────────────────────────────────────────────
start_times <- tibble::tribble(
  ~userid, ~start_ts,
  "P01", as.POSIXct("2026-01-01 07:17:00", tz = "UTC"),
  "P02", as.POSIXct("2026-01-02 15:52:00", tz = "UTC")
)

stages <- c("Registration", "Day-7", "Day-14")

# ─────────────────────────────────────────────
# Helper: ISO8601 with milliseconds and timezone
# Example: 2020-11-18T22:52:22.565+00:00
# ─────────────────────────────────────────────
format_iso_millis <- function(x) {
  sec_ms <- format(x, "%OS3")  # seconds + milliseconds
  paste0(format(x, "%Y-%m-%dT%H:%M:"), sec_ms, "+00:00")
}

# ─────────────────────────────────────────────
# Main simulator for one participant
# ─────────────────────────────────────────────
simulate_wpai_one <- function(userid, baseline) {

  # THREE timestamps: Registration, Day-7, Day-14
  ts <- tibble(
    stage = stages,
    timestamp = c(
      baseline,
      baseline + days(7),
      baseline + days(14)
    )
  ) %>%
    mutate(
      localTimestamp = map_chr(timestamp, format_iso_millis),
      userid = userid
    ) %>%
    select(userid, stage, localTimestamp)

  # WPAI items:
  # areyoucurrentlyemployedworkingforpay: character yes/no
  ts$areyoucurrentlyemployedworkingforpay <-
    sample(c("yes", "no"), size = nrow(ts), replace = TRUE, prob = c(0.7, 0.3))

  # (numeric ranges as provided)
  ts$duringthepastsevendayshowmanyhoursdidyoumissfromworkbecauseofproblemsassociatedwithyourhypoglycaemia <-
    sample(0:3, size = nrow(ts), replace = TRUE)

  ts$duringthepastsevendayshowmanyhoursdidyoumissfromworkbecauseofanyotherreasonsuchasvacationholidaystimeofftoparticipateinthisstudy <-
    sample(0:50, size = nrow(ts), replace = TRUE)

  ts$duringthepastsevendayshowmanyhoursdidyouactuallywork <-
    sample(0:70, size = nrow(ts), replace = TRUE)

  ts$duringthepastsevendayshowmuchdidhypoglycaemiaaffectyourproductivitywhileyouwereworking <-
    sample(1:3, size = nrow(ts), replace = TRUE)

  ts$duringthepastsevendayshowmuchdidhypoglycaemiaaffectyourabilitytodoyourregulardailyactivitiesotherthanworkatajob <-
    sample(0:3, size = nrow(ts), replace = TRUE)

  ts
}

# ─────────────────────────────────────────────
# Generate data for all participants
# ─────────────────────────────────────────────

raw_wpai <- start_times %>%
  mutate(data = pmap(list(userid, start_ts), simulate_wpai_one)) %>%
  select(data) %>%
  tidyr::unnest(data) %>%
  arrange(userid, stage)

# ─────────────────────────────────────────────
# Save dataset in package
# ─────────────────────────────────────────────

if (requireNamespace("usethis", quietly = TRUE)) {
  usethis::use_data(raw_wpai, overwrite = TRUE, compress = "xz")
} else {
  dir.create("data", showWarnings = FALSE)
  save(raw_wpai, file = "data/raw_wpai.rda", compress = "xz")
}

message("Saved: data/raw_wpai.rda (Simulated WPAI dataset for P01 and P02)")

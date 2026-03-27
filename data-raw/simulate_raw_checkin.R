# data-raw/simulate_raw_checkin.R
# Final: Daily check-in dataset with 1st–5th hypo columns,
# hypo times aligned to raw_cgm + raw_motif_segment,
# morning timestamps, overnight hypo times, all character fields.

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(tibble)
  library(purrr)
  library(tidyr)
})

set.seed(20260320)

#──────────────────────────────────────────────────────────────
# LOAD REQUIRED DATA
#──────────────────────────────────────────────────────────────
data("raw_cgm", package = "hypometrics")
data("raw_motif_segment", package = "hypometrics")

# Collect CGM hypo timestamps
hypo_ts <- raw_cgm %>% filter(glucose < 3.9)

p01_hypo <- hypo_ts %>% filter(id == "P01") %>% pull(cgm_timestamp)
p02_hypo <- hypo_ts %>% filter(id == "P02") %>% pull(cgm_timestamp)

# Collect motif timestamps
p01_motif <- raw_motif_segment %>% filter(UserId=="P01") %>% pull(uMotifTime) %>% ymd_hms() %>% unique()
p02_motif <- raw_motif_segment %>% filter(UserId=="P02") %>% pull(uMotifTime) %>% ymd_hms() %>% unique()

# Required: P01: 1 CGM hypo + 2 motif; P02: 2 CGM hypo + 4 motif
p01_targets <- c(p01_hypo[1], p01_motif[1:2])
p02_targets <- c(p02_hypo[1:2], p02_motif[1:4])

#──────────────────────────────────────────────────────────────
# Helper — convert timestamp to HH:MM in overnight window (22:00–06:00)
#──────────────────────────────────────────────────────────────
overnight_time <- function(x) {
  x2 <- x + minutes(sample(-20:20, 1))
  hh <- hour(x2)
  mm <- minute(x2)
  if (!(hh >= 22 | hh < 6)) {
    hh <- sample(c(22:23, 0:5), 1)
    mm <- sample(0:59, 1)
  }
  sprintf("%02d:%02d", hh, mm)
}

p01_times_hhmm <- map_chr(p01_targets, overnight_time)
p02_times_hhmm <- map_chr(p02_targets, overnight_time)

#──────────────────────────────────────────────────────────────
# DAILY TIMELINES
#──────────────────────────────────────────────────────────────
start_dates <- tibble::tribble(
  ~userid, ~start_date,
  "P01", as.Date("2026-01-01"),
  "P02", as.Date("2026-01-02")
)

make_daily_rows <- function(userid, startdate) {
  tibble(
    userid = userid,
    day_idx = 1:14,
    stage = if_else(day_idx == 1, "Registration", paste0("Day-", day_idx)),
    date = startdate + days(day_idx - 1),
    localTimestamp = paste0(
      as.character(date), " ",
      sprintf("%02d:%02d:%02d",
              sample(7:11, 1),
              sample(0:59, 1),
              sample(0:59, 1))
    )
  )
}

p01_df <- make_daily_rows("P01", as.Date("2026-01-01"))
p02_df <- make_daily_rows("P02", as.Date("2026-01-02"))

#──────────────────────────────────────────────────────────────
# Insert hypo-linked time values
#──────────────────────────────────────────────────────────────
# Create empty columns for 1st–5th hypo time fields
hypo_time_cols <- c(
  "AM_forfirsthypoAtwhattimedidthishappen",
  "AM_forsecondhypoAtwhattimedidthishappen",
  "AM_forthirdhypoAtwhattimedidthishappen",
  "AM_forfourthhypoAtwhattimedidthishappen",
  "AM_forfifthhypoAtwhattimedidthishappen"
)

for (col in hypo_time_cols) {
  p01_df[[col]] <- ""
  p02_df[[col]] <- ""
}

# Assign P01's 3 hypo times to random days
p01_rows <- sample(1:nrow(p01_df), 3)
p01_df$AM_forfirsthypoAtwhattimedidthishappen[p01_rows] <- p01_times_hhmm

# Assign P02's 6 hypo times
p02_rows <- sample(1:nrow(p02_df), 6)
p02_df$AM_forfirsthypoAtwhattimedidthishappen[p02_rows] <- p02_times_hhmm

#──────────────────────────────────────────────────────────────
# DETECTION + WHAT HAPPENED options
#──────────────────────────────────────────────────────────────
detect_opts <- c(
  "I had symptoms",
  "I slept through it - and realised when I woke up",
  "I checked my sensor"
)

action_opts <- c(
  "I ate / drank to treat a hypo",
  "I ate / drank to prevent a hypo",
  "I took no action"
)

#──────────────────────────────────────────────────────────────
# Helper to add detection/other/whathappened columns
#──────────────────────────────────────────────────────────────
add_hypo_columns <- function(df, prefix_num) {

  time_col     <- paste0("AM_for", prefix_num, "hypoAtwhattimedidthishappen")
  detect_col   <- paste0("AM_for", prefix_num, "hypoHowdidyoudetectyourhypoorahypothatwasabouttohappen")
  detect_other <- paste0("AM_for", prefix_num, "hypoHowdidyoudetectyourhypoorahypothatwasabouttohappenotherpleasespecify")
  what_col     <- paste0("AM_for", prefix_num, "hypoWhathappened")

  # Create if missing
  for (col in c(detect_col, detect_other, what_col)) {
    if (!(col %in% names(df))) df[[col]] <- ""
  }

  df[[detect_col]] <- ifelse(df[[time_col]] != "",
                             sample(detect_opts, nrow(df), TRUE),
                             "")

  df[[detect_other]] <- ifelse(df[[time_col]] != "",
                               "",  # stays empty unless you add "other" as an option
                               "")

  df[[what_col]] <- ifelse(df[[time_col]] != "",
                           sample(action_opts, nrow(df), TRUE),
                           "")

  df
}

#──────────────────────────────────────────────────────────────
# Apply to all hypo levels
#──────────────────────────────────────────────────────────────
apply_hypo_logic <- function(df) {

  # First hypo (already created by script)
  df <- df %>%
    mutate(
      AM_forfirsthypoHowdidyoudetectyourhypoorahypothatwasabouttohappen =
        ifelse(AM_forfirsthypoAtwhattimedidthishappen != "",
               sample(detect_opts, n(), TRUE),
               ""),

      AM_forfirsthypoHowdidyoudetectyourhypoorahypothatwasabouttohappenotherpleasespecify =
        ifelse(AM_forfirsthypoAtwhattimedidthishappen != "",
               "",
               ""),

      AM_forfirsthypoWhathappened =
        ifelse(AM_forfirsthypoAtwhattimedidthishappen != "",
               sample(action_opts, n(), TRUE),
               "")
    )

  df <- add_hypo_columns(df, "second")
  df <- add_hypo_columns(df, "third")
  df <- add_hypo_columns(df, "fourth")
  df <- add_hypo_columns(df, "fifth")

  df
}

p01_df <- apply_hypo_logic(p01_df)
p02_df <- apply_hypo_logic(p02_df)

#──────────────────────────────────────────────────────────────
# FINAL DATASET
#──────────────────────────────────────────────────────────────
raw_checkin <- bind_rows(p01_df, p02_df) %>%
  arrange(userid, day_idx) %>%
  mutate(across(everything(), as.character))

#──────────────────────────────────────────────────────────────
# SAVE
#──────────────────────────────────────────────────────────────
if (requireNamespace("usethis", quietly=TRUE)) {
  usethis::use_data(raw_checkin, overwrite=TRUE, compress="xz")
} else {
  dir.create("data", showWarnings = FALSE)
  save(raw_checkin, file="data/raw_checkin.rda", compress="xz")
}

message("Saved: data/raw_checkin.rda (final daily check-ins with 1st–5th hypo fields)")

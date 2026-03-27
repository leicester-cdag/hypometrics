# data-raw/simulate_raw_motif_segment.R
# Simulated uMotif-like Segment dataset with hypo-aligned timestamps.
# For P01: 1 timestamp falls inside a hypoglycaemia window (SegmentID 1 → Value = 1)
# For P02: 3 timestamps fall inside hypo windows       (SegmentID 11 → Value = 1)

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(tibble)
  library(purrr)
  library(tidyr)
})

set.seed(20260320)

# ─────────────────────────────────────────────
# Load raw_cgm (needed to identify hypo timestamps)
# ─────────────────────────────────────────────
data("raw_cgm", package = "hypometrics")

# Identify hypoglycaemia timestamps (< 3.9)
hypo_ts <- raw_cgm %>%
  filter(glucose < 3.9) %>%
  select(id, cgm_timestamp)

# Extract candidate timestamps by participant
p01_hypo <- hypo_ts %>% filter(id == "P01") %>% pull(cgm_timestamp)
p02_hypo <- hypo_ts %>% filter(id == "P02") %>% pull(cgm_timestamp)

if (length(p01_hypo) < 1) stop("P01 has no hypo timestamps in raw_cgm.")
if (length(p02_hypo) < 3) stop("P02 has fewer than 3 hypo timestamps in raw_cgm.")

p01_hypo_pick <- p01_hypo[1]
p02_hypo_pick <- p02_hypo[1:3]

# ─────────────────────────────────────────────
# Participant timelines
# ─────────────────────────────────────────────
start_times <- tibble::tribble(
  ~UserId, ~start_ts,
  "P01", as.POSIXct("2026-01-01 07:17:00", tz="UTC"),
  "P02", as.POSIXct("2026-01-02 15:52:00", tz="UTC")
)

end_times <- start_times %>% mutate(end_ts = start_ts + days(14))

# Base number of timestamps
n_t_p01 <- 3
n_t_p02 <- 5

# ─────────────────────────────────────────────
# Create equal-spaced timestamps for each participant
# ─────────────────────────────────────────────
make_even_timestamps <- function(start, end, n) {
  ts <- seq(from = start, to = end, length.out = n)
  format(ts, "%Y-%m-%d %H:%M:%S")
}

# ─────────────────────────────────────────────
# Build timestamps (then overwrite hypo-aligned ones)
# ─────────────────────────────────────────────
p01_ts <- make_even_timestamps(
  start_times$start_ts[start_times$UserId=="P01"],
  end_times$end_ts[end_times$UserId=="P01"],
  n_t_p01
)

p02_ts <- make_even_timestamps(
  start_times$start_ts[start_times$UserId=="P02"],
  end_times$end_ts[end_times$UserId=="P02"],
  n_t_p02
)

# Insert real hypo timestamps
p01_ts[1] <- format(p01_hypo_pick, "%Y-%m-%d %H:%M:%S")
p02_ts[1:3] <- format(p02_hypo_pick, "%Y-%m-%d %H:%M:%S")

# ─────────────────────────────────────────────
# Segment IDs:
#  P01 → 1–10   (each timestamp)
#  P02 → 11–20  (each timestamp)
# ─────────────────────────────────────────────
simulate_one <- function(UserId, ts_vec) {

  seg_ids <- if (UserId=="P01") 1:10 else 11:20

  expand_grid(
    UserId = UserId,
    uMotifTime = ts_vec,
    SegmentId = seg_ids
  ) %>%
    mutate(
      Value = sample(1:5, n(), replace = TRUE)
    )
}

p01_df <- simulate_one("P01", p01_ts)
p02_df <- simulate_one("P02", p02_ts)

# ─────────────────────────────────────────────
# Force Value = 1 for hypo-aligned segments
# ─────────────────────────────────────────────
p01_df <- p01_df %>%
  mutate(Value = ifelse(
    uMotifTime == format(p01_hypo_pick, "%Y-%m-%d %H:%M:%S") &
      SegmentId == 1,
    1,
    Value
  ))

p02_df <- p02_df %>%
  mutate(Value = ifelse(
    uMotifTime %in% format(p02_hypo_pick, "%Y-%m-%d %H:%M:%S") &
      SegmentId == 11,
    1,
    Value
  ))

# ─────────────────────────────────────────────
# Final dataset
# ─────────────────────────────────────────────
raw_motif_segment <- bind_rows(p01_df, p02_df) %>%
  arrange(UserId, uMotifTime, SegmentId)

# ─────────────────────────────────────────────
# Save
# ─────────────────────────────────────────────
if (requireNamespace("usethis", quietly = TRUE)) {
  usethis::use_data(raw_motif_segment, overwrite = TRUE, compress = "xz")
} else {
  dir.create("data", showWarnings=FALSE)
  save(raw_motif_segment, file="data/raw_motif_segment.rda", compress="xz")
}

message("Saved: data/raw_motif_segment.rda with hypo-aligned motif segments.")

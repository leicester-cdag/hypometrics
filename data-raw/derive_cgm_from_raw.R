# data-raw/derive_cgm_from_raw.R
# Create a 5-minute CGM dataset from raw_cgm:
# - raw_cgm has implicit gaps (no NA rows)
# - interpolates gaps < 30 min and >= than 10 min
# - leaves gaps >= 30 min as NA
# - using the hypometrics::cgmInterpolate function
# - saves as data/cgm.rda for use throughout the package

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(tidyr)
  library(purrr)
})

# Load raw_cgm
data("raw_cgm", package = "hypometrics")

#Use the hypometrics::cgmInterpolate function to linearly interpolate data

cgm <-
  hypometrics::cgmInterpolate(raw_cgm,
                              Interpolate = TRUE,
                              MinGap = 600,
                              Granularity = 300,
                              MaxGap = 1800)

# ---- Save ----
if (requireNamespace("usethis", quietly = TRUE)) {
  usethis::use_data(cgm, overwrite = TRUE, compress = "xz")
} else {
  dir.create("data", showWarnings = FALSE)
  save(cgm, file = "data/cgm.rda", compress = "xz")
}

message("Saved: data/cgm.rda (5-minute CGM with interpolation < 30 minutes).")

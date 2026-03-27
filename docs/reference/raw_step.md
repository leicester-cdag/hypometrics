# Simulated Minute‑Level Step Count Data

\`raw_step\` contains \*\*synthetic Fitbit‑style step count data\*\* for
two simulated participants (\`P01\` and \`P02\`). The dataset spans
\*\*two weeks\*\* at \*\*1‑minute resolution\*\*. It includes
\*\*exercise sessions\*\*, \*\*sleep‑related inactivity\*\*, and
\*\*Fitbit‑like missingness patterns\*\*, providing a realistic
structure for testing algorithms that process wearable activity data.

## Usage

``` r
raw_step
```

## Format

A tibble with three columns:

- id:

  Character participant ID (`"P01"`, `"P02"`).

- step_timestamp:

  POSIXct timestamp in UTC, at 1‑minute resolution.

- count:

  Whole‑number step count per minute.

## Details

**Participants**

- `"P01"` — starts at `"2026‑01‑01 07:17:00"` (UTC)

- `"P02"` — starts at `"2026‑01‑02 15:52:00"` (UTC)

Each participant has exactly \*\*14 days\*\* of simulated minute‑level
data.

**Activity patterns**

- **Daytime activity:** mild random movement with realistic variability.

- **Sleep inactivity:** step counts drop to near‑zero between
  23:00–06:00.

- **Exercise sessions:** intermittent bouts with ramp‑up, sustained high
  counts, and gradual cooldown.

- **Noise:** stochastic variation representing device and behavioural
  variation.

**Missingness model (Fitbit‑like)**

Step counts include NA values introduced using:

- *tiny micro‑gaps* (1–2 minute sensor losses),

- *longer dropouts* (10–60 minutes).

These patterns mimic real‑world wrist‑worn accelerometer behaviour
during motion artifacts, device removal, or Bluetooth disconnection.

**Generation script**

This dataset is produced reproducibly by the script:

      data-raw/simulate_raw_step.R

## Examples

``` r
data(raw_step, package = "hypometrics")

# Structure
dplyr::glimpse(raw_step)
#> Rows: 40,320
#> Columns: 3
#> $ id             <chr> "P01", "P01", "P01", "P01", "P01", "P01", "P01", "P01",…
#> $ step_timestamp <dttm> 2026-01-01 07:17:00, 2026-01-01 07:18:00, 2026-01-01 0…
#> $ count          <dbl> 35, 38, 52, 44, 17, 46, 44, 42, 35, 37, 29, 46, 35, 34,…

# First few rows for P01
dplyr::filter(raw_step, id == "P01") %>% head()
#> # A tibble: 6 × 3
#>   id    step_timestamp      count
#>   <chr> <dttm>              <dbl>
#> 1 P01   2026-01-01 07:17:00    35
#> 2 P01   2026-01-01 07:18:00    38
#> 3 P01   2026-01-01 07:19:00    52
#> 4 P01   2026-01-01 07:20:00    44
#> 5 P01   2026-01-01 07:21:00    17
#> 6 P01   2026-01-01 07:22:00    46

# Missingness percentage per participant
raw_step %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(
    pct_missing = mean(is.na(count)) * 100
  )
#> # A tibble: 2 × 2
#>   id    pct_missing
#>   <chr>       <dbl>
#> 1 P01          3.72
#> 2 P02          4.49
```

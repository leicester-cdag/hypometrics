# Simulated Fitbit Sleep Dataset

\`raw_sleep\` provides a fully simulated set of sleep data for two
synthetic participants (\`P01\` and \`P02\`). Each record represents a
single night of sleep, with realistic bedtimes and wake times, unique
nightly identifiers, and occasional missing nights to mirror imperfect
real‑world device collection.

## Usage

``` r
raw_sleep
```

## Format

A tibble with the following columns:

- id:

  Character participant ID: `"P01"` or `"P02"`.

- logId:

  Unique 10‑digit numeric identifier for the sleep log.

- dateOfSleep:

  Character date (YYYY‑MM‑DD) representing the date of sleep.

- startTime:

  POSIXct datetime marking the start of the sleep period.

- endTime:

  POSIXct datetime marking the end of the sleep period.

## Details

The dataset is intended solely for demonstration, examples, and testing
within the \`hypometrics\` package.

**Participants and date coverage**

- `"P01"` — first night: `"2026‑01‑01"`

- `"P02"` — first night: `"2026‑01‑02"`

Each participant is simulated for \*\*13 consecutive nights\*\*, after
which \*\*2–3 randomly selected nights are removed completely\*\* to
mimic missing Fitbit‑style nights of sleep.

**Generation script**

The dataset is generated reproducibly from:

    data-raw/simulate_raw_sleep.R

## Examples

``` r
data(raw_sleep, package = "hypometrics")

# View structure
dplyr::glimpse(raw_sleep)
#> Rows: 20
#> Columns: 9
#> $ id            <chr> "P01", "P01", "P01", "P01", "P01", "P01", "P01", "P01", …
#> $ dateOfSleep   <chr> "2026-01-01", "2026-01-02", "2026-01-03", "2026-01-04", …
#> $ logId         <dbl> 1233452892, 5249981493, 5332094017, 5467075058, 20143725…
#> $ startTime     <dttm> 2025-12-31 22:48:09, 2026-01-01 22:56:55, 2026-01-03 00…
#> $ endTime       <dttm> 2026-01-01 07:39:11, 2026-01-02 09:12:50, 2026-01-03 06…
#> $ duration      <dbl> 31860000, 36900000, 18180000, 15900000, 19320000, 246000…
#> $ timeInBed     <dbl> 531, 615, 303, 265, 322, 410, 440, 357, 486, 474, 536, 3…
#> $ minutesAsleep <dbl> 377, 523, 244, 188, 274, 318, 388, 277, 389, 376, 415, 2…
#> $ minutesAwake  <dbl> 154, 92, 59, 77, 48, 92, 52, 80, 97, 98, 121, 64, 55, 11…

# Sleep duration for the first few nights
raw_sleep %>%
  dplyr::mutate(duration_hours =
      as.numeric(difftime(endTime, startTime, units = "hours"))) %>%
  head()
#> # A tibble: 6 × 10
#>   id    dateOfSleep      logId startTime           endTime             duration
#>   <chr> <chr>            <dbl> <dttm>              <dttm>                 <dbl>
#> 1 P01   2026-01-01  1233452892 2025-12-31 22:48:09 2026-01-01 07:39:11 31860000
#> 2 P01   2026-01-02  5249981493 2026-01-01 22:56:55 2026-01-02 09:12:50 36900000
#> 3 P01   2026-01-03  5332094017 2026-01-03 00:58:40 2026-01-03 06:02:19 18180000
#> 4 P01   2026-01-04  5467075058 2026-01-04 02:27:50 2026-01-04 06:53:22 15900000
#> 5 P01   2026-01-05  2014372548 2026-01-05 02:47:08 2026-01-05 08:09:16 19320000
#> 6 P01   2026-01-09  1290069097 2026-01-08 23:21:35 2026-01-09 06:12:06 24600000
#> # ℹ 4 more variables: timeInBed <dbl>, minutesAsleep <dbl>, minutesAwake <dbl>,
#> #   duration_hours <dbl>
```

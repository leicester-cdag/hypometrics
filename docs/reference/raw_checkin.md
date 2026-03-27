# Simulated Daily Check‑In Dataset

\`raw_checkin\` contains fully simulated daily “morning check‑in”
questionnaire data for two synthetic participants (\`P01\`, \`P02\`)
across a 14‑day follow‑up. The dataset includes registration‑day entries
followed by daily morning check‑ins through `"Day‑14"`.

## Usage

``` r
raw_checkin
```

## Format

A tibble with the following character columns:

- userid:

  Participant ID (`"P01"`, `"P02"`).

- stage:

  `"Registration"` or `"Day‑2"`–`"Day‑14"`.

- localTimestamp:

  Timestamp of check-in questionnaire completion.

- AM_forfirsthypoAtwhattimedidthishappen.:

- AM_forfirsthypoHowdidyoudetectyourhypoorahypothatwasabouttohappen.:

- AM_forfirsthypoWhathappened.:

## Details

This dataset is \*\*entirely synthetic\*\* and is intended for testing
and demonstrating multi‑modal data integration.

**Generation script**

This dataset is produced reproducibly using:

    data-raw/simulate_raw_checkin.R

## Examples

``` r
data(raw_checkin, package = "hypometrics")

# Structure
dplyr::glimpse(raw_checkin)
#> Rows: 28
#> Columns: 25
#> $ userid                                                                               <chr> …
#> $ day_idx                                                                              <chr> …
#> $ stage                                                                                <chr> …
#> $ date                                                                                 <chr> …
#> $ localTimestamp                                                                       <chr> …
#> $ AM_forfirsthypoAtwhattimedidthishappen                                               <chr> …
#> $ AM_forsecondhypoAtwhattimedidthishappen                                              <chr> …
#> $ AM_forthirdhypoAtwhattimedidthishappen                                               <chr> …
#> $ AM_forfourthhypoAtwhattimedidthishappen                                              <chr> …
#> $ AM_forfifthhypoAtwhattimedidthishappen                                               <chr> …
#> $ AM_forfirsthypoHowdidyoudetectyourhypoorahypothatwasabouttohappen                    <chr> …
#> $ AM_forfirsthypoHowdidyoudetectyourhypoorahypothatwasabouttohappenotherpleasespecify  <chr> …
#> $ AM_forfirsthypoWhathappened                                                          <chr> …
#> $ AM_forsecondhypoHowdidyoudetectyourhypoorahypothatwasabouttohappen                   <chr> …
#> $ AM_forsecondhypoHowdidyoudetectyourhypoorahypothatwasabouttohappenotherpleasespecify <chr> …
#> $ AM_forsecondhypoWhathappened                                                         <chr> …
#> $ AM_forthirdhypoHowdidyoudetectyourhypoorahypothatwasabouttohappen                    <chr> …
#> $ AM_forthirdhypoHowdidyoudetectyourhypoorahypothatwasabouttohappenotherpleasespecify  <chr> …
#> $ AM_forthirdhypoWhathappened                                                          <chr> …
#> $ AM_forfourthhypoHowdidyoudetectyourhypoorahypothatwasabouttohappen                   <chr> …
#> $ AM_forfourthhypoHowdidyoudetectyourhypoorahypothatwasabouttohappenotherpleasespecify <chr> …
#> $ AM_forfourthhypoWhathappened                                                         <chr> …
#> $ AM_forfifthhypoHowdidyoudetectyourhypoorahypothatwasabouttohappen                    <chr> …
#> $ AM_forfifthhypoHowdidyoudetectyourhypoorahypothatwasabouttohappenotherpleasespecify  <chr> …
#> $ AM_forfifthhypoWhathappened                                                          <chr> …

# View registration entries
raw_checkin %>% dplyr::filter(stage == "Registration")
#> # A tibble: 2 × 25
#>   userid day_idx stage        date       localTimestamp   AM_forfirsthypoAtwha…¹
#>   <chr>  <chr>   <chr>        <chr>      <chr>            <chr>                 
#> 1 P01    1       Registration 2026-01-01 2026-01-01 09:4… ""                    
#> 2 P02    1       Registration 2026-01-02 2026-01-02 08:3… "02:21"               
#> # ℹ abbreviated name: ¹​AM_forfirsthypoAtwhattimedidthishappen
#> # ℹ 19 more variables: AM_forsecondhypoAtwhattimedidthishappen <chr>,
#> #   AM_forthirdhypoAtwhattimedidthishappen <chr>,
#> #   AM_forfourthhypoAtwhattimedidthishappen <chr>,
#> #   AM_forfifthhypoAtwhattimedidthishappen <chr>,
#> #   AM_forfirsthypoHowdidyoudetectyourhypoorahypothatwasabouttohappen <chr>,
#> #   AM_forfirsthypoHowdidyoudetectyourhypoorahypothatwasabouttohappenotherpleasespecify <chr>, …

# Rows containing hypo times
raw_checkin %>%
  dplyr::filter(AM_forfirsthypoAtwhattimedidthishappen != "")
#> # A tibble: 9 × 25
#>   userid day_idx stage        date       localTimestamp   AM_forfirsthypoAtwha…¹
#>   <chr>  <chr>   <chr>        <chr>      <chr>            <chr>                 
#> 1 P01    7       Day-7        2026-01-07 2026-01-07 09:4… 04:08                 
#> 2 P01    12      Day-12       2026-01-12 2026-01-12 09:4… 04:01                 
#> 3 P01    13      Day-13       2026-01-13 2026-01-13 09:4… 22:17                 
#> 4 P02    1       Registration 2026-01-02 2026-01-02 08:3… 02:21                 
#> 5 P02    2       Day-2        2026-01-03 2026-01-03 08:3… 00:50                 
#> 6 P02    4       Day-4        2026-01-05 2026-01-05 08:3… 04:00                 
#> 7 P02    6       Day-6        2026-01-07 2026-01-07 08:3… 01:00                 
#> 8 P02    11      Day-11       2026-01-12 2026-01-12 08:3… 23:48                 
#> 9 P02    14      Day-14       2026-01-15 2026-01-15 08:3… 01:54                 
#> # ℹ abbreviated name: ¹​AM_forfirsthypoAtwhattimedidthishappen
#> # ℹ 19 more variables: AM_forsecondhypoAtwhattimedidthishappen <chr>,
#> #   AM_forthirdhypoAtwhattimedidthishappen <chr>,
#> #   AM_forfourthhypoAtwhattimedidthishappen <chr>,
#> #   AM_forfifthhypoAtwhattimedidthishappen <chr>,
#> #   AM_forfirsthypoHowdidyoudetectyourhypoorahypothatwasabouttohappen <chr>,
#> #   AM_forfirsthypoHowdidyoudetectyourhypoorahypothatwasabouttohappenotherpleasespecify <chr>, …
```

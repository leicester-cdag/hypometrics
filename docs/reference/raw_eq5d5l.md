# Simulated EQ‑5D‑5L Health Status Dataset

\`raw_eq5d5l\` contains a fully synthetic version of the EQ‑5D‑5L
instrument for two simulated participants (\`P01\` and \`P02\`) across
three study stages: `"Registration"`, `"Day-7"`, and `"Day-14"`. The
dataset includes ISO‑8601 timestamped assessments and categorical
responses for each of the five EQ‑5D‑5L dimensions.

## Usage

``` r
raw_eq5d5l
```

## Format

A tibble with the following variables:

- userid:

  Character participant ID (`"P01"`, `"P02"`).

- stage:

  Assessment stage (`"Registration"`, `"Day-7"`, `"Day-14"`).

- localTimestamp:

  Character ISO‑8601 timestamp including milliseconds and timezone.

- SC:

  Self‑Care domain level (`"SC1"`–`"SC5"`).

- AD:

  Anxiety/Depression domain level (`"AD1"`–`"AD5"`).

- UA:

  Usual Activities domain level (`"UA1"`–`"UA5"`).

- MB:

  Mobility domain level (`"MB1"`–`"MB5"`).

- PD:

  Pain/Discomfort domain level (`"PD1"`–`"PD5"`).

## Details

This dataset is simulated and intended for demonstrating workflows and
examples within the \`hypometrics\` package.

**Participants and timing**

- `"P01"` — Registration time: `"2026-01-01 07:17:00"` (UTC)

- `"P02"` — Registration time: `"2026-01-02 15:52:00"` (UTC)

Follow‑up assessments occur exactly \*\*7\*\* and \*\*14\*\* days after
Registration for each participant.

**EQ‑5D‑5L dimensions (each with 5 levels)**

Each EQ‑5D‑5L domain is represented using the 5‑level structure, encoded
as:

- `SC1`–`SC5`: \*\*Self‑Care\*\*

- `AD1`–`AD5`: \*\*Anxiety/Depression\*\*

- `UA1`–`UA5`: \*\*Usual Activities\*\*

- `MB1`–`MB5`: \*\*Mobility\*\*

- `PD1`–`PD5`: \*\*Pain / Discomfort\*\*

**Generation script**

This dataset is produced reproducibly using:

    data-raw/simulate_raw_eq5d5l.R

## Examples

``` r
data(raw_eq5d5l, package = "hypometrics")

# Inspect structure
dplyr::glimpse(raw_eq5d5l)
#> Rows: 6
#> Columns: 8
#> $ userid         <chr> "P01", "P01", "P01", "P02", "P02", "P02"
#> $ stage          <chr> "Day-14", "Day-7", "Registration", "Day-14", "Day-7", "…
#> $ localTimestamp <chr> "2026-01-15T07:17:00.000+00:00", "2026-01-08T07:17:00.0…
#> $ SC             <chr> "SC5", "SC3", "SC1", "SC1", "SC4", "SC5"
#> $ AD             <chr> "AD2", "AD2", "AD1", "AD2", "AD3", "AD5"
#> $ UA             <chr> "UA3", "UA3", "UA2", "UA4", "UA4", "UA2"
#> $ MB             <chr> "MB4", "MB5", "MB3", "MB2", "MB4", "MB5"
#> $ PD             <chr> "PD1", "PD2", "PD4", "PD3", "PD2", "PD1"

# Show values for P01
raw_eq5d5l %>% dplyr::filter(userid == "P01")
#> # A tibble: 3 × 8
#>   userid stage        localTimestamp               SC    AD    UA    MB    PD   
#>   <chr>  <chr>        <chr>                        <chr> <chr> <chr> <chr> <chr>
#> 1 P01    Day-14       2026-01-15T07:17:00.000+00:… SC5   AD2   UA3   MB4   PD1  
#> 2 P01    Day-7        2026-01-08T07:17:00.000+00:… SC3   AD2   UA3   MB5   PD2  
#> 3 P01    Registration 2026-01-01T07:17:00.000+00:… SC1   AD1   UA2   MB3   PD4  

# Summarise EQ-5D-5L domains by participant and stage
raw_eq5d5l %>%
  dplyr::group_by(userid, stage) %>%
  dplyr::summarise(
    across(c(SC, AD, UA, MB, PD), ~ dplyr::n_distinct(.x)),
    .groups = "drop"
  )
#> # A tibble: 6 × 7
#>   userid stage           SC    AD    UA    MB    PD
#>   <chr>  <chr>        <int> <int> <int> <int> <int>
#> 1 P01    Day-14           1     1     1     1     1
#> 2 P01    Day-7            1     1     1     1     1
#> 3 P01    Registration     1     1     1     1     1
#> 4 P02    Day-14           1     1     1     1     1
#> 5 P02    Day-7            1     1     1     1     1
#> 6 P02    Registration     1     1     1     1     1
```

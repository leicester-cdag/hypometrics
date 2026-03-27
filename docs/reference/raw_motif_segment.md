# Simulated uMotif Segment Dataset

\`raw_motif_segment\` contains a synthetic version of a uMotif-style
“Segment” dataset for two simulated participants (\`P01\`, \`P02\`).
Each participant contributes multiple uMotif timestamps, and for each
timestamp, a set of Segment IDs and associated values is recorded.

## Usage

``` r
raw_motif_segment
```

## Format

A tibble with the following variables:

- UserId:

  Character ID for the participant (`"P01"`, `"P02"`).

- uMotifTime:

  Character timestamp in `"YYYY-MM-DD HH:MM:SS"` format.

- SegmentId:

  Integer segment code: `1–10` for P01, `11–20` for P02.

- Value:

  Integer value (1–5).

## Details

**Participants**

\* `"P01"` — uMotif timeline beginning at `"2026-01-01 07:17:00"` UTC
with exactly \*\*3 uMotif timestamps\*\* over a 14‑day period.

\* `"P02"` — uMotif timeline beginning at `"2026-01-02 15:52:00"` UTC
with exactly \*\*5 uMotif timestamps\*\* over a 14‑day period.

**Generation script**

The dataset is produced using the reproducible script:

    data-raw/simulate_raw_motif_segment.R

## Examples

``` r
data(raw_motif_segment, package = "hypometrics")

# Inspect structure
dplyr::glimpse(raw_motif_segment)
#> Rows: 80
#> Columns: 4
#> $ UserId     <chr> "P01", "P01", "P01", "P01", "P01", "P01", "P01", "P01", "P0…
#> $ uMotifTime <chr> "2026-01-01 12:22:00", "2026-01-01 12:22:00", "2026-01-01 1…
#> $ SegmentId  <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1…
#> $ Value      <dbl> 1, 3, 5, 1, 2, 2, 2, 3, 3, 3, 5, 4, 4, 2, 1, 5, 4, 1, 5, 3,…

# Values for P01 at the hypoglycaemia-aligned timestamp
raw_motif_segment %>%
  dplyr::filter(UserId == "P01") %>%
  dplyr::filter(SegmentId == 1)
#> # A tibble: 3 × 4
#>   UserId uMotifTime          SegmentId Value
#>   <chr>  <chr>                   <int> <dbl>
#> 1 P01    2026-01-01 12:22:00         1     1
#> 2 P01    2026-01-08 07:17:00         1     5
#> 3 P01    2026-01-15 07:17:00         1     2

# Values for P02 at hypoglycaemia-aligned timestamps
raw_motif_segment %>%
  dplyr::filter(UserId == "P02", SegmentId == 11)
#> # A tibble: 5 × 4
#>   UserId uMotifTime          SegmentId Value
#>   <chr>  <chr>                   <int> <dbl>
#> 1 P02    2026-01-02 19:25:00        11     1
#> 2 P02    2026-01-02 19:30:00        11     1
#> 3 P02    2026-01-02 19:35:00        11     1
#> 4 P02    2026-01-13 03:52:00        11     2
#> 5 P02    2026-01-16 15:52:00        11     5
```

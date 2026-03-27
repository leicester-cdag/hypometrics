# Simulated WPAI (Work Productivity and Activity Impairment) Dataset

\`raw_wpai\` contains a fully simulated version of the \*\*WPAI
questionnaire\*\*, completed by two synthetic participants (\`P01\` and
\`P02\`) at three study stages: `"Registration"`, `"Day-7"`, and
`"Day-14"`. It includes employment status, work hours missed, hours
worked, and perceived hypoglycaemia-related impacts on productivity and
daily activities.

## Usage

``` r
raw_wpai
```

## Format

A tibble with the following variables:

- userid:

  Character: participant ID (`"P01"` or `"P02"`).

- stage:

  Character: one of `"Registration"`, `"Day-7"`, `"Day-14"`.

- localTimestamp:

  Character ISO‑8601 timestamp with milliseconds and timezone.

- areyoucurrentlyemployedworkingforpay:

  Character (`"yes"`, `"no"`).

- duringthepastsevendayshowmanyhoursdidyoumissfromworkbecauseofproblemsassociatedwithyourhypoglycaemia:

  Numeric 0–3.

- duringthepastsevendayshowmanyhoursdidyoumissfromworkbecauseofanyotherreasonsuchasvacationholidaystimeofftoparticipateinthisstudy:

  Numeric 0–50.

- duringthepastsevendayshowmanyhoursdidyouactuallywork:

  Numeric 0–70.

- duringthepastsevendayshowmuchdidhypoglycaemiaaffectyourproductivitywhileyouwereworking:

  Numeric 1–3.

- duringthepastsevendayshowmuchdidhypoglycaemiaaffectyourabilitytodoyourregulardailyactivitiesotherthanworkatajob:

  Numeric 0–3.

## Details

The dataset is intended for demonstrating how WPAI survey data can be
integrated with CGM, HR, step count, and PROMIS data within the
\`hypometrics\` workflow.

**Participants**

- `"P01"` — Registration timestamp: `"2026-01-01 07:17:00"` UTC

- `"P02"` — Registration timestamp: `"2026-01-02 15:52:00"` UTC

Follow‑up stages occur exactly \*\*7\*\* and \*\*14\*\* days after
Registration.

**Generation script**

This synthetic dataset is produced using:

    data-raw/simulate_raw_wpai.R

## Examples

``` r
data(raw_wpai, package = "hypometrics")

# Explore structure
dplyr::glimpse(raw_wpai)
#> Rows: 6
#> Columns: 9
#> $ userid                                                                                                                           <chr> …
#> $ stage                                                                                                                            <chr> …
#> $ localTimestamp                                                                                                                   <chr> …
#> $ areyoucurrentlyemployedworkingforpay                                                                                             <chr> …
#> $ duringthepastsevendayshowmanyhoursdidyoumissfromworkbecauseofproblemsassociatedwithyourhypoglycaemia                             <int> …
#> $ duringthepastsevendayshowmanyhoursdidyoumissfromworkbecauseofanyotherreasonsuchasvacationholidaystimeofftoparticipateinthisstudy <int> …
#> $ duringthepastsevendayshowmanyhoursdidyouactuallywork                                                                             <int> …
#> $ duringthepastsevendayshowmuchdidhypoglycaemiaaffectyourproductivitywhileyouwereworking                                           <int> …
#> $ duringthepastsevendayshowmuchdidhypoglycaemiaaffectyourabilitytodoyourregulardailyactivitiesotherthanworkatajob                  <int> …

# View all stages for P01
raw_wpai %>% dplyr::filter(userid == "P01")
#> # A tibble: 3 × 9
#>   userid stage      localTimestamp areyoucurrentlyemplo…¹ duringthepastsevenda…²
#>   <chr>  <chr>      <chr>          <chr>                                   <int>
#> 1 P01    Day-14     2026-01-15T07… yes                                         2
#> 2 P01    Day-7      2026-01-08T07… yes                                         0
#> 3 P01    Registrat… 2026-01-01T07… no                                          2
#> # ℹ abbreviated names: ¹​areyoucurrentlyemployedworkingforpay,
#> #   ²​duringthepastsevendayshowmanyhoursdidyoumissfromworkbecauseofproblemsassociatedwithyourhypoglycaemia
#> # ℹ 4 more variables:
#> #   duringthepastsevendayshowmanyhoursdidyoumissfromworkbecauseofanyotherreasonsuchasvacationholidaystimeofftoparticipateinthisstudy <int>,
#> #   duringthepastsevendayshowmanyhoursdidyouactuallywork <int>,
#> #   duringthepastsevendayshowmuchdidhypoglycaemiaaffectyourproductivitywhileyouwereworking <int>,
#> #   duringthepastsevendayshowmuchdidhypoglycaemiaaffectyourabilitytodoyourregulardailyactivitiesotherthanworkatajob <int>

# Summarise WPAI impacts
raw_wpai %>%
  dplyr::group_by(userid, stage) %>%
  dplyr::summarise(
    hrs_missed_hypo = mean(duringthepastsevendayshowmanyhoursdidyoumissfromworkbecauseofproblemsassociatedwithyourhypoglycaemia),
    hrs_missed_other = mean(duringthepastsevendayshowmanyhoursdidyoumissfromworkbecauseofanyotherreasonsuchasvacationholidaystimeofftoparticipateinthisstudy),
    hrs_worked = mean(duringthepastsevendayshowmanyhoursdidyouactuallywork),
    .groups = "drop"
  )
#> # A tibble: 6 × 5
#>   userid stage        hrs_missed_hypo hrs_missed_other hrs_worked
#>   <chr>  <chr>                  <dbl>            <dbl>      <dbl>
#> 1 P01    Day-14                     2               17         65
#> 2 P01    Day-7                      0               48         54
#> 3 P01    Registration               2               28         53
#> 4 P02    Day-14                     1               43         45
#> 5 P02    Day-7                      2                4         28
#> 6 P02    Registration               2               48          0
```

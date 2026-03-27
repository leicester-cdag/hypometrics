# Key Continuous Glucose Monitoring Metrics

Summarise glucose data using key metrics and leveraging various
functions from the iglu package.

## Usage

``` r
cgmSummarise(
  DataFrame,
  GlucoseUnit = "mmol/L",
  InterQuartileRange = c(25, 75),
  InRange = c(70, 180),
  AboveRange = c(180, 250),
  BelowRange = c(70, 54)
)
```

## Arguments

- DataFrame:

  A dataframe containing CGM data. Must have columns id, cgm_timestamp,
  glucose.

- GlucoseUnit:

  A character object that specifies Unit used for glucose in DataFrame.
  Can be either mmol/L or mg/dL. Depending on the unit, data
  transformation may be required within the function.

- InterQuartileRange:

  A vector of values indicating which quantile values desired for the
  glucose variable. Possible value ranges from 0 to 100. Default is 25
  and 75.

- InRange:

  Vector of values indicating target values desired to calculate the
  time in range. Default is 70 and 180mg/dL.

- AboveRange:

  Vector of values indicating target values desired to calculate the
  time above range. Default is 180 and 250mg/dL.

- BelowRange:

  Vector of values indicating target values desired to calculate the
  time below range. Default is 70 and 54mg/dL.

## Value

Data frame with one line per participant with key CGM metrics. The
metrics are produced using iglu functions and combining outputs of those
functions to generate a single data frame.

## Examples

``` r
if (FALSE) { # \dontrun{
hypometrics::cgmSummarise(DataFrame,
                          GlucoseUnit = "mmol/L",
                          InterQuartileRange = c(25,75),
                          InRange = c(70, 180),
                          AboveRange = c(180, 250),
                          BelowRange = c(70, 54))
} # }
```

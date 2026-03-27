# Links Continuous Glucose Monitoring (CGM) Time Series Data with Person-Reported Hypoglycaemia (PRH) Data

Creates a dataset containing longitudinal CGM time series data with PRH
episodes on the same row as the closest (in time) CGM timestamps. To run
this function, the PRH dataset must be first built using the prhLink
function.

## Usage

``` r
cgmprhLink(CgmDataFrame, PrhDataFrame)
```

## Arguments

- CgmDataFrame:

  A dataframe containing CGM data. Must have columns id, cgm_timestamp,
  glucose.

- PrhDataFrame:

  A dataframe containing all PRH episodes reported with a time. Output
  of the umotifLink function.

## Value

A dataset containing CGM data with timestamp of PRH episode aligned with
closest CGM timestamp.

## Examples

``` r
if (FALSE) { # \dontrun{
hypometrics::cgmprhLink(CgmDataFrame = cgm,
                        PrhDataFrame = prh_map)
} # }
```

# Links Continuous Glucose Monitoring (CGM) Data with Sleep Data

Creates a dataset containing CGM data matched with sleep data: for each
CGM timestamp, sleep status is available

## Usage

``` r
cgmsleepLink(CgmDataFrame, SleepDataFrame)
```

## Arguments

- CgmDataFrame:

  A dataframe containing CGM data. Must have columns id, cgm_timestamp,
  glucose.

- SleepDataFrame:

  A dataframe containing Fitbit sleep data.

## Value

A dataset with every cgm timestamp marked as either asleep (timestamp
fell within sleep start and and time), awake (timestamp was outside
sleep interval) or NA (no sleep information available at the time)

## Examples

``` r
if (FALSE) { # \dontrun{
hypometrics::cgmsleepLink(CgmDataFrame = cgm,
                          SleepDataFrame)
} # }
```

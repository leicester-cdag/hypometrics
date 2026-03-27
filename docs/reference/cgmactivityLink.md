# Links Continuous Glucose Monitoring (CGM) Data with Activity Data

Creates a dataset containing CGM data matched with activity data: for
each CGM timestamp, mean step count and/or heart rate is available

## Usage

``` r
cgmactivityLink(CgmDataFrame, ActivityDataFrame, DataType)
```

## Arguments

- CgmDataFrame:

  A dataframe containing CGM data. Must have columns id, cgm_timestamp,
  glucose.

- ActivityDataFrame:

  A dataframe containing Fitbit step count or heart rate data.

- DataType:

  Character object determining which data to process. 2 options
  available: "stepcount" or "heartrate".

## Value

A dataset where for each CGM timestamp there is a corresponding step
count or mean heart rate.

## Examples

``` r
if (FALSE) { # \dontrun{
hypometrics::cgmactivityLink(CgmDataFrame,
                             ActivityDataFrame,
                             DataType)
} # }
```

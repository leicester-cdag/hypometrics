# Detection and Description of Episodes of Continuous Glucose Monitoring (CGM) Hypoglycaemia

Function which summarises raw CGM data and produces an output which
includes key characteristics (e.g nadir, duration) of each CGM-detected
hypoglycaemic episode.

## Usage

``` r
sdhDetection(
  DataFrame,
  DetectionLimit = 3.9,
  DetectionDuration = 15,
  AddSleepStatus = "no"
)
```

## Arguments

- DataFrame:

  A dataframe with CGM data in which hypoglycaemic episodes will be
  detected. Must have columns id, cgm_timestamp, glucose.

- DetectionLimit:

  Object of type numeric or integer corresponding to the glucose value
  used to detect hypoglycaemia. Default is 3.9 mmol/L.

- DetectionDuration:

  Object of type numeric or integer corresponding to the duration at or
  below the DetectionLimit for a hypolgycaemic episode to be detected.
  Default is 15 (minutes).

- AddSleepStatus:

  A character object specifying whether sleep status (sleep or awake)
  should be added for each SDH. Default is "no". Other option is "yes".

## Value

A dataframe with one hypoglycaemic episode per row with characteristics
in each column. If user has CGM and sleep data available, the sleep
status can be added for each episode.

## Examples

``` r
if (FALSE) { # \dontrun{
hypometrics::sdhDetection(DataFrame,
                          DetectionLimit = 3,
                          DetectionDuration = 30,
                          AddSleepStatus = "no")
} # }
```

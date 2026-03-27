# Key Sensor-Detected Hypoglycaemia Metrics

Summarise sensor-detected hypoglycaemia (SDH) data for the chosen
glucose threshold

## Usage

``` r
sdhSummarise(
  DataFrame,
  DetectionLimit = "3.9",
  LongDuration = 120,
  AddSleepSummary = "no"
)
```

## Arguments

- DataFrame:

  A dataframe object, output of the
  [`sdhDetection()`](https://leicester-cdag.github.io/hypometrics/reference/sdhDetection.md)
  function. It includes for each participant, one hypoglycaemic episode
  per row with characteristics in each column.

- DetectionLimit:

  Object of type numeric or integer corresponding to the glucose value
  used to detect hypoglycaemia. Default is 3.9 mmol/L.

- LongDuration:

  Numeric object indicating the minimum duration used to define a long
  episode of sensor detected hypoglycaemia. Default is 120 minutes.

- AddSleepSummary:

  A character object specifying whether a summary of SDH should be added
  according to sleep status. Default is "no". Other option is "yes".

## Value

Data frame with one line per participant with key SDH metrics. This
includes the number of episodes during the day/night, mean duration of
episodes, the number long SDH and the number of days during which those
occur.

## Details

This function goes hand in hand with the
[`sdhDetection()`](https://leicester-cdag.github.io/hypometrics/reference/sdhDetection.md)
function. Once the details of each individual hypoglycaemia episode has
been produced, this function can be used to summarise them. If an SDH
summary by sleep status is required, the sdhDetection function must have
been run with the AddSleepStatus argument set to "yes".

## Examples

``` r
if (FALSE) { # \dontrun{
hypometrics::sdhSummarise(DataFrame,
                          DetectionLimit = "3.0",
                          LongDuration = 120,
                          AddSleepSummary = "no")
} # }
```

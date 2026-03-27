# Visualise Activity data

Creates a bar chart of the number of steps or heart rate minute by
minute over the study period

## Usage

``` r
activityVisualise(
  DataFrame,
  DataType,
  TimeBreak = "no",
  PageNumber = NA_real_,
  StudyID
)
```

## Arguments

- DataFrame:

  A dataframe containing Fitbit activity data.

- DataType:

  Character object determining which data to visualise. 2 options
  available: "stepcount" or "heartrate".

- TimeBreak:

  Character object which defines whether plot outputs should be split by
  time period. Default is "No" in which case there will be a single plot
  produce including data for the whole period. Other options are "week"
  and "day" in which case multiple plots will be produced according to
  TimeBreak.

- PageNumber:

  Vector indicating which page (i.e. week/day) number selected for
  visualisation as plot is faceted according to TimeBreak.

- StudyID:

  ID of participant for whom step count or heart rate data will be
  plotted.

## Value

Bar chart displaying step count or heart rate over time

## Examples

``` r
if (FALSE) { # \dontrun{
hypometrics::activityVisualise(DataFrame,
                               DataType = "stepcount",
                               TimeBreak = "no",
                               PageNumber = 1,
                               StudyID = "01001")
} # }
```

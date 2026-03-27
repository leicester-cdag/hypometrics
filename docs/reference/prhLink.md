# Links Real-Time and Retrospective uMotif Person-Reported Hypoglycaemia (PRH) Data

Creates a dataset containing real-time PRH data (i.e. through motif
flower) linked with retrospective PRH data (i.e. through daily
questionnaires). There is one row per PRH event. When the same PRH event
was reported through both methods, both timestamps are reported. There
is an option to add sleep status for each PRH timestamp. To run this
function, the uMotifClean function must be run first to produce the
motif and checkin PRH dataframes.

## Usage

``` r
prhLink(
  MotifDataFrame,
  CheckinDataFrame,
  AddSleepStatus = "no",
  SleepDataFrame = ""
)
```

## Arguments

- MotifDataFrame:

  A dataframe containing Motif PRH data. Output of the
  [umotifClean](https://leicester-cdag.github.io/hypometrics/reference/umotifClean.md)
  function.

- CheckinDataFrame:

  A dataframe containing Checkin PRH data. Output of the
  [umotifClean](https://leicester-cdag.github.io/hypometrics/reference/umotifClean.md)
  function.

- AddSleepStatus:

  A character object specifying whether sleep status (sleep or awake)
  should be added for each PRH

- SleepDataFrame:

  A dataframe containing Fitbit sleep data.

## Value

A dataset containing both real-time and retrospective PRHs in
chronological order for each participant. Where there were real-time and
retrospective PRHs within 1h of each other, those have been matched (i.e
considered to be the same event and shown on a unique row). It includes
informatin on symptoms, characteritiscs of the episode and where
applicable, whether it occuring during sleeping or waking hours.

## Examples

``` r
if (FALSE) { # \dontrun{
hypometrics::prhLink(MotifDataFrame = motif,
                           CheckinDataFrame = checkin,
                           AddSleepStatus = "yes",
                           SleepDataFrame = raw_sleep)
} # }
```

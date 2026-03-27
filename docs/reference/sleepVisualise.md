# Visualise Sleeping Patterns

Creates histograms showing the distribution of times when participant(s)
went to bed and got up

## Usage

``` r
sleepVisualise(DataFrame, VisualiseAll = TRUE, StudyID = "")
```

## Arguments

- DataFrame:

  A dataframe containing Fitbit sleep data.

- VisualiseAll:

  Logical string (TRUE/FALSE) which determines whether histograms will
  be plotted for all participants or a selected participant. Default is
  TRUE.

- StudyID:

  ID of participant for whom sleep patterns will be plotted. This is
  only relevant if VisualiseAll = FALSE, and will produce individualised
  graphs.

## Value

Two histogram plots displaying all times recorded by the Fitbit when
participant(s) went to bed and got up.

## Examples

``` r
if (FALSE) { # \dontrun{
hypometrics::sleepVisualise(DataFrame,
                            VisualiseAll = TRUE,
                            StudyID = "")
} # }
```

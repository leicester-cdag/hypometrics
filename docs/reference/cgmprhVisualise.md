# Visualise Glucose linked with Person-Reported Hypoglycaemia (PRH) Data

Plots Glucose with PRH Data Over Time

## Usage

``` r
cgmprhVisualise(
  DataFrame,
  StudyID,
  TimeBreak = "no",
  PageNumber = NA_real_,
  AddSleep = "no"
)
```

## Arguments

- DataFrame:

  A dataframe of CGM data linked with PRH data. Output of the
  [cgmprhLink](https://leicester-cdag.github.io/hypometrics/reference/cgmprhLink.md)
  function.

- StudyID:

  ID of participant for whom CGM data will be plotted.

- TimeBreak:

  Character object which defines whether plot outputs should be split by
  time period. Default is "No" in which case there will be a single plot
  produce including data for the whole period. Other options are "week"
  and "day" in which case multiple plots will be produced according to
  TimeBreak.

- PageNumber:

  Vector indicating which page (i.e. week/day) number selected for
  visualisation as plot is faceted according to TimeBreak.

- AddSleep:

  Character object for the user to specify whether the CGM plot will
  have added day/night visualisation ("no" - default) or sleep/awake
  visualisation ("yes"). If set to "yes", the function requires a CGM
  dataset with a sleep_status (Awake vs asleep) column as input - this
  can be achieved by running the
  [cgmsleepLink](https://leicester-cdag.github.io/hypometrics/reference/cgmsleepLink.md)
  followed by the
  [cgmprhLink](https://leicester-cdag.github.io/hypometrics/reference/cgmprhLink.md)
  function.

## Value

A graphical representation showing glucose trace wtih PRH time points
over time with shaded area representing night or sleep time.

## Details

This functions plots CGM with PRH data over time with red shaded area
corresponding to the time period between 00:00 and 06:00 as typically
used to describe nocturnal hypoglycaemia. The function offers the
options to look at the glucose data over the entire study period, for a
specific week or specific date. It also offers the option to plot data
with corresponding sleep status (awake vs asleep) if sleep tracker data
is available. Where sleep data is missing, the area is coloured grey.

## Examples

``` r
if (FALSE) { # \dontrun{
hypometrics::cgmprhVisualise(DataFrame,
                            StudyID = "001",
                            TimeBreak = "day",
                            PageNumber = 7,
                            AddSleep = "no")
} # }
```

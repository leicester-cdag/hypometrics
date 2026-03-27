# Key Person-Reported Hypoglycaemia Metrics

Summarises person-reported hypoglycaemia (PRH) data

## Usage

``` r
prhSummarise(DataFrame, AddSleepSummary = "no")
```

## Arguments

- DataFrame:

  A dataframe object, output of the
  [`prhLink()`](https://leicester-cdag.github.io/hypometrics/reference/prhLink.md)
  function. It includes for each participant, one hypoglycaemic episode
  per row with characteristics in each column.

- AddSleepSummary:

  A character object specifying whether a summary of PRH should be added
  according to sleep status. Default is "no". Other option is "yes".

## Value

Data frame with one line per participant with the count of different
types of PRHs. This includes the number of symptomatic, prevented
episodes, night status, and sleep status where specified.

## Details

This function goes hand in hand with the
[`prhLink()`](https://leicester-cdag.github.io/hypometrics/reference/prhLink.md)
function. Once the details of each individual hypoglycaemia episode has
been produced, this function can be used to summarise them. There is an
option to obtain a summary according to sleep status if the
AddSleepSummary is set as "yes".

## Examples

``` r
if (FALSE) { # \dontrun{
hypometrics::prhSummarise(DataFrame,
                          AddSleepSummary = "yes")
} # }
```

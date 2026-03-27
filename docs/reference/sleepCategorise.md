# Categorise Sleep Information from Fitbit Raw Data

Function creates 3 separate datasets according to Fitbit sleep data:
main sleeping periods, short wake periods and detailed sleep stages.

## Usage

``` r
sleepCategorise(DataFrame)
```

## Arguments

- DataFrame:

  A dataframe containing Fitbit sleep data.

## Value

Three dataframes: one including information on main sleeping periods,
another containing more detailed data regarding sleeping stages and the
last one including information on short wake periods occurring during
the night.

## Examples

``` r
if (FALSE) { # \dontrun{
hypometrics::sleepCategorise(DataFrame)
} # }
```

# Cleans Raw uMotif Files Downloaded From the Data Download Portal

This function enables the cleaning of raw uMotif data according to the
type of file

## Usage

``` r
umotifClean(DataFrame, FileType)
```

## Arguments

- DataFrame:

  A dataframe containing uMotif data.

- FileType:

  Character object indicating what type of file is to be cleaned. Can be
  either "motif", "checkin", "promis", "wpai" or "eq5d5l".

## Value

A dataset containing clean uMotif data (e.g. daily questionnaires, wpai)

## Examples

``` r
if (FALSE) { # \dontrun{
hypometrics::umotifClean(DataFrame = raw_checkin,
                        FileType = "checkin")
} # }
```

# Visualise uMotif Symptoms Data

Plots Symptoms of Hypoglycaemia, Their Combination and Prevalence By
Glucose Range

## Usage

``` r
prhVisualise(DataFrame, GraphType, VisualiseAll = TRUE, UserID = "")
```

## Arguments

- DataFrame:

  A dataframe of CGM data which will be filled and interpolated where
  specified. Must contain columns: id, cgm_timestamp and glucose.

- GraphType:

  Type of visualisation to be used. Can be "upset" to visualise
  combinations of symptoms of hypoglycaemia reported (leveraging
  [upset](https://rdrr.io/pkg/UpSetR/man/upset.html) function from
  UpSetR package) or "heatmap" to visualise frequency of symptoms by
  glucose concentration reported

- VisualiseAll:

  Logical string (TRUE/FALSE) which determines whether graphs will be
  plotted for all participants or a selected participant. Default is
  TRUE.

- UserID:

  umotifID of participant for whom symptom graph will be plotted. This
  is only relevant if VisualiseAll = FALSE, and will produce
  individualised graphs.

## Value

An upset plot of the combination of symptoms reported or a heat map of
the prevalence of hypoglycaemia symptoms by glucose range.

## Details

This functions plots person-reported hypoglycaemia symptoms data. The
function offers the options to look at overall or individual data, and
combination of symptoms or prevalence of symptoms by glucose range.

## Examples

``` r
if (FALSE) { # \dontrun{
hypometrics::prhVisualise(DataFrame,
                             GraphType = "upset"
                             )
} # }
```

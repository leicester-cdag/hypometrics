# Links Sensor-Detected Hypoglycaemia (SDH) with Person-Reported Hypoglycaemia (PRH) episodes

Creates a dataset containing SDH episodes (from CGM data) linked with
PRH episodes (from uMotif data). There is one row per hypoglycaemic
episode. When PRH episodes fall within +/- 1h the SDH interval, they are
reported on the same row. To run this function, SDH and PRH datasets
must be first built using sdhDetection and umotifprhLink functions.

## Usage

``` r
sdhprhLink(SdhDataFrame, PrhDataFrame)
```

## Arguments

- SdhDataFrame:

  A dataframe containing all SDH episodes. Output of the
  [sdhDetection](https://leicester-cdag.github.io/hypometrics/reference/sdhDetection.md)
  function.

- PrhDataFrame:

  A dataframe containing all PRH episodes reported with a time. Output
  of the umotifLink function.

## Value

A dataset containing both SDHs and PRHs in chronological order for each
participant. Where PRHs fall within -/+ 1h of SDH interval, those have
been matched (i.e considered to be the same event and shown on a unique
row).

## Examples

``` r
if (FALSE) { # \dontrun{
hypometrics::sdhprhLink(SdhDataFrame = sdh,
                        PrhDataFrame = prh)
} # }
```

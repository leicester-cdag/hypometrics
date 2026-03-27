# Implicit to Explicit Missingness and Linear Interpolation of Glucose Data.

Transforms implicit gaps in CGM timestamps into explicit missing glucose
values, and linearly interpolate glucose values if needed.

## Usage

``` r
cgmInterpolate(
  DataFrame,
  Interpolate = TRUE,
  MinGap = 120,
  MaxGap = 1800,
  Granularity = 60
)
```

## Arguments

- DataFrame:

  A dataframe of CGM data which will be filled and interpolated where
  specified. Must contain columns: id, cgm_timestamp and glucose.

- Interpolate:

  Logical value (TRUE/FALSE) which determines whether linear
  interpolation should be conducted. If FALSE, implicit gaps in CGM
  timestamps will be turned into explicit gaps and no further changes to
  the data will be made.D Default is TRUE so that linear interpolation
  is conducted.

- MinGap:

  Numeric value which defines the minimum duration in seconds of gaps to
  be filled and interpolated. Default is 120 seconds, i.e. 2 minutes

- MaxGap:

  Numeric value which defines the maximum duration in seconds of gaps to
  be filled and interpolated. Default is 1800 seconds, i.e. 30 mines

- Granularity:

  Numeric value which defines the granularity of the Glucose data
  desired in seconds. Typically closely linked to the MinGap parameter
  e.g. if granularity of data needed is 60 seconds, MinGap would be 120
  so that gap is wide enough to add extra CGM timestamps. Default is 60
  seconds or 1 min. i.e. the resulting data frame will have CGM data at
  the 1 -min level.

## Value

A dataframe of CGM data interpolated. Gaps above the maximum duration
are left as is.

## Details

This functions firstly turns implicit gaps in CGM timestamps into
explicit missing glucose values. Secondly, if needed, it interpolates
missing glucose value as defined by the MinGap, MaxGap and Granularity
parameters.

## Examples

``` r
if (FALSE) { # \dontrun{
hypometrics::cgmInterpolate(DataFrame)
} # }
```

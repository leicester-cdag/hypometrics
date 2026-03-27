# Unzips and Reads Raw uMotif Files Downloaded From the Data Download Portal

This function enables the unzipping and reading of raw uMotif csv files
following download from data download portal.

## Usage

``` r
umotifRead(Unzip = FALSE, FolderPath, FilePattern)
```

## Arguments

- Unzip:

  Logical string (TRUE/FALSE) which determines whether uMotif folder
  needs to be unzipped or not

- FolderPath:

  Character object indicating path to folder where uMotif data is stored

- FilePattern:

  Character object indicating the pattern in the file name that will be
  used to extract the uMotif files of interest. For example,
  "evening-checkin" for evening questionnaires, "wpai" for work
  productivity questionnaire, "motif_segmentvalue" for symptoms files.

## Value

A dataset containing original uMotif data (e.g. daily questionnaires,
wpai)

## Examples

``` r
if (FALSE) { # \dontrun{
hypometrics::umotifRead(Unzip = TRUE,
                        FolderPath = "~/Documents",
                        FilePattern = "morning-checkin"
                        )
} # }
```

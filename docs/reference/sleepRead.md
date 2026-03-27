# Unzip, Read and Combine Raw Sleep Files Downloaded From User's Fitbit account

This function enables the unzipping, reading and combining of raw sleep
files following download from user's Fitbit account.

## Usage

``` r
sleepRead(Unzip = FALSE, FolderPath, FileType, StudyID)
```

## Arguments

- Unzip:

  Logical string (TRUE/FALSE) which determines whether fitbit folder
  needs to be unzipped or not

- FolderPath:

  Character object indicating path to folder where Fitbit data is stored

- FileType:

  Character object indicating what type of file is to be read. Can be
  either "json" or "csv".

- StudyID:

  ID of participant for whom activity data will be read.

## Value

A dataset containing original Fitbit sleep data with an additional
column with participant's ID.

## Examples

``` r
if (FALSE) { # \dontrun{
hypometrics::sleepRead(Unzip = TRUE,
                       FolderPath = "~/Documents",
                       FileType = "json",
                       StudyID = "001")
} # }
```

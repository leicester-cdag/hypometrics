hypometrics Shiny GUI

A graphical user interface for the hypometrics R package, built as an R Shiny web application. It provides access to the full functionality of the package (CGM analysis, sleep analysis, physical activity analysis, person-reported hypoglycaemia analysis, and the cross-stream linking functions) without requiring the user to write any R code.

Developed as part of an MSc Bioinformatics Independent Research Project (BS7130), University of Leicester.

Author: Maya Jhyount Supervisor: Dr Gilberte Martine-Edith

What it does

The GUI is organised into tabs that map onto the hypometrics package's functionality.

Data: upload CGM, sleep, physical activity, and PRH (person-reported hypoglycaemia) data, either as a single combined file or as separate files per data type. Includes a column mapper for confirming timestamp, glucose and ID columns, and validates uploads before loading.

CGM: data quality checking (cgmCheck), gap interpolation (cgmInterpolate), summary statistics (cgmSummarise), and sensor-detected hypoglycaemia episode detection and summarisation (sdhDetection, sdhSummarise).

Physical Activity: step count and heart rate visualisation (activityVisualise).

Sleep: sleep period categorisation, summary statistics, and onset/offset distribution plots (sleepCategorise, sleepSummarise, sleepVisualise).

Person-Reported Hypoglycaemia: cleaning and linking of real-time and retrospective PRH reports (umotifClean, prhLink, prhSummarise).

CGM-Sleep, CGM-Activity, CGM-PRH: the package's cross-stream linking functions (cgmsleepLink, cgmactivityLink, cgmprhLink, sdhprhLink), which tag or match CGM readings against sleep status, activity data, and reported hypoglycaemia episodes.

All plots and tables can be downloaded as PNG or CSV from the interface.

Requirements

R (developed and tested on R 4.4.3)

The hypometrics package, installed from GitHub. See package documentation for installation instructions.

CRAN packages: shiny, bslib, ggplot2, plotly, dplyr, tidyr, DT, lubridate

Install the CRAN dependencies with:

install.packages(c("shiny", "bslib", "ggplot2", "plotly", "dplyr", "tidyr", "DT", "lubridate"))

Running the app

shiny::runApp("hypometrics_gui_final.R")

The app opens with example data loaded from the hypometrics package by default, so its functionality can be explored immediately without needing to upload anything.

Data format

The GUI is built around Fitbit-style sleep and activity data and CGM exports with a timestamp and glucose column. It includes flexible column-name matching and automatic delimiter and format detection to accommodate exports from different devices and platforms, but it expects data at the granularity the underlying hypometrics functions were designed for. See the in-app About tab for the exact column names and types expected by each upload type.

Validation

Beyond the package's own example datasets, this GUI was tested against three independent external datasets not used in the development of hypometrics itself, to check robustness to real-world data variation in naming conventions, delimiters, date formats, missing columns, and glucose units. This process surfaced several genuine issues in the underlying hypometrics package, summarised below.

Known limitations

hypometrics::sleepVisualise() has a bug in its VisualiseAll = TRUE code path (mismatched scale_x_time breaks and labels). The GUI defaults to the single-participant view, which is unaffected, and shows an explanatory message if a user selects "Show all participants".

hypometrics::cgmVisualise() assumes mmol/L input with no unit parameter. The GUI converts glucose to mmol/L before calling it when the user's data is in mg/dL, then relabels the axis to the user's chosen display unit.

hypometrics::sleepCategorise() and sleepVisualise() require startTime and endTime as character strings rather than POSIXct objects, and logId as numeric rather than character. The GUI's upload pipeline formats data to match what the functions actually require.

hypometrics::cgmactivityLink() requires pre-sorted, non-missing timestamps in both input data frames but does not enforce or document this. The GUI sorts and filters data before calling it.

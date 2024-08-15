#' @title Checking Daily Patterns of Missingness in CGM data
#'
#' @description
#' Calculates the daily number of CGM hours available for analysis and
#' creates a graph highlighting missing glucose data,
#' leveraging \link[imputeTS]{ggplot_na_distribution} function from imputeTS package.
#'
#'
#' @param DataFrame A dataframe containing CGM data in which missingness will be assessed.
#' Must have columns id, cgm_timestamp, glucose.
#' @param CheckAll Logical string (TRUE/FALSE) which determines whether missingness will be checked for all participants
#' (in which case an individual graph will not be produced) or a selected participant (in which case an
#' individual missingness graph will be produced). Default is TRUE.
#' @param StudyID ID of participant for which CGM data will be checked for missingness. This is only
#' relevant if CheckAll = FALSE, and will produce 3 individualised graphs.
#' @param AxisLabels Vector of numeric values determining the labels for the breaks on the y axis (i.e. glucose) of the
#' missingness distribution graph. Only relevant when CheckAll = FALSE.
#'
#' @return If CheckAll = TRUE returns a data frame with 3 columns: id, date and number of CGM hours available.
#' If CheckAll = FALSE, returns a list containing: data frame with id, date and number of CGM hours available and
#' a glucose trace with missingness in data highlighted in orange.
#'
#' @examples
#' \dontrun{
#' # Checking daily missingness in CGM data for all participants
#' hypometrics::checkMissingCGM(DataFrame)
#'
#' #Checking daily missingness in CGM data for a specific participant
#' hypometrics::checkMissingCGM(DataFrame,
#'                              CheckAll = FALSE,
#'                              StudyID = "001",
#'                              AxisLabels = c(0, 2.2, 3.9, 10, 20))
#' }
#'
#'@export
checkMissingCGM <- function(DataFrame,
                            CheckAll = TRUE,
                            StudyID = "",
                            AxisLabels = NA_real_) {


  #### Check function arguments and data frame columns ####

  chk::check_names(DataFrame, names = c("id", "cgm_timestamp", "glucose"))

  chk::chk_character(DataFrame$id)

  checkmate::assertPOSIXct(DataFrame$cgm_timestamp)

  chk::chk_numeric(DataFrame$glucose)

  chk::chk_flag(CheckAll)

  chk::chk_character(StudyID)

  chk::chk_numeric(AxisLabels)

#### CGM Data completeness check  ####

if(CheckAll == TRUE){

  #### Calculating number of available CGM hours per day for each individual in the whole sample ####

  cgm_completeness_check <- DataFrame %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(Date = as.Date(cgm_timestamp)) %>%
    dplyr::group_by(id, Date) %>%
    #Get the full duration of when CGM data is non-missing - identifies the timestamp up until when CGM-data isn't missing
    dplyr::mutate(gluc_complete = ifelse(!is.na(glucose) | !is.na(lag(glucose)), 1, 0),
           seq = ifelse(gluc_complete==1, data.table::rleid(gluc_complete), NA_real_)) %>%
    dplyr::group_by(id, Date, seq) %>%
    dplyr::mutate(time_diff = ifelse(!is.na(seq),
                              lubridate::time_length(lubridate::as.interval(dplyr::lag(cgm_timestamp), cgm_timestamp), unit = "hour"),
                              NA_real_)) %>%
    dplyr::group_by(id, Date) %>%
    dplyr::summarise(total_cgm_hours = sum(time_diff, na.rm=T)) %>%
    dplyr::ungroup()

  ## Return output ##
  return(cgm_completeness_check = cgm_completeness_check)

} else{

  #### Filtering selected participant's ID ###

  DataFrame <- DataFrame %>%
    dplyr::filter(id == StudyID)

  #### Calculating number of available CGM hours per day for the selected participants ####

    cgm_completeness_check <- DataFrame %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(Date = as.Date(cgm_timestamp)) %>%
      dplyr::group_by(id, Date) %>%
      #Get the full duration of when CGM data is non-missing - identifies the timestamp up until when CGM-data isn't missing
      dplyr::mutate(gluc_complete = ifelse(!is.na(glucose) | !is.na(dplyr::lag(glucose)), 1, 0),
                    seq = ifelse(gluc_complete==1, data.table::rleid(gluc_complete), NA_real_)) %>%
      dplyr::group_by(id, Date, seq) %>%
      dplyr::mutate(time_diff = ifelse(!is.na(seq),
                                       lubridate::time_length(lubridate::as.interval(dplyr::lag(cgm_timestamp), cgm_timestamp), unit = "hour"),
                                       NA_real_)) %>%
      dplyr::group_by(id, Date) %>%
      dplyr::summarise(total_cgm_hours = sum(time_diff, na.rm=T)) %>%
      dplyr::ungroup()

    #### Creating plots to visualise the patterns of missingness in CGM data ####

  cgm_na_distribution_plot <-
    imputeTS::ggplot_na_distribution(x = DataFrame$glucose,
                                     x_axis_labels = DataFrame$cgm_timestamp,
                                     color_points = "dark red",
                                     color_lines = "dark red",
                                     #color_missing = "orange", #leaving this as default to indian red as the parameters color missing argument does not seem to work - colour only changes when you change colour missing border.
                                     color_missing_border = "tan1",
                                     #alpha_missing = 0.5,
                                     title = paste0("Overview of missingness in ",
                                                    StudyID,
                                                    "'s CGM data"),
                                     subtitle = "Gaps in glucose data highlighted in orange",
                                     xlab = "Date (DD-MM-YYYY)",
                                     ylab = "Glucose concentration (mmol/L)",
                                     size_points = 1) +
    ggplot2::scale_x_datetime(date_labels = "%d-%m-%Y",
                              date_breaks = "1 day",
                              expand = c(0,0)) +
    ggplot2::scale_y_continuous(breaks = AxisLabels,
                                expand=c(0,0)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                       size = 8,
                                                       hjust = 0.5,
                                                       vjust = 0.5),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank())


  ### Return completeness check output and data visualisations ###
  return(list(cgm_completeness_check = cgm_completeness_check,
              cgm_na_distribution_plot = cgm_na_distribution_plot)
         )

}

}

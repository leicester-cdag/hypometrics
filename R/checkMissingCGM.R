#' @title checkMissingCGM
#'
#' @description
#' Calculates the completeness of CGM and provides visualisations to examine the amount of missing data.
#'
#'
#' @param DataFrame A dataframe containing CGM data in which missingness will be assessed.
#' Must have columns id, time, gl.
#'
#' @return A list containing: the CGM data with an additional Date and total_cgm_hours_per_day column,
#' a visualisation of the distribution of NAs within the data, a visualisation of the percent of NA's
#' in the data, and a visualisation of the pattern of NAs in the data.
#' @export
#'
#' @examples
#' \dontrun{
#' hypometrics::checkMissingCGM(DataFrame)
#' }
#'
checkMissingCGM <- function(DataFrame) {
  CGM_completeness_check <- DataFrame %>%
    dplyr::mutate(Date = as.Date(time)) %>%
    dplyr::group_by(Date) %>%
    dplyr::mutate(gluc_complete = ifelse(!is.na(gl) | !is.na(lag(gl)), 1, 0), #this ensures we get the full duration of when CGM data is non-missing - identifies the timestamp up until when CGM-data isn't missing
           seq = ifelse(gluc_complete==1, data.table::rleid(gluc_complete), NA_real_)) %>%
    dplyr::group_by(Date, seq) %>%
    dplyr::mutate(time_diff = ifelse(!is.na(seq),
                              lubridate::time_length(lubridate::as.interval(dplyr::lag(time), time), unit = "hour"),
                              NA_real_)) %>%
    dplyr::group_by(Date) %>%
    dplyr::mutate(total_cgm_hours_per_day = sum(time_diff, na.rm=T)) %>%
    dplyr::select(-c(gluc_complete, seq, time_diff))

  CGM_NA_distribution_plot <- imputeTS::ggplot_na_distribution(x = DataFrame$gl,
                           x_axis_labels = DataFrame$time,
                           color_points = "dark red",
                           color_lines = "dark red",
                           #color_missing = "orange", #leaving this as default to indian red as the parameters color missing argument does not seem to work - colour only changes when you change colour missing border.
                           color_missing_border = "tan1",
                           #alpha_missing = 0.5,
                           title = "Distribution of CGM data",
                           subtitle = "CGM with gaps highlighted in orange",
                           xlab = "Date",
                           ylab = "Glucose concentration",
                           size_points = 2) +
    ggplot2::scale_x_datetime(date_labels = "%d-%m-%Y",
                     date_breaks = "1 day",
                     expand = c(0,0)) +
    ggplot2::scale_y_continuous(#limits = c(0, 30),
      breaks = c(2.2, 3.9, 10, 20),
      expand=c(0,0)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank())

  CGM_NA_percent_plot <- imputeTS::ggplot_na_distribution2(DataFrame$gl,
                        interval_size = 288, #this is because there should be about 288 timestamps per day so that means that R looks for blocks of 288 observations (24h blocks) and determine % missing data within that window
                        title = "Gaps per day",
                        xlab = "Study day") +
    ggplot2::scale_x_continuous(breaks = seq(288, 20160, 288),
                       labels = c(1:70),
                       expand=c(0,0)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 1.3, vjust = 2))


  CGM_NA_pattern_plot <- imputeTS::ggplot_na_gapsize(DataFrame$gl,
                      title = "Occurence of gaps sizes in CGM data",
                      include_total=F,
                      limit=50) +
    ggplot2::scale_y_continuous(breaks = seq(0, 50, 2),
                       expand=c(0,0))

  return(list(data = CGM_completeness_check,
              distribution_plot = CGM_NA_distribution_plot,
              percent_plot = CGM_NA_percent_plot,
              pattern_plot = CGM_NA_pattern_plot)
  )
}

# x <- checkMissingCGM(iglu_data)
#
# x[[1]]
# x[[2]]
# x[[3]]
# x[[4]]

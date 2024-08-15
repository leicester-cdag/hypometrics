#' @title Key Continuous Glucose Monitoring Metrics
#'
#' @description
#' Summarise glucose data using key metrics and leveraging various functions from
#' the iglu package.
#'
#' @param DataFrame A dataframe containing CGM data.
#' Must have columns id, cgm_timestamp, glucose.
#' @param GlucoseUnit A character object that specifies Unit used for glucose in DataFrame.
#' Can be either mmol/L or mg/dL. Depending on the unit, data transformation may be required within the function.
#' @param InterQuartileRange A vector of values indicating which quantile values desired for the glucose variable.
#' Possible value ranges from 0 to 100. Default is 25 and 75.
#' @param InRange Vector of values indicating target values desired to calculate the time in range.
#' Default is 70 and 180mg/dL.
#' @param AboveRange Vector of values indicating target values desired to calculate the time above range.
#' Default is 180 and 250mg/dL.
#' @param BelowRange Vector of values indicating target values desired to calculate the time below range.
#' Default is 70 and 54mg/dL.
#'
#' @return Data frame with one line per participant with key CGM metrics. The metrics are produced using
#' iglu functions and combining outputs of those functions to generate a single data frame.
#'
#' @examples
#' \dontrun{
#' hypometrics::cgmSummarise(DataFrame,
#'                           GlucoseUnit = "mmol/L",
#'                           InterQuartileRange = c(25,75),
#'                           InRange = c(70, 180),
#'                           AboveRange = c(180, 250),
#'                           BelowRange = c(70, 54))
#' }
#'
#'@export
cgmSummarise <- function(DataFrame,
                         GlucoseUnit = "mmol/L",
                         InterQuartileRange = c(25, 75),
                         InRange = c(70, 180),
                         AboveRange = c(180, 250),
                         BelowRange = c(70, 54)){

  #### Check function arguments and data frame columns ####

  chk::check_names(DataFrame, names = c("id", "cgm_timestamp", "glucose"))

  chk::chk_character(DataFrame$id)

  checkmate::assertPOSIXct(DataFrame$cgm_timestamp)

  chk::chk_numeric(DataFrame$glucose)

  chk::chk_character(GlucoseUnit)

  if(!GlucoseUnit %in% c("mmol/L", "mg/dL")){
    stop("GlucoseUnit must be `mmol/L` or `mg/dL` only.")
  }

  chk::chk_numeric(InterQuartileRange)

  chk::chk_numeric(InRange)

  chk::chk_numeric(AboveRange)

  chk::chk_numeric(BelowRange)

  #### Function within the function so it can be called at the participant level ####

  cgm_overview <- function(df){

  #### Data Preparation ####
  df_glu <- df %>%
    #getting data in an iglu-friendly format
    dplyr::rename(gl = glucose,
                  time = cgm_timestamp) %>%
    dplyr::select(id, time, gl)

  if(GlucoseUnit == "mmol/L"){

    df_glu <- df_glu %>%
      transform(gl = gl * 18)
  }

  #### Glucometrics ####

  key_metrics <- list(iglu::active_percent(df_glu),
                      iglu::mean_glu(df_glu),
                      iglu::sd_glu(df_glu),
                      iglu::median_glu(df_glu),
                      iglu::quantile_glu(df_glu, quantiles = InterQuartileRange),
                      iglu::cv_glu(df_glu),
                      iglu::in_range_percent(df_glu, target_ranges = list(InRange)),
                      iglu::above_percent(df_glu, targets_above = AboveRange),
                      iglu::below_percent(df_glu, targets_below = BelowRange)) %>%
    purrr:::reduce(dplyr::full_join)

  if(GlucoseUnit == "mmol/L"){

    key_metrics <- key_metrics %>%
      transform(mean = mean/18,
                SD = SD/18,
                median = median/18,
                `25` = `25`/18,
                `75` = `75`/18)
  }


  key_metrics <- key_metrics %>%
    dplyr::mutate(nweeks = ndays/7) %>%
    transform(nweeks = as.numeric(nweeks)) %>%
    dplyr::rename(start_date_cgm = start_date, end_date_cgm = end_date, mean_glu = mean, sd_glu = SD,
                  median_glu = median, Q1_glu = X25, Q3_glu = X75, cv_glu = CV) %>%
    dplyr::mutate_if(is.numeric, round, digits = 1) %>%
    dplyr::select(id, start_date_cgm, end_date_cgm, nweeks, ndays, active_percent, mean_glu, sd_glu, median_glu, Q1_glu, Q3_glu,
                  cv_glu, above_250, above_180, in_range_70_180, below_70, below_54)

  if(GlucoseUnit == "mmol/L"){

    key_metrics <- key_metrics %>%
      dplyr::rename(in_range_3.9_10 = in_range_70_180,
                    above_13.9 = above_250,
                    above_10 = above_180,
                    below_3.9 = below_70,
                    below_3 = below_54)
  }

return(key_metrics = key_metrics)
}

  #### Splitting the DataFrame by id ####
  list_participants <- split(DataFrame, DataFrame$id)

  #### Calling the function for each data frame containing individual participants data ####
  list_data <- lapply(list_participants, cgm_overview)

  #### Row binding individual participants data in a single dataframe ####
  final_data <- dplyr::bind_rows(list_data, .id = "id")

  #### Return CGM key metrics ####
  return(final_data = data.frame(final_data))


}

#' @title Key Sensor-Detected Hypoglycaemia Metrics
#'
#' @description Summarise sensor-detected hypoglycaemia (SDH) data for the chosen glucose threshold
#'
#' @param DataFrame A dataframe object, output of the \code{\link[hypometrics]{sdhDetection}()} function.
#' It includes for each participant, one hypoglycaemic episode per row with characteristics in each column.
#' @param DetectionLimit  Object of type numeric or integer corresponding to
#' the glucose value used to detect hypoglycaemia. Default is 3.9 mmol/L.
#' @param LongDuration Numeric object indicating the minimum duration used to define a long
#' episode of sensor detected hypoglycaemia. Default is 120 minutes.
#'
#' @details This function goes hand in hand with the \code{\link[hypometrics]{sdhDetection}()} function. Once
#' the details of each individual hypoglycaemia episode has been produced, this function can be used to summarise
#' them.
#'
#' @return Data frame with one line per participant with key SDH metrics. This includes the number of episodes
#' during the day/night, mean duration of episodes, the number long SDH
#' and the number of days during which those occur.
#'
#' @examples
#' \dontrun{
#' hypometrics::sdhSummarise(DataFrame,
#'                           DetectionLimit = "3.0",
#'                           LongDuration = 120)
#' }
#'
#'@export
sdhSummarise <- function(DataFrame,
                         DetectionLimit = "3.9",
                         LongDuration = 120){


  #### Check function arguments and columns included in dataset ####

  chk::check_names(DataFrame, names = c("id", "sdh_number", "sdh_night_status", "sdh_duration_mins", "sdh_interval"))

  chk::chk_character(DetectionLimit)

  chk::chk_numeric(LongDuration)

  #Setting variable names
  varnames <- c("id",
                paste0("n_sdh", DetectionLimit),
                paste0("n_sdh", DetectionLimit, "_day"),
                paste0("n_sdh", DetectionLimit, "_night"),
                paste0("n_sdh", DetectionLimit, "_overlap"),
                paste0("mean_duration_sdh", DetectionLimit),
                paste0("mean_duration_sdh", DetectionLimit, "_day"),
                paste0("mean_duration_sdh", DetectionLimit, "_night"),
                paste0("mean_duration_sdh", DetectionLimit, "_overlap"))

  # SDH number and duration summary
  df_sdh <- DataFrame %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(dplyr::n_distinct(sdh_number),
                     dplyr::n_distinct(sdh_number[sdh_night_status=="Day"]),
                     dplyr::n_distinct(sdh_number[sdh_night_status=="Night"]),
                     dplyr::n_distinct(sdh_number[sdh_night_status=="Overlap"]),
                     mean(sdh_duration_mins, na.rm = T),
                     mean(sdh_duration_mins[sdh_night_status=="Day"]),
                     mean(sdh_duration_mins[sdh_night_status=="Night"]),
                     mean(sdh_duration_mins[sdh_night_status=="Overlap"])) %>%
    magrittr::set_names(varnames) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_if(is.numeric, round, digits = 1)

#Set variable names for second dataset
  varnames2 <- c("id",
                 paste0("n_longsdh", DetectionLimit),
                 paste0("n_day_with_longsdh", DetectionLimit))

# Long SDH number and number of days
  df_longsdh <- DataFrame %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(dplyr::n_distinct(sdh_number[sdh_duration_mins >= LongDuration]),
                     dplyr::n_distinct(as.Date(lubridate::int_start(sdh_interval[sdh_duration_mins >= LongDuration])))) %>%
    magrittr::set_names(varnames2) %>%
    dplyr::ungroup()

  #### Combining the two data sets
  combined_sdh_summary <- dplyr::full_join(df_sdh,
                                           df_longsdh,
                                           by = dplyr::join_by(id))

  #### Return output ####
  return (combined_sdh_summary = data.frame(combined_sdh_summary))

}

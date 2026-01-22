#' @title Implicit to Explicit Missingness and Linear Interpolation of Glucose Data.
#'
#' @description Transforms implicit gaps in CGM timestamps into explicit
#' missing glucose values, and linearly interpolate glucose values if needed.
#'
#' @param DataFrame A dataframe of CGM data which will be filled and interpolated
#' where specified. Must contain columns: id, cgm_timestamp and glucose.
#' @param Interpolate Logical value (TRUE/FALSE) which determines whether
#' linear interpolation should be conducted. If FALSE, implicit gaps in CGM timestamps
#' will be turned into explicit gaps and no further changes to the data will be made.D
#' Default is TRUE so that linear interpolation is conducted.
#' @param MinGap Numeric value which defines the minimum duration in seconds
#' of gaps to be filled and interpolated. Default is 120 seconds, i.e. 2 minutes
#' @param MaxGap Numeric value which defines the maximum duration in seconds
#' of gaps to be filled and interpolated. Default is 1800 seconds, i.e. 30 mines
#' @param Granularity Numeric value which defines the granularity of the
#' Glucose data desired in seconds. Typically closely linked to the MinGap parameter e.g.
#' if granularity of data needed is 60 seconds, MinGap would be 120 so that gap is
#' wide enough to add extra CGM timestamps. Default is 60 seconds or 1 min. i.e. the
#' resulting data frame will have CGM data at the 1 -min level.
#'
#' @details
#' This functions firstly turns implicit gaps in CGM timestamps into explicit
#' missing glucose values. Secondly, if needed, it interpolates missing glucose value as defined
#' by the MinGap, MaxGap and Granularity parameters.
#'
#' @return A dataframe of CGM data interpolated. Gaps above the maximum duration
#' are left as is.
#'
#' @examples
#' \dontrun{
#' hypometrics::cgmInterpolate(DataFrame)
#' }
#'
#' @export
cgmInterpolate <- function(DataFrame,
                           Interpolate = TRUE,
                           MinGap = 120,
                           MaxGap = 1800,
                           Granularity = 60){

  #### Check function arguments and data frame columns ####

  chk::check_names(DataFrame, names = c("id", "cgm_timestamp", "glucose"))

  chk::chk_character(DataFrame$id)

  checkmate::assertPOSIXct(DataFrame$cgm_timestamp)

  chk::chk_numeric(DataFrame$glucose)

  chk::chk_flag(Interpolate)

  chk::chk_numeric(MinGap)

  chk::chk_numeric(MaxGap)

  chk::chk_numeric(Granularity)

  #### Function within the function which will turn implicit glucose missingness into explicit and fill gaps where specified for each id ####
  fill <- function(df){

    cgmtimestamps <- c(df$cgm_timestamp)
    diff_time_sec <- difftime(df$cgm_timestamp, dplyr::lag(df$cgm_timestamp), units="secs")
    diff_time_sec <- diff_time_sec[!is.na(diff_time_sec)]
    timediff_timestamps <- as.numeric(diff_time_sec)

    #Identify indices of timestamps where there is a sufficient gap (between min and max) to add missing cgm timestamps
    indices_of_interest <- which(timediff_timestamps>=MinGap & timediff_timestamps<=MaxGap)

    #Add cgm timestamps in between min and max gap up at the Granularity level decided until there is not sufficient space in the time gap
    new_timestamps_smallgaps <- sapply(indices_of_interest, function (x)
      if (timediff_timestamps[x] %% Granularity !=0){
        seq(cgmtimestamps[x]+ Granularity, cgmtimestamps[x+1]-(timediff_timestamps[x] %% Granularity), Granularity)
      }else{
        seq(cgmtimestamps[x]+ Granularity, cgmtimestamps[x+1]- Granularity, Granularity)
      })


    timestamps_for_smallgaps <- c(unlist(new_timestamps_smallgaps))
    timestamps_for_smallgaps <- data.frame(timestamps_for_smallgaps)

    timestamps_for_smallgaps <- timestamps_for_smallgaps %>%
      dplyr::rename(cgm_timestamp = timestamps_for_smallgaps) %>%
      transform(cgm_timestamp = lubridate::as_datetime(cgm_timestamp)) %>%
      dplyr::arrange(cgm_timestamp)

    df <- dplyr::full_join(df,
                           timestamps_for_smallgaps,
                           by = c("cgm_timestamp" = "cgm_timestamp"))
    df <- dplyr::arrange(df, cgm_timestamp)

    if(Interpolate == TRUE){
      #If interpolation of glucose values is required, a linear interpolation will be conducted.
      df$glucose <-
        stats::approx(df$cgm_timestamp, df$glucose, df$cgm_timestamp, method = "linear")$y

    }

    indices_for_largegaps <- which(timediff_timestamps>MaxGap)
    #i.e. if participant has no gaps that are larger than MaxGaps than will not create further explicit gaps in their data
    if(pracma::isempty(indices_for_largegaps)){

      df <- df %>%
        dplyr::arrange(cgm_timestamp) %>%
        dplyr::mutate_if(is.numeric, round, digits = 2) %>%
        dplyr::mutate(
          id =  df$id[1],
          cgm_timestamp = lubridate::floor_date(cgm_timestamp, unit = "minutes")
        )

    } #i.e. if participant has gaps longer than MaxGap, the extra timestamps will be created and glucose values marked as NA
    else{



      new_timestamps_largegaps <- sapply(indices_for_largegaps, function (x)
        if (timediff_timestamps[x] %% Granularity !=0){
          seq(cgmtimestamps[x]+Granularity, cgmtimestamps[x+1]-(timediff_timestamps[x] %% Granularity), Granularity)
        }else{
          seq(cgmtimestamps[x]+Granularity, cgmtimestamps[x+1]-Granularity, Granularity)
        })

      timestamps_for_largegaps <- c(unlist(new_timestamps_largegaps))
      timestamps_for_largegaps <- data.frame(timestamps_for_largegaps)

      timestamps_for_largegaps <- timestamps_for_largegaps %>%
        dplyr::rename(cgm_timestamp = timestamps_for_largegaps) %>%
        transform(cgm_timestamp = lubridate::as_datetime(cgm_timestamp)) %>%
        dplyr::arrange(cgm_timestamp)

      df <- dplyr::full_join(df, timestamps_for_largegaps, by = "cgm_timestamp")

      df <- df %>%
        dplyr::arrange(cgm_timestamp) %>%
        dplyr::mutate_if(is.numeric, round, digits = 2) %>%
        dplyr::mutate(id = df$id[1],
                      cgm_timestamp = lubridate::floor_date(cgm_timestamp, unit = "minutes"))

      return(df)
    }

  }

  #### Splitting the DataFrame by id ####
  list_participants <- split(DataFrame, DataFrame$id)

  #### Calling the function for each data frame containing individual participants data ####
  list_data <- lapply(list_participants, fill)

  #### Row binding individual participants data in a single dataframe ####
  final_data <- dplyr::bind_rows(list_data, .id = "id")

  #### Return CGM data with explicit gaps and interpolated glucose if specified ####
  return(final_data = data.frame(final_data))


}

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
#' @param StudyID ID of participant for whom CGM data will be checked for missingness. This is only
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
#' hypometrics::cgmCheck(DataFrame)
#'
#' #Checking daily missingness in CGM data for a specific participant
#' hypometrics::cgmCheck(DataFrame,
#'                       CheckAll = FALSE,
#'                       StudyID = "001",
#'                       AxisLabels = c(0, 2.2, 3.9, 10, 20))
#' }
#'
#'@export
cgmCheck <- function(DataFrame,
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
                    median_glu = median, Q1_glu = `25`, Q3_glu = `75`, cv_glu = CV) %>%
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
#' @param AddSleepSummary A character object specifying whether a summary of SDH should be added according to
#' sleep status. Default is "no". Other option is "yes".
#'
#' @details This function goes hand in hand with the \code{\link[hypometrics]{sdhDetection}()} function. Once
#' the details of each individual hypoglycaemia episode has been produced, this function can be used to summarise
#' them. If an SDH summary by sleep status is required, the sdhDetection function must have been run with the
#' AddSleepStatus argument set to "yes".
#'
#' @return Data frame with one line per participant with key SDH metrics. This includes the number of episodes
#' during the day/night, mean duration of episodes, the number long SDH
#' and the number of days during which those occur.
#'
#' @examples
#' \dontrun{
#' hypometrics::sdhSummarise(DataFrame,
#'                           DetectionLimit = "3.0",
#'                           LongDuration = 120,
#'                           AddSleepSummary = "no")
#' }
#'
#'@export
sdhSummarise <- function(DataFrame,
                         DetectionLimit = "3.9",
                         LongDuration = 120,
                         AddSleepSummary = "no"){


  #### Check function arguments and columns included in dataset ####

  chk::check_names(DataFrame, names = c("id", "sdh_number", "sdh_night_status", "sdh_duration_mins", "sdh_interval"))

  chk::chk_character(DetectionLimit)

  chk::chk_numeric(LongDuration)

  chk::chk_character(AddSleepSummary)


  ############ Preparing the data ######
  if(AddSleepSummary == "no"){

    #Setting variable names
    varnames <- c("id",
                  paste0("n_sdh", DetectionLimit),
                  paste0("n_sdh", DetectionLimit, "_day"),
                  paste0("n_sdh", DetectionLimit, "_night"),
                  paste0("n_sdh", DetectionLimit, "_overlap_daynight"),
                  paste0("mean_duration_sdh", DetectionLimit),
                  paste0("mean_duration_sdh", DetectionLimit, "_day"),
                  paste0("mean_duration_sdh", DetectionLimit, "_night"),
                  paste0("mean_duration_sdh", DetectionLimit, "_overlap_daynight"))

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


  } else{
  #Setting variable names
  varnames <- c("id",
                paste0("n_sdh", DetectionLimit),
                paste0("n_sdh", DetectionLimit, "_day"),
                paste0("n_sdh", DetectionLimit, "_night"),
                paste0("n_sdh", DetectionLimit, "_overlap_daynight"),
                paste0("n_sdh", DetectionLimit, "_awake"),
                paste0("n_sdh", DetectionLimit, "_asleep"),
                paste0("n_sdh", DetectionLimit, "_overlap_awakeasleep"),
                paste0("n_sdh", DetectionLimit, "_sleepstatus_missing"),
                paste0("mean_duration_sdh", DetectionLimit),
                paste0("mean_duration_sdh", DetectionLimit, "_day"),
                paste0("mean_duration_sdh", DetectionLimit, "_night"),
                paste0("mean_duration_sdh", DetectionLimit, "_overlap_daynight"),
                paste0("mean_duration_sdh", DetectionLimit, "_awake"),
                paste0("mean_duration_sdh", DetectionLimit, "_asleep"),
                paste0("mean_duration_sdh", DetectionLimit, "_overlap_awakeasleep"))

  # SDH number and duration summary
  df_sdh <- DataFrame %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(dplyr::n(),
                     sum(sdh_night_status=="Day"),
                     sum(sdh_night_status=="Night"),
                     sum(sdh_night_status=="Overlap"),
                     sum(sdh_sleep_status=="Awake", na.rm=T),
                     sum(sdh_sleep_status=="Asleep", na.rm=T),
                     sum(sdh_sleep_status=="Overlap", na.rm=T),
                     sum(is.na(sdh_sleep_status)),
                     mean(sdh_duration_mins, na.rm = T),
                     mean(sdh_duration_mins[sdh_night_status=="Day"]),
                     mean(sdh_duration_mins[sdh_night_status=="Night"]),
                     mean(sdh_duration_mins[sdh_night_status=="Overlap"]),
                     mean(sdh_duration_mins[sdh_sleep_status=="Awake" & !is.na(sdh_sleep_status)]),
                     mean(sdh_duration_mins[sdh_sleep_status=="Asleep" & !is.na(sdh_sleep_status)]),
                     mean(sdh_duration_mins[sdh_sleep_status=="Overlap" & !is.na(sdh_sleep_status)])) %>%
    magrittr::set_names(varnames) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_if(is.numeric, round, digits = 1)
  }

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

#' @title Detection and Description of Episodes of Continuous Glucose Monitoring (CGM) Hypoglycaemia
#'
#' @description Function which summarises raw CGM data and produces an output which
#' includes key characteristics (e.g nadir, duration) of each CGM-detected hypoglycaemic episode.
#'
#' @param DataFrame A dataframe with CGM data in which hypoglycaemic episodes will be detected.
#' Must have columns id, cgm_timestamp, glucose.
#' @param DetectionLimit  Object of type numeric or integer corresponding to
#' the glucose value used to detect hypoglycaemia. Default is 3.9 mmol/L.
#' @param DetectionDuration Object of type numeric or integer corresponding to
#' the duration at or below the DetectionLimit for a hypolgycaemic episode to be detected.
#' Default is 15 (minutes).
#' @param AddSleepStatus A character object specifying whether sleep status (sleep or awake) should be added for
#' each SDH. Default is "no". Other option is "yes".
#'
#' @return A dataframe with one hypoglycaemic episode per row with characteristics in each column. If user has
#' CGM and sleep data available, the sleep status can be added for each episode.
#'
#' @examples
#' \dontrun{
#' hypometrics::sdhDetection(DataFrame,
#'                           DetectionLimit = 3,
#'                           DetectionDuration = 30,
#'                           AddSleepStatus = "no")
#' }
#'
#' @export
#'
sdhDetection <- function(DataFrame,
                         DetectionLimit = 3.9,
                         DetectionDuration = 15,
                         AddSleepStatus = "no"){

  #### Check function inputs and data frame columns ####
  chk::check_names(DataFrame, names = c("id", "cgm_timestamp", "glucose"))

  chk::chk_character(DataFrame$id)

  checkmate::assertPOSIXct(DataFrame$cgm_timestamp)

  chk::chk_numeric(DataFrame$glucose)

  chk::chk_numeric(DetectionLimit)

  chk::chk_numeric(DetectionDuration)

  chk::chk_character(AddSleepStatus)

  #### Function within the function which will detect hypos for each id ####
  detect <- function(df){
    #### Start data manipulation ####

    if(AddSleepStatus == "no"){
    #### Original CGM data in long format with added information on time of data and values below threshold####
    cgm_data_long <- df %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(time_of_day = ifelse(as.numeric(format(cgm_timestamp, "%H"))<6 & as.numeric(format(cgm_timestamp, "%H"))>=0, "Night", "Day")) %>%
      dplyr::select(id, time_of_day, cgm_timestamp, glucose) %>%
      dplyr::mutate(below_threshold = dplyr::case_when(glucose < DetectionLimit ~ 1,
                                                       glucose >= DetectionLimit ~ 0),
                    seq_event = data.table::rleid(below_threshold))
    } else{

      cgm_data_long <- df %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(time_of_day = ifelse(as.numeric(format(cgm_timestamp, "%H"))<6 & as.numeric(format(cgm_timestamp, "%H"))>=0, "Night", "Day")) %>%
        dplyr::select(id, time_of_day, cgm_timestamp, glucose, sleep_status) %>%
        dplyr::mutate(below_threshold = dplyr::case_when(glucose < DetectionLimit ~ 1,
                                                         glucose >= DetectionLimit ~ 0),
                      seq_event = data.table::rleid(below_threshold))

}
    #### Transform CGM data to wide format ####
    #Data frame displays the sequence of events i.e. below threshold, above threshold, NA... and how long for
    cgm_data_wide <- cgm_data_long %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(last_timestamp = data.table::last(cgm_timestamp)) %>%
      dplyr::group_by(id, seq_event, below_threshold,last_timestamp) %>%
      dplyr::summarise(start_seq = data.table::first(cgm_timestamp)) %>%
      dplyr::group_by(id, last_timestamp) %>%
      dplyr::mutate(end_seq = dplyr::lead(start_seq)) %>%
      transform(end_seq = dplyr::if_else(is.na(end_seq), last_timestamp, end_seq)) %>%
      dplyr::mutate(interval_seq = lubridate::as.interval(start_seq, end_seq),
                    duration_seq= lubridate::time_length(interval_seq, unit = "min")) %>%
      dplyr::select(-last_timestamp)


    #This will mark adjacent SDH (less than the detection duration apart)
    #as a single group (i.e. as a single event) where applicable based on Hypo-METRICS rules (please refer to manual)

    for (i in 1:nrow(cgm_data_wide)){

      if(isTRUE(cgm_data_wide$below_threshold[i]==1) & isTRUE(cgm_data_wide$duration_seq[i]>=DetectionDuration)){
        cgm_data_wide$sdh_grouped[i] <- 1

      } else if(isTRUE(cgm_data_wide$below_threshold[i-1]==1) & isTRUE(cgm_data_wide$duration_seq[i-1]>=DetectionDuration) & isTRUE(cgm_data_wide$below_threshold[i]==0) & isTRUE(cgm_data_wide$duration_seq[i]<=DetectionDuration) & isTRUE(cgm_data_wide$below_threshold[i+1]==1)){
        cgm_data_wide$sdh_grouped[i] <- 1


      } else if(isTRUE(cgm_data_wide$below_threshold[i]==1) & isTRUE(cgm_data_wide$duration_seq[i]<DetectionDuration) & isTRUE(cgm_data_wide$below_threshold[i-1]==0) & isTRUE(cgm_data_wide$duration_seq[i-1]<=DetectionDuration) & isTRUE(cgm_data_wide$below_threshold[i-2]==1) & isTRUE(cgm_data_wide$duration_seq[i-2]>=DetectionDuration)){
        cgm_data_wide$sdh_grouped[i] <- 1

      } else if(isTRUE(is.na(cgm_data_wide$below_threshold[i])=="TRUE")){
        cgm_data_wide$sdh_grouped[i] <- NA

      } else if(isTRUE(cgm_data_wide$below_threshold[i]==0) & isTRUE(cgm_data_wide$duration_seq[i]<=DetectionDuration) & isTRUE(cgm_data_wide$sdh_grouped[i-1]==1) & isTRUE(cgm_data_wide$sdh_grouped[i+1]==1)){
        cgm_data_wide$sdh_grouped[i] <-1

      } else if(isTRUE(cgm_data_wide$below_threshold[i]==0) & isTRUE(cgm_data_wide$duration_seq[i]<=DetectionDuration) & isTRUE(cgm_data_wide$sdh_grouped[i-1]==1) & isTRUE(cgm_data_wide$below_threshold[i+1]==1)) {
        cgm_data_wide$sdh_grouped[i] <-1

      } else if(isTRUE(cgm_data_wide$below_threshold[i]==1) & isTRUE(cgm_data_wide$duration_seq[i]<DetectionDuration) & isTRUE(cgm_data_wide$sdh_grouped[i-2]==1) & isTRUE(cgm_data_wide$below_threshold[i-1]==0) & isTRUE(cgm_data_wide$duration_seq[i-1]<=DetectionDuration)){
        cgm_data_wide$sdh_grouped[i] <-1

      } else{
        cgm_data_wide$sdh_grouped[i] <- 0
      }
    }


    #Return in a long format dataset, the information about whether or not SDH are eligible to be grouped (based on the sdh_grouped variable)
    combined_cgm <- dplyr::full_join(cgm_data_long,
                                     cgm_data_wide,
                                     by = c("id", "seq_event", "below_threshold"))

    #Add when there is an SDH, internal, duration and nadir information (dataset still in long format)
    combined_cgm <- combined_cgm %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(sdh_seq = ifelse(sdh_grouped==1, data.table::rleid(sdh_grouped), NA)) %>%
      dplyr::group_by(id, sdh_seq, sdh_grouped) %>%
      dplyr::mutate(sdh_interval = dplyr::if_else(!is.na(sdh_seq), #note if_else here, as opposed to ifelse will preserve the interval format and not coerce it to numeric
                                                  lubridate::as.interval(data.table::first(start_seq),
                                                                         data.table::last(end_seq)),
                                                  NA),
                    sdh_duration_mins = ifelse(!is.na(sdh_seq),
                                               lubridate::time_length(sdh_interval, unit = "mins"),
                                               NA),
                    sdh_nadir = ifelse(!is.na(sdh_seq), min(glucose), NA))


    #Add information as to whether there was a change in night status during a single SDH
    combined_cgm <- combined_cgm %>%
      dplyr::group_by(id, sdh_seq) %>%
      dplyr::mutate(timeofday_change_flag = dplyr::case_when(!is.na(sdh_seq) & time_of_day == data.table::first(time_of_day) ~ 0,
                                                             !is.na(sdh_seq) & time_of_day != dplyr::lag(time_of_day) ~ 1,
                                                             !is.na(sdh_seq) & time_of_day == dplyr::lag(time_of_day) ~ 0,
                                                             is.na(sdh_seq) ~ NA_real_))

    #Indicate whether SDH night status is overlap if group night status is 1
    combined_cgm <- combined_cgm %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(sdh_night_status = ifelse(!is.na(sdh_seq), time_of_day, NA)) %>%
      dplyr::group_by(id, sdh_seq) %>%
      dplyr::mutate(group_night_status = ifelse(!is.na(timeofday_change_flag), sum(timeofday_change_flag), NA)) %>%
      #essentially this will flag if no change in status then will be 0s if change in status the sum will be 1 for that sdh!
      base::transform(sdh_night_status = ifelse(group_night_status == 0, sdh_night_status, "Overlap"))

    if(AddSleepStatus == "no"){
   #return to a map format with one row per event
    sdh_map <- combined_cgm %>%
      dplyr::filter(sdh_grouped==1) %>%
      dplyr::group_by(id, sdh_seq, sdh_grouped, sdh_interval, sdh_duration_mins, sdh_nadir, sdh_night_status) %>%
      dplyr::summarise() %>%
      dplyr::ungroup() %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(sdh_number = data.table::rleid(sdh_seq)) %>%
      dplyr::select(id, sdh_number, sdh_interval, sdh_duration_mins, sdh_nadir, sdh_night_status) %>%
      dplyr::ungroup() %>%
      dplyr::mutate_if(is.numeric, round, digits = 1) %>%
      dplyr::mutate(sdh_night_status = as.character(sdh_night_status)) #to make sure of consistent variable type for row binding

    }else if(AddSleepStatus == "yes"){
      combined_cgm <- combined_cgm %>%
        dplyr::group_by(id, sdh_seq) %>%
        dplyr::mutate(sleepstatus_change_flag = dplyr::case_when(!is.na(sdh_seq) & sleep_status == data.table::first(sleep_status) ~ 0,
                                                          !is.na(sdh_seq) & sleep_status != dplyr::lag(sleep_status) ~ 1,
                                                          !is.na(sdh_seq) & sleep_status == dplyr::lag(sleep_status) ~ 0,
                                                          is.na(sdh_seq) ~ NA_real_))

      combined_cgm <- combined_cgm %>%
        dplyr::mutate(sdh_sleep_status = ifelse(!is.na(sdh_seq), sleep_status, NA)) %>%
        dplyr::group_by(sdh_seq) %>%
        dplyr::mutate(group_sleep_status = ifelse(!is.na(sleepstatus_change_flag), sum(sleepstatus_change_flag), NA)) %>%
        #essentially this will flag if no change in status then will be 0s if change in status the sum will be 1 for that sdh!
        transform(sdh_sleep_status = ifelse(group_sleep_status == 0, sdh_sleep_status, "Overlap"))

      sdh_map <- combined_cgm %>%
        dplyr::filter(sdh_grouped==1) %>%
        dplyr::group_by(id, sdh_seq, sdh_grouped, sdh_interval, sdh_duration_mins, sdh_nadir, sdh_night_status, sdh_sleep_status) %>%
        dplyr::summarise() %>%
        dplyr::ungroup() %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(sdh_number = data.table::rleid(sdh_seq)) %>%
        dplyr::select(id, sdh_number, sdh_interval, sdh_duration_mins, sdh_nadir, sdh_night_status, sdh_sleep_status) %>%
        dplyr::ungroup() %>%
        dplyr::mutate_if(is.numeric, round, digits = 1) #%>%
        #dplyr::mutate(sdh_night_status = as.character(sdh_night_status)) #to make sure of consistent variable type for row binding

 }



    #Return final output - 1 row per event per individual
    return(sdh_map = data.frame(sdh_map))

  }

  #### Splitting the DataFrame by id ####
  list_participants <- split(DataFrame, DataFrame$id)

  #### Calling the function for each data frame containing individual participants data ####
  list_map <- lapply(list_participants, detect)

  #### Row binding individual participants data in a single dataframe ####
  final_map <- dplyr::bind_rows(list_map, .id = "id")

  #### Return sdh map ####
  return(final_map = data.frame(final_map))

}

#' @title Visualise Glucose Data
#'
#' @description Plots Glucose Data Over Time
#'
#' @param DataFrame A dataframe of CGM data which will be filled and interpolated
#' where specified. Must contain columns: id, cgm_timestamp and glucose.
#' @param StudyID ID of participant for whom CGM data will be plotted.
#' @param TimeBreak Character object which defines whether plot outputs should be split
#' by time period. Default is "No" in which case there will be a single plot produce
#' including data for the whole period. Other options are "week" and "day" in which case multiple
#' plots will be produced according to TimeBreak.
#' @param PageNumber Vector indicating which page (i.e. week/day) number selected for
#' visualisation as plot is faceted according to TimeBreak.
#' @param AddSleep Character object for the user to specify whether the CGM plot will have added day/night
#' visualisation ("no" - default) or sleep/awake visualisation ("yes"). If set to "yes", the function requires a CGM dataset
#' with a sleep_status (Awake vs asleep) column as input
#'
#' @details
#' This functions plots CGM data over time with grey shaded area corresponding to the time period
#' between 00:00 and 06:00 as typically used to describe nocturnal hypoglycaemia. The function
#' offers the options to look at the  glucose data over the entire study period, for a specific week or
#' specific date. It also offers the option to plot data with corresponding sleep status (awake vs asleep)
#' if sleep tracker data is available. Where sleep data is missing, the area is coloured grey.
#'
#' @return A graphical representation showing glucose trace over time with shaded area
#' representing night time.
#'
#' @examples
#' \dontrun{
#' hypometrics::cgmVisualise(DataFrame,
#'                           StudyID = "001",
#'                           TimeBreak = "day",
#'                           PageNumber = 7,
#'                           AddSleep = "no")
#' }
#'
#' @export
cgmVisualise <- function(DataFrame,
                         StudyID,
                         TimeBreak = "no",
                         PageNumber = NA_real_,
                         AddSleep = "no"){


  #### Check function arguments and data frame columns ####
if(AddSleep=="no"){
  chk::check_names(DataFrame, names = c("id", "cgm_timestamp", "glucose"))
} else { chk::check_names(DataFrame, names = c("id", "cgm_timestamp", "glucose", "sleep_status")) }

  chk::chk_character(DataFrame$id)

  checkmate::assertPOSIXct(DataFrame$cgm_timestamp)

  chk::chk_numeric(DataFrame$glucose)

  if(AddSleep=="yes"){chk::chk_character(DataFrame$sleep_status)}

  chk::chk_character(TimeBreak)

  chk::chk_numeric(PageNumber)

  #### Data Prep ####
  if(AddSleep=="no"){
  cgm_data <- DataFrame %>%
    dplyr::filter(id == StudyID) %>%
    dplyr::mutate(time_of_day = ifelse(as.numeric(format(cgm_timestamp, "%H"))<6 & as.numeric(format(cgm_timestamp, "%H"))>=0, "Night", "Day"),
                  study_day =  match(as.Date(cgm_timestamp), unique(as.Date(cgm_timestamp))),
                  week_start = cut((as.Date(cgm_timestamp)),
                                   breaks = seq.Date(min((as.Date(cgm_timestamp))),
                                                     max((as.Date(cgm_timestamp))) + 7,
                                                     by = "week")),
                  study_week = match(as.Date(week_start), unique(as.Date(week_start))))


  } else if(AddSleep=="yes"){

    cgm_data <- DataFrame %>%
      dplyr::filter(id == StudyID) %>%
      dplyr::mutate(time_of_day = factor(sleep_status, levels = c("Awake", "Asleep")),
                    study_day =  match(as.Date(cgm_timestamp), unique(as.Date(cgm_timestamp))),
                    week_start = cut((as.Date(cgm_timestamp)),
                                     breaks = seq.Date(min((as.Date(cgm_timestamp))),
                                                       max((as.Date(cgm_timestamp))) + 7,
                                                       by = "week")),
                    study_week = match(as.Date(week_start), unique(as.Date(week_start))))


}
  #### Plot CGM data with Day vs Night or Sleep vs Awake status and Glucose thresholds ####
  if(TimeBreak == "no"){
    cgm_plot <-
      ggplot2::ggplot(cgm_data, ggplot2::aes(x=cgm_timestamp, y=glucose)) +
      ggplot2::geom_rect(ggplot2::aes(xmin = cgm_timestamp, xmax = dplyr::lead(cgm_timestamp), ymin = -Inf, ymax = Inf, fill=time_of_day), alpha=0.5) +
      ggplot2::scale_fill_manual(values = c("transparent", "darkgrey")) +
      ggplot2::geom_line(ggplot2::aes(y=glucose), color = "blue4", size=0.5) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 3.9), colour="green4", size=0.5) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 10), colour="green4", size=0.5) +
      ggplot2::ylab("Glucose concentration (mmol/L)") +
      ggplot2::xlab("Date (DD/MM/YY)") +
      ggplot2::scale_x_datetime(date_labels = "%d/%m/%y",
                                date_breaks = "1 week",
                                expand = c(0,0)) +
      ggplot2::scale_y_continuous(breaks = c(3, 3.9, 10, 15)) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position="none",
                     panel.grid.minor.y = ggplot2::element_blank(),
                     panel.grid.minor.x = ggplot2::element_blank(),
                     plot.margin = ggplot2::unit(c(0.25, 0.75, 0.25, 0.25), "cm"))

    return(cgm_plot)

  } else if(TimeBreak == "week"){

    cgm_plot <-
      ggplot2::ggplot(cgm_data, ggplot2::aes(x=cgm_timestamp, y=glucose)) +
      ggforce::facet_grid_paginate(~study_week, ncol=1, nrow=1, page = PageNumber, scales = "free") +
      ggplot2::geom_rect(ggplot2::aes(xmin = cgm_timestamp, xmax = dplyr::lead(cgm_timestamp), ymin = -Inf, ymax = Inf, fill=time_of_day), alpha=0.5) +
      ggplot2::scale_fill_manual(values = c("transparent", "darkgrey")) +
      ggplot2::geom_line(ggplot2::aes(y=glucose), color = "blue4", size=0.5) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 3.9), colour="green4", size=0.5) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 10), colour="green4", size=0.5) +
      ggplot2::ylab("Glucose concentration (mmol/L)") +
      ggplot2::xlab("Date (DD/MM/YY HH:MM)") +
      ggplot2::scale_x_datetime(date_labels = "%d/%m/%y \n %H:%M",
                                date_breaks = "1 day",
                                date_minor_breaks = "6 hours",
                                expand = c(0,0),
                                limits = c(lubridate::floor_date(dplyr::first(cgm_data$cgm_timestamp[cgm_data$study_week==PageNumber]), unit = "day"),
                                           lubridate::ceiling_date(dplyr::last(cgm_data$cgm_timestamp[cgm_data$study_week==PageNumber]), unit = "day"))) +
      ggplot2::scale_y_continuous(breaks = c(3, 3.9, 10, 15)) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position="none",
                     panel.grid.minor.y = ggplot2::element_blank(),
                     plot.margin = ggplot2::unit(c(0.25, 0.75, 0.25, 0.25), "cm"))

    return(cgm_plot)

  } else if(TimeBreak == "day"){

    cgm_plot <-
      ggplot2::ggplot(cgm_data, ggplot2::aes(x=cgm_timestamp, y=glucose)) +
      ggforce::facet_grid_paginate(~study_day, ncol=1, nrow=1, page = PageNumber, scales = "free") +
      ggplot2::geom_rect(ggplot2::aes(xmin = cgm_timestamp, xmax = dplyr::lead(cgm_timestamp), ymin = -Inf, ymax = Inf, fill=time_of_day), alpha=0.5) +
      ggplot2::scale_fill_manual(values = c("transparent", "darkgrey")) +
      ggplot2::geom_line(ggplot2::aes(y=glucose), color = "blue4", size=0.5) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 3.9), colour="green4", size=0.5) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 10), colour="green4", size=0.5) +
      ggplot2::ylab("Glucose concentration (mmol/L)") +
      ggplot2::xlab("Date (DD/MM/YY HH:MM)") +
      ggplot2::scale_x_datetime(date_labels = "%d/%m/%y \n %H:%M",
                                date_breaks = "6 hours",
                                expand = c(0,0),
                                limits = c(lubridate::floor_date(dplyr::first(cgm_data$cgm_timestamp[cgm_data$study_day==PageNumber]), unit = "day"),
                                           lubridate::ceiling_date(dplyr::last(cgm_data$cgm_timestamp[cgm_data$study_day==PageNumber]), unit = "day"))) +
      ggplot2::scale_y_continuous(breaks = c(3, 3.9, 10, 15)) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position="none",
                     panel.grid.minor.y = ggplot2::element_blank(),
                     panel.grid.minor.x = ggplot2::element_blank(),
                     plot.margin = ggplot2::unit(c(0.25, 0.75, 0.25, 0.25), "cm"))

    return(cgm_plot)
  }

}

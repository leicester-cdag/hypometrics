#' @title Categorise Sleep Information from Fitbit Raw Data
#'
#' @description Function creates 3 separate datasets according to Fitbit sleep data:
#' main sleeping periods, short wake periods and detailed sleep stages.
#'
#' @param DataFrame A dataframe containing Fitbit sleep data.
#'
#' @return Three dataframes: one including information on main sleeping periods,
#' another containing more detailed data regarding sleeping stages and the last one including
#' information on short wake periods occurring during the night.
#'
#' @examples
#'
#' \dontrun{
#' hypometrics::sleepCategorise(DataFrame)
#' }
#'
#' @export

sleepCategorise <- function(DataFrame){

  #### Check function inputs and data frame columns ####
  chk::check_names(DataFrame, names = c("id", "logId", "dateOfSleep", "startTime", "endTime",
                                        "levels.data", "levels.shortData"))

  chk::chk_character(DataFrame$id)

  #### Sleep stages ####
    sleep_stages <- DataFrame %>%
      tidyr::unnest(cols=c(levels.data)) %>%
      dplyr::select(id, logId, dateOfSleep, dateTime, level, seconds) %>%
      #Changing to right date or datetime formats
      transform(dateOfSleep = as.Date(dateOfSleep),
                dateTime = lubridate::ymd_hms(dateTime)) %>%
      #Arranging in chronological order
      dplyr::arrange(id, dateTime)

  #### Short wake periods during the night ####
    short_wake_periods <- DataFrame %>%
      dplyr::mutate_if(is.list, purrr::simplify_all) %>%  # https://stackoverflow.com/questions/38860380/unnesting-a-list-of-lists-in-a-data-frame-column
      tidyr::unnest(levels.shortData) %>%
      dplyr::select(id, logId, dateOfSleep, dateTime, level, seconds) %>%
    #Changing to right date or datetime formats
    transform(dateOfSleep = as.Date(dateOfSleep),
              dateTime = lubridate::ymd_hms(dateTime)) %>%
    #Arranging in chronological order
    dplyr::arrange(id, dateTime)

  #### Sleep summary ####
    sleep_overview <- DataFrame %>%
      dplyr::group_by(id) %>%
      #Removing more granular data on sleep stages and short wake periods
      dplyr::select(-levels.data, -levels.shortData) %>%
      #Arranging in chronological order
      dplyr::arrange(startTime) %>%
      #Changing to right date or datetime formats
      transform(dateOfSleep = as.Date(dateOfSleep),
                startTime = lubridate::ymd_hms(startTime),
                endTime = lubridate::ymd_hms(endTime)) %>%
      #Removing duplications if applicable
      dplyr::distinct(startTime, .keep_all = T) %>%
      dplyr::group_by(id) %>%
      #Adding row for each missing night where applicable
      tidyr::complete(dateOfSleep = seq(min(dateOfSleep), max(dateOfSleep), by = 1)) %>%
      #Fill in gaps in id as complete function creates a missing row in all variables
      tidyr::fill(id) %>%
      #Fill start time with last end time of sleep recorded +1sec
      transform(startTime = dplyr::case_when(is.na(startTime) & !is.na(dplyr::lag(logId)) ~ imputeTS::na_locf(as.numeric(endTime)+1), #This adds a start time for the gap in sleep data (filled with last end sleep time recorded +1sec)
                                             is.na(startTime) & is.na(dplyr::lag(logId)) ~ imputeTS::na_locf(as.numeric(endTime)+1), #this is in case there are multiple missing nights so if the previous row the logid is missing (i.e. previous night is missing) then it takes the last non missing value
                                             !is.na(startTime) ~ as.numeric(startTime)), #'if that night is not missing then it keeps the start time value*
                #Fill end time with the next start time of sleep recorded -1sec
                endTime = dplyr::case_when(is.na(endTime) & !is.na(dplyr::lag(logId)) ~ imputeTS::na_locf(as.numeric(startTime)-1, option = "nocb"),
                                           is.na(endTime) & is.na(dplyr::lag(logId)) ~ imputeTS::na_locf(as.numeric(startTime)-1, option = "nocb"),
                                           !is.na(endTime) ~ as.numeric(endTime))) %>%
    #Change the newly filled start and end time from numeric to POSIXct
      transform(startTime = as.POSIXct(startTime, origin = "1970-01-01", tz="UTC"),
                endTime = as.POSIXct(endTime, origin = "1970-01-01", tz="UTC")) %>%
      dplyr::select(id, everything()) %>%
      dplyr::ungroup()

  #### Return outputs ####
return(list(sleep_stages = data.frame(sleep_stages),
            short_wake_periods = data.frame(short_wake_periods),
            sleep_overview = data.frame(sleep_overview)))


}


#' @title Summarises Sleep Information from Fitbit Raw Data
#'
#' @description Creates a dataset containing summarised sleep data such as
#' mean time in bed, sleep duration and time awake
#'
#' @param DataFrame A dataframe containing Fitbit sleep data.
#'
#' @return A summarised dataset with average time in bed, asleep awake as well as
#' number of nights with or without missing sleep data for each participant
#'
#' @examples
#'
#' \dontrun{
#' hypometrics::sleepSummarise(DataFrame)
#' }
#'
#' @export

sleepSummarise <- function(DataFrame){

  #### Check function inputs and data frame columns ####
  chk::check_names(DataFrame, names = c("id", "logId", "dateOfSleep", "startTime", "endTime"))

  chk::chk_character(DataFrame$id)

  #### Prep raw data ####
  sleep_overview <- DataFrame %>%
    dplyr::group_by(id) %>%
    #Removing more granular data on sleep stages and short wake periods
    dplyr::select(-levels.data, -levels.shortData) %>%
    #Arranging in chronological order
    dplyr::arrange(startTime) %>%
    #Changing to right date or datetime formats
    transform(dateOfSleep = as.Date(dateOfSleep),
              startTime = lubridate::ymd_hms(startTime),
              endTime = lubridate::ymd_hms(endTime)) %>%
    #Removing duplications if applicable
    dplyr::distinct(startTime, .keep_all = T) %>%
    dplyr::group_by(id) %>%
    #Adding row for each missing night where applicable
    tidyr::complete(dateOfSleep = seq(min(dateOfSleep), max(dateOfSleep), by = 1)) %>%
    #Fill in gaps in id as complete function creates a missing row in all variables
    tidyr::fill(id) %>%
    #Fill start time with last end time of sleep recorded +1sec
    transform(startTime = dplyr::case_when(is.na(startTime) & !is.na(dplyr::lag(logId)) ~ imputeTS::na_locf(as.numeric(endTime)+1), #This adds a start time for the gap in sleep data (filled with last end sleep time recorded +1sec)
                                           is.na(startTime) & is.na(dplyr::lag(logId)) ~ imputeTS::na_locf(as.numeric(endTime)+1), #this is in case there are multiple missing nights so if the previous row the logid is missing (i.e. previous night is missing) then it takes the last non missing value
                                           !is.na(startTime) ~ as.numeric(startTime)), #'if that night is not missing then it keeps the start time value*
              #Fill end time with the next start time of sleep recorded -1sec
              endTime = dplyr::case_when(is.na(endTime) & !is.na(dplyr::lag(logId)) ~ imputeTS::na_locf(as.numeric(startTime)-1, option = "nocb"),
                                         is.na(endTime) & is.na(dplyr::lag(logId)) ~ imputeTS::na_locf(as.numeric(startTime)-1, option = "nocb"),
                                         !is.na(endTime) ~ as.numeric(endTime))) %>%
    #Change the newly filled start and end time from numeric to POSIXct
    transform(startTime = as.POSIXct(startTime, origin = "1970-01-01", tz="UTC"),
              endTime = as.POSIXct(endTime, origin = "1970-01-01", tz="UTC")) %>%
    dplyr::select(id, everything()) %>%
    dplyr::ungroup()

  #### Produce summary data ####
  sleep_summary <- sleep_overview %>%
    dplyr::group_by(id) %>%
    #Counting number of nights where sleep data is missing
    dplyr::mutate(n_nights_missing = sum(is.na(logId))) %>%
    #Removing missing nights to summarise data for nights where there is sleep data
    dplyr::filter(!is.na(logId)) %>%
    dplyr::group_by(id, n_nights_missing) %>%
    dplyr::summarise(n_nights_with_sleep_data = dplyr::n_distinct(dateOfSleep),
                     average_time_in_bed_hours = mean(timeInBed/60),
                     average_time_asleep_hours = mean(minutesAsleep/60),
                     average_time_awake_hours = mean(minutesAwake/60)) %>%
    dplyr::mutate_if(is.numeric, round, digits=1) %>%
    dplyr::ungroup()

  #### Return output of interest ####
  return(sleep_summary)

}

#' @title Visualise Sleeping Patterns
#'
#' @description Creates histograms showin the distribution of times when
#' participant(s) went to bed and got up
#'
#' @param DataFrame A dataframe containing Fitbit sleep data.
#' @param VisualiseAll Logical string (TRUE/FALSE) which determines whether histograms will be plotted for all participants
#' or a selected participant. Default is TRUE.
#' @param StudyID ID of participant for whom sleep patterns will be plotted. This is only
#' relevant if VisualiseAll = FALSE, and will produce individualised graphs.
#'
#' @return Two histogram plots displaying all times recorded by the Fitbit
#' when participant(s) went to bed and got up.
#'
#' @examples
#'
#' \dontrun{
#' hypometrics::sleepVisualise(DataFrame,
#'                             VisualiseAll = TRUE,
#'                             StudyID = "")
#' }
#'
#' @export

sleepVisualise <- function(DataFrame,
                           VisualiseAll = TRUE,
                           StudyID = ""){

  #### Check function inputs and data frame columns ####
  chk::check_names(DataFrame, names = c("id", "logId", "dateOfSleep", "startTime", "endTime"))

  chk::chk_character(DataFrame$id)

  chk::chk_flag(VisualiseAll)

  chk::chk_character(StudyID)

  #### Prep raw data ####

  sleep_overview <- DataFrame %>%
    dplyr::group_by(id) %>%
    #Removing more granular data on sleep stages and short wake periods
    dplyr::select(-levels.data, -levels.shortData) %>%
    #Arranging in chronological order
    dplyr::arrange(startTime) %>%
    #Changing to right date or datetime formats
    transform(dateOfSleep = as.Date(dateOfSleep),
              startTime = lubridate::ymd_hms(startTime),
              endTime = lubridate::ymd_hms(endTime)) %>%
    #Removing duplications if applicable
    dplyr::distinct(startTime, .keep_all = T) %>%
    dplyr::group_by(id) %>%
    #Adding row for each missing night where applicable
    tidyr::complete(dateOfSleep = seq(min(dateOfSleep), max(dateOfSleep), by = 1)) %>%
    #Fill in gaps in id as complete function creates a missing row in all variables
    tidyr::fill(id) %>%
    #Fill start time with last end time of sleep recorded +1sec
    transform(startTime = dplyr::case_when(is.na(startTime) & !is.na(dplyr::lag(logId)) ~ imputeTS::na_locf(as.numeric(endTime)+1), #This adds a start time for the gap in sleep data (filled with last end sleep time recorded +1sec)
                                           is.na(startTime) & is.na(dplyr::lag(logId)) ~ imputeTS::na_locf(as.numeric(endTime)+1), #this is in case there are multiple missing nights so if the previous row the logid is missing (i.e. previous night is missing) then it takes the last non missing value
                                           !is.na(startTime) ~ as.numeric(startTime)), #'if that night is not missing then it keeps the start time value*
              #Fill end time with the next start time of sleep recorded -1sec
              endTime = dplyr::case_when(is.na(endTime) & !is.na(dplyr::lag(logId)) ~ imputeTS::na_locf(as.numeric(startTime)-1, option = "nocb"),
                                         is.na(endTime) & is.na(dplyr::lag(logId)) ~ imputeTS::na_locf(as.numeric(startTime)-1, option = "nocb"),
                                         !is.na(endTime) ~ as.numeric(endTime))) %>%
    #Change the newly filled start and end time from numeric to POSIXct
    transform(startTime = as.POSIXct(startTime, origin = "1970-01-01", tz="UTC"),
              endTime = as.POSIXct(endTime, origin = "1970-01-01", tz="UTC")) %>%
    dplyr::select(id, everything()) %>%
    dplyr::ungroup()

  #### Visualise sleeping times for all participants ####

  if(VisualiseAll == TRUE){

    go_bed_plot <- sleep_overview %>%
      dplyr::mutate(time_go_to_bed = hms::as_hms(startTime)) %>%
      #Shifting by 24hours so that the plot starts at 12:00pm and ends at 12:00 the next day -
      #renders a better visual rather than the peak of data being split due to plot ending at 23:59
      dplyr::mutate(time_go_to_bed_shifted = dplyr::if_else(time_go_to_bed >= hms::as_hms("00:00:00") & time_go_to_bed < hms::as_hms("12:00:00"),
                                                            time_go_to_bed + chron::times(86400),
                                                            time_go_to_bed)) %>%
      ggplot2::ggplot(ggplot2::aes(x=time_go_to_bed_shifted)) +
      ggplot2::geom_histogram(color="blue", fill="blue", alpha=0.6, position = 'identity', bins = 80) +
      ggplot2::scale_x_time(labels = c("12:00", "16:00", "20:00", "00:00", "04:00", "08:00", "12:00"),
                            expand = c(0,0)) +
      ggplot2::scale_y_continuous(expand = c(0,0)) +
      ggplot2::theme_bw() +
      ggplot2::ylab("Count")+
      ggplot2::xlab("Time when going to bed (HH:MM)") +
      ggplot2::theme(axis.text = ggplot2::element_text(size=10),
                     axis.title = ggplot2::element_text(face = "bold", size = 11),
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size=10))

    get_up_plot <- sleep_overview %>%
      dplyr::mutate(time_get_up = hms::as_hms(endTime)) %>%
      ggplot2::ggplot(ggplot2::aes(x=time_get_up)) +
      ggplot2::geom_histogram(color="darkgreen", fill="darkgreen", alpha=0.6, position = 'identity', bins = 80) +
      ggplot2::scale_x_time(labels = c("00:00", "04:00", "08:00", "12:00", "16:00", "20:00", "00:00"),
                            expand = c(0,0)) +
      ggplot2::scale_y_continuous(expand = c(0,0)) +
      ggplot2::theme_bw() +
      ggplot2::ylab("Count")+
      ggplot2::xlab("Time when getting up (HH:MM)") +
      ggplot2::theme(axis.text = ggplot2::element_text(size=10),
                     axis.title = ggplot2::element_text(face = "bold", size = 11),
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size=10))


    combined_plot <- ggpubr::ggarrange(go_bed_plot, get_up_plot, ncol=2)

    #### Return plots ####
    return(combined_plot)

  } else{

    #### Visualise sleeping times for a selected participant ####

    sleep_overview <- sleep_overview %>%
      dplyr::filter(id == StudyID)

    go_bed_plot <- sleep_overview %>%
      dplyr::mutate(time_go_to_bed = hms::as_hms(startTime)) %>%
      #Shifting by 24hours so that the plot starts at 12:00pm and ends at 12:00 the next day -
      #renders a better visual rather than the peak of data being split due to plot ending at 23:59
      dplyr::mutate(time_go_to_bed_shifted = dplyr::if_else(time_go_to_bed >= hms::as_hms("00:00:00") & time_go_to_bed < hms::as_hms("12:00:00"),
                                                            time_go_to_bed + chron::times(86400),
                                                            time_go_to_bed)) %>%
      ggplot2::ggplot(ggplot2::aes(x=time_go_to_bed_shifted)) +
      ggplot2::geom_histogram(color="blue", fill="blue", alpha=0.6, position = 'identity', bins = 80) +
      ggplot2::scale_x_time(breaks = c(lubridate::hms("12:00:00"), lubridate::hms("16:00:00"), lubridate::hms("20:00:00"),
                                       lubridate::hms("24:00:00"), lubridate::hms("28:00:00"),
                                       lubridate::hms("32:00:00"), lubridate::hms("36:00:00")),
                            labels = c("12:00", "16:00", "20:00", "00:00", "04:00", "08:00", "12:00"),
                            expand = c(0,0)
                            ) +
      ggplot2::scale_y_continuous(expand = c(0,0)) +
      ggplot2::theme_bw() +
      ggplot2::ylab("Count")+
      ggplot2::xlab("Time when going to bed (HH:MM)") +
      ggplot2::theme(axis.text = ggplot2::element_text(size=10),
                     axis.title = ggplot2::element_text(face = "bold", size = 11),
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size=10))

    get_up_plot <- sleep_overview %>%
      dplyr::mutate(time_get_up = hms::as_hms(endTime)) %>%
      ggplot2::ggplot(ggplot2::aes(x=time_get_up)) +
      ggplot2::geom_histogram(color="darkgreen", fill="darkgreen", alpha=0.6, position = 'identity', bins = 80) +
      ggplot2::scale_x_time(breaks = c(lubridate::hms("00:00:00"), lubridate::hms("04:00:00"), lubridate::hms("08:00:00"),
                                       lubridate::hms("12:00:00"), lubridate::hms("16:00:00"),
                                       lubridate::hms("20:00:00")),
                            labels = c("00:00", "04:00", "08:00", "12:00", "16:00", "20:00"),
                            expand = c(0,0)) +
      ggplot2::scale_y_continuous(expand = c(0,0)) +
      ggplot2::theme_bw() +
      ggplot2::ylab("Count")+
      ggplot2::xlab("Time when getting up (HH:MM)") +
      ggplot2::theme(axis.text = ggplot2::element_text(size=10),
                     axis.title = ggplot2::element_text(face = "bold", size = 11),
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size=10))


    combined_plot <- ggpubr::ggarrange(go_bed_plot, get_up_plot, ncol=2)

    #### Return plots ####
    return(combined_plot)


  }

}

#' @title Links Continuous Glucose Monitoring (CGM) Data with Sleep Data
#'
#' @description Creates a dataset containing CGM data matched with sleep data:
#' for each CGM timestamp, sleep status is available
#'
#' @param CgmDataFrame A dataframe containing CGM data. Must have columns id, cgm_timestamp, glucose.
#' @param SleepDataFrame A dataframe containing Fitbit sleep data.
#'
#' @return A dataset with every cgm timestamp marked as
#' either asleep (timestamp fell within sleep start and and time),
#' awake (timestamp was outside sleep interval) or
#' NA (no sleep information available at the time)
#'
#'
#' @examples
#'
#' \dontrun{
#' hypometrics::cgmsleepLink(CgmDataFrame = cgm,
#'                           SleepDataFrame)
#' }
#'
#' @export

cgmsleepLink <- function(CgmDataFrame,
                         SleepDataFrame){

  #### Check function inputs and data frame columns ####
  chk::check_names(CgmDataFrame, names = c("id", "cgm_timestamp", "glucose"))

  chk::chk_character(CgmDataFrame$id)

  checkmate::assertPOSIXct(CgmDataFrame$cgm_timestamp)

  chk::chk_numeric(CgmDataFrame$glucose)

  chk::check_names(SleepDataFrame, names = c("id", "logId", "dateOfSleep", "startTime", "endTime"))

  chk::chk_character(SleepDataFrame$id)

  #### Function within the function so it can be called at the participant level ####

  cgm_sleep_link <- function(cgm_dataset, sleep_dataset){

    #### Prep raw sleep data ####
    sleep_overview <- sleep_dataset %>%
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
      dplyr::select(id, dplyr::everything()) %>%
      dplyr::mutate(sleep_status = ifelse(!is.na(logId), "Asleep", NA_character_))

     #creating a vector of the length of CGM timestamps to determine which ones fall within a sleep interval
    vec <- cgm_dataset$cgm_timestamp

    #finding which sleep intervals that each cgm timestamp belongs to
    combine <- data.table::setDT(sleep_overview)[data.table::data.table(vec), on = .(startTime <= vec, endTime >= vec)] %>%
      #Result is a df of the same length as the vector (i.e. as the cgm dataframe with sleep data on the rows where cgm timestamps fall within the sleep intervals - it will be NA otherwise)
      #Selecting variables of interest
      dplyr::select(id, dateOfSleep, logId, startTime, sleep_status) %>%
      #Renaming as Start Time and end Time here will be identical and both correspond to CGM timestamps
      dplyr::rename(cgm_timestamp = startTime) %>%
      #Removing extra rows when there are multiple missing nights in a row as it creates duplicates in the CGM_Timestamp(one for each missing night in a row)
      dplyr::distinct(cgm_timestamp, .keep_all = T) %>%
      transform(sleep_status = ifelse(cgm_timestamp >= dplyr::first(sleep_overview$startTime) & cgm_timestamp <= dplyr::last(sleep_overview$endTime) & is.na(dateOfSleep) & is.na(logId),
                                      "Awake",
                                      sleep_status)) %>%
      #Fill in gaps in id as the ID will be NA on the rows where the CGM timestamp is not falling within a sleep interval
      tidyr::fill(id) %>%
      dplyr::select(id, cgm_timestamp, sleep_status)

      #Returning the data frame with sleep status to the original cgm dataframe
      cgm_dataset <- dplyr::left_join(cgm_dataset, combine) %>%
        dplyr::select(id, sleep_status, dplyr::everything()) %>%
        dplyr::ungroup()

}

  #### Splitting the CGM and sleep DataFrames by id (returns list object) ####
  cgm_dataset <- split(CgmDataFrame, CgmDataFrame$id)
  sleep_dataset <- split(SleepDataFrame, SleepDataFrame$id)

  #### Calling the function for each object (i.e. id) of the list of data frames ####
  list_linked <- mapply(cgm_sleep_link, cgm_dataset, sleep_dataset, SIMPLIFY = F)

  #### Row binding individual participants data in a single dataframe ####
  final_linked_df <- dplyr::bind_rows(list_linked, .id = "id")

  #### Return output ####
  return(final_linked_df)

}

#' @title Links Continuous Glucose Monitoring (CGM) Data with Activity Data
#'
#' @description Creates a dataset containing CGM data matched with activity data:
#' for each CGM timestamp, mean step count and/or heart rate is available
#'
#' @param CgmDataFrame A dataframe containing CGM data. Must have columns id, cgm_timestamp, glucose.
#' @param ActivityDataFrame A dataframe containing Fitbit step count or heart rate data.
#' @param DataType Character object determining which data to process. 2 options available: "stepcount"
#' or "heartrate".
#'
#' @return A dataset where for each CGM timestamp there is a corresponding step count or mean heart rate.
#'
#' @examples
#'
#' \dontrun{
#' hypometrics::cgmactivityLink(CgmDataFrame,
#'                              ActivityDataFrame,
#'                              DataType)
#' }
#'
#' @export

cgmactivityLink <- function(CgmDataFrame,
                            ActivityDataFrame,
                            DataType){



  #### Check function inputs and CGM data frame columns ####

  if(!DataType %in% c("stepcount", "heartrate")){
    stop("DataType must be `stepcount` or `heartrate` only.")
  }

  chk::check_names(CgmDataFrame, names = c("id", "cgm_timestamp", "glucose"))

  chk::chk_character(CgmDataFrame$id)

  checkmate::assertPOSIXct(CgmDataFrame$cgm_timestamp)

  chk::chk_numeric(CgmDataFrame$glucose)


  #### Define lubridate::within() function ####
  `%within%` <- lubridate::`%within%`

  #### Function within the function so it can be called at the participant level ####

  cgm_activity_link <- function(cgm_dataset, activity_dataset){

    #Defining CGM timestamp interval to later be able to calculate mean step count/HR
    #for each interval
    cgm <- cgm_dataset %>%
      dplyr::mutate(interval_number = data.table::rleid(cgm_timestamp),
                    time_interval = lubridate::interval(cgm_timestamp, dplyr::lead(cgm_timestamp)))

    ##Formatting activity data
    if(DataType == "stepcount"){

      #### Check data frame columns #####

      chk::check_names(ActivityDataFrame, names = c("id", "step_timestamp", "count"))

      chk::chk_character(ActivityDataFrame$id)

      checkmate::assertPOSIXct(ActivityDataFrame$step_timestamp)

      chk::chk_numeric(ActivityDataFrame$count)

      #### Start processing data ####

      activity <- activity_dataset %>%
        #filtering specifically to match the CGM dataframe time window
        dplyr::filter(step_timestamp %within% lubridate::interval(dplyr::first(cgm$cgm_timestamp),
                                                                  dplyr::last(cgm$cgm_timestamp))) %>%
        #Identify the missing minutes of step count data
        tidyr::complete(step_timestamp = seq(min(step_timestamp), max(step_timestamp), by=60)) %>%
        #Fill in gaps in id as complete function creates a missing row in all variables
        tidyr::fill(id) %>%
        dplyr::select(id, dplyr::everything())

      #Look through every single step timestamp to determine which cgm timestamp interval it falls within - which will help for my merging
      activity$interval_number <- findInterval(activity$step_timestamp, cgm$cgm_timestamp)

      #Create a summary steps dataset by interval of cgm timestamp
      steps_summary <- activity %>%
        dplyr::group_by(id, interval_number) %>%
        #groups by interval number (i.e. between two cgm timestamps) and calculates mean number of steps
        dplyr::summarise(step_count = sum(count, na.rm=T)#,
                         #this will highlight number of fitbit steps timestamps that fall within the cgm interval
                         #n_step_timestamps_in_int = dplyr::n(),
                         #this will highlight the number of missing step data i.e. timestamp with no step data that fall within the interval
                         #n_missing_min_steps_by_int = sum(is.na(count))
                         )

      ##Combining cgm and step count data in one data frame
        cgm_step <- dplyr::left_join(cgm, steps_summary) %>%
          dplyr::select(-interval_number, -time_interval)

    }else if(DataType == "heartrate"){

      #### Check data frame columns ####

      chk::check_names(ActivityDataFrame, names = c("id", "hr_timestamp", "heart_rate"))

      chk::chk_character(ActivityDataFrame$id)

      checkmate::assertPOSIXct(ActivityDataFrame$hr_timestamp)

      chk::chk_numeric(ActivityDataFrame$heart_rate)

      #### Formatting heart rate data ####
      hr <- activity_dataset %>%
        dplyr::filter(hr_timestamp %within% lubridate::interval(dplyr::first(cgm$cgm_timestamp),
                                                                dplyr::last(cgm$cgm_timestamp))) %>%
        #reformatting so I get a min-by-min average similarly to the step count data
        dplyr::group_by(id, hr_timestamp = format(hr_timestamp, "%Y-%m-%d %H:%M")) %>%
        dplyr::summarise(heart_rate = mean(heart_rate)) %>%
        transform(heart_rate = round(heart_rate, digits=0),
                  hr_timestamp = lubridate::ymd_hm(hr_timestamp)) %>%
        #Identify the missing minutes of heart rate data
        tidyr::complete(hr_timestamp = seq(min(hr_timestamp), max(hr_timestamp), by=60)) %>%
        #Fill in gaps in id as complete function creates a missing row in all variables
        tidyr::fill(id) %>%
        dplyr::select(id, dplyr::everything())

      ##Look through every single hr timestamp to determine which cgm timestamp interval it falls within - which will help for merging later on
      hr$interval_number <- findInterval(hr$hr_timestamp, cgm$cgm_timestamp)

      ##Creating a smaller heart rate dataframe with summarised values by interval which will be used to merge with the reference cgm dataframe
       hr_summary <- hr %>%
         dplyr::group_by(id, interval_number) %>%
         dplyr::summarise(heart_rate = mean(heart_rate, na.rm=T)#,
                          #n_hr_timestamps_in_int = dplyr::n(),
                          #n_missing_min_hr_by_int = sum(is.na(heart_rate))
                          ) %>%
         transform(heart_rate = round(heart_rate, digits=0),
                   interval_number = as.numeric(interval_number))

      ##Combining cgm and hr data in one data frame
        cgm_hr <- dplyr::left_join(cgm, hr_summary) %>%
          dplyr::select(-interval_number, -time_interval)

    }

  }

  #### Splitting the CGM and activity DataFrames by id - produces a list of datasets split according to id####
  cgm_dataset <- split(CgmDataFrame, CgmDataFrame$id)
  activity_dataset <- split(ActivityDataFrame, ActivityDataFrame$id)

  #### Calling the function for each element (i.e. ID) of the list of cgm and activity datasets ####
  list_linked <- mapply(cgm_activity_link, cgm_dataset, activity_dataset, SIMPLIFY = F)

  #### Row binding individual participants data in a single dataframe ####
  final_linked_df <- dplyr::bind_rows(list_linked, .id = "id")

  #### Return output ####
  return(final_linked_df)

}

#' @title Visualise Glucose linked with Activity Data
#'
#' @description Plots Glucose with Activity Data Over Time
#'
#' @param DataFrame A dataframe of CGM data linked with activity data. Output of the \link[hypometrics]{cgmactivityLink}
#' function.
#' @param StudyID ID of participant for whom CGM data will be plotted.
#' @param TimeBreak Character object which defines whether plot outputs should be split
#' by time period. Default is "No" in which case there will be a single plot produce
#' including data for the whole period. Other options are "week" and "day" in which case multiple
#' plots will be produced according to TimeBreak.
#' @param PageNumber Vector indicating which page (i.e. week/day) number selected for
#' visualisation as plot is faceted according to TimeBreak.
#' @param AddSleep Character object for the user to specify whether the CGM plot will have added day/night
#' visualisation ("no" - default) or sleep/awake visualisation ("yes"). If set to "yes", the function requires a CGM dataset
#' with a sleep_status (Awake vs asleep) column as input - this can be achieved by running the \link[hypometrics]{cgmsleepLink}
#' followed by the \link[hypometrics]{cgmactivityLink} function.
#' @param DataType Character object determining which activity data to process. 2 options available: "stepcount"
#' or "heartrate".
#'
#' @details
#' This functions plots CGM with activity data (either step count or heart rate) over time with light grey shaded area
#' corresponding to the time period between 00:00 and 06:00 as typically used to describe nocturnal hypoglycaemia.
#' The function offers the options to look at the  glucose data over the entire study period, for a specific week or
#' specific date. It also offers the option to plot data with corresponding sleep status (sleep periods in light grey, awake periods
#' in white) if sleep tracker data is available. Where sleep data is missing, the area is dark grey.
#'
#' @return A graphical representation showing glucose trace with step count or heart rate over time with shaded area
#' representing night or sleep time.
#'
#' @examples
#' \dontrun{
#' hypometrics::cgmactivityVisualise(DataFrame,
#'                                   StudyID = "001",
#'                                   TimeBreak = "day",
#'                                   PageNumber = 7,
#'                                   AddSleep = "no",
#'                                   DataType = "activity")
#' }
#'
#' @export
cgmactivityVisualise <- function(DataFrame,
                                 StudyID,
                                 TimeBreak = "no",
                                 PageNumber = NA_real_,
                                 AddSleep = "no",
                                 DataType){


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

  if(!DataType %in% c("stepcount", "heartrate")){
    stop("DataType must be `stepcount` or `heartrate` only.")
  }

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
      ggplot2::scale_fill_manual(values = c("transparent", "grey")) +
      ggplot2::geom_line(ggplot2::aes(y=glucose), color = "blue4", size=0.5) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 3.9), colour="green4", size=0.5) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 10), colour="green4", size=0.5) +
      ggplot2::ylab("Glucose concentration (mmol/L)") +
      #ggplot2::xlab("Date (DD/MM/YY)") +
      ggplot2::scale_x_datetime(date_labels = "%d/%m/%y",
                                date_breaks = "1 week",
                                expand = c(0,0)) +
      ggplot2::scale_y_continuous(breaks = c(3, 3.9, 10, 15)) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position="none",
                     axis.title.x = ggplot2::element_blank(),
                     panel.grid.minor.y = ggplot2::element_blank(),
                     panel.grid.minor.x = ggplot2::element_blank(),

                     plot.margin = ggplot2::unit(c(0.25, 0.75, 0.25, 0.25), "cm"))

    if(DataType == "stepcount"){
    activity_plot <-
      ggplot2::ggplot(cgm_data, ggplot2::aes(x=cgm_timestamp, y=step_count)) +
      ggplot2::geom_rect(ggplot2::aes(xmin = cgm_timestamp, xmax = dplyr::lead(cgm_timestamp), ymin = -Inf, ymax = Inf, fill=time_of_day), alpha=0.5) +
      ggplot2::scale_fill_manual(values = c("transparent", "grey")) +
      ggplot2::geom_bar(stat="identity", fill="cadetblue", width = 75) +
      ggplot2::ylab("Step count (n/min)") +
      ggplot2::xlab("Date (DD/MM/YY)") +
      ggplot2::scale_x_datetime(date_labels = "%d/%m/%y",
                                date_breaks = "1 week",
                                expand = c(0,0)) +
      ggplot2::scale_y_continuous(breaks = c(50, 100, 150, 200),
                                  expand = c(0,0)) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position="none",
                     panel.grid.minor.y = ggplot2::element_blank(),
                     panel.grid.minor.x = ggplot2::element_blank(),
                     plot.margin = ggplot2::unit(c(0.25, 0.75, 0.25, 0.25), "cm"))

    combined_plot <- ggpubr::ggarrange(cgm_plot, activity_plot,
                                        heights = c(1, 0.5),
                                        ncol=1,
                                        nrow=2, align="v", common.legend = F)

    } else if(DataType == "heartrate"){

      hr_plot <- ggplot2::ggplot(cgm_data, ggplot2::aes(x=cgm_timestamp, y=heart_rate)) +
        ggplot2::geom_rect(ggplot2::aes(xmin = cgm_timestamp, xmax = dplyr::lead(cgm_timestamp), ymin = -Inf, ymax = Inf, fill=time_of_day), alpha=0.5) +
        ggplot2::scale_fill_manual(values = c("transparent", "grey")) +
        ggplot2::geom_line(ggplot2::aes(y=heart_rate), colour = "darkred") +
        ggplot2::ylab("Heart rate (bpm)") +
        ggplot2::xlab("Date (DD/MM/YY)") +
        ggplot2::scale_x_datetime(date_labels = "%d/%m/%y",
                                  date_breaks = "1 week",
                                  expand = c(0,0)) +
        ggplot2::scale_y_continuous(breaks = c(50, 100, 150),
                                    expand=c(0,0)) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position="none",
                       panel.grid.minor.y = ggplot2::element_blank(),
                       panel.grid.minor.x = ggplot2::element_blank(),
                       plot.margin = ggplot2::unit(c(0.25, 0.75, 0.25, 0.25), "cm"))

      combined_plot <- ggpubr::ggarrange(cgm_plot, hr_plot,
                                         heights = c(1, 0.5),
                                         ncol=1,
                                         nrow=2, align="v", common.legend = F)

    }

    return(combined_plot)

  } else if(TimeBreak == "week"){

    cgm_plot <-
      ggplot2::ggplot(cgm_data, ggplot2::aes(x=cgm_timestamp, y=glucose)) +
      ggforce::facet_grid_paginate(~study_week, ncol=1, nrow=1, page = PageNumber, scales = "free") +
      ggplot2::geom_rect(ggplot2::aes(xmin = cgm_timestamp, xmax = dplyr::lead(cgm_timestamp), ymin = -Inf, ymax = Inf, fill=time_of_day), alpha=0.5) +
      ggplot2::scale_fill_manual(values = c("transparent", "grey")) +
      ggplot2::geom_line(ggplot2::aes(y=glucose), color = "blue4", size=0.5) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 3.9), colour="green4", size=0.5) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 10), colour="green4", size=0.5) +
      ggplot2::ylab("Glucose concentration (mmol/L)") +
      #ggplot2::xlab("Date (DD/MM/YY HH:MM)") +
      ggplot2::scale_x_datetime(date_labels = "%d/%m/%y \n %H:%M",
                                date_breaks = "1 day",
                                date_minor_breaks = "6 hours",
                                expand = c(0,0),
                                limits = c(lubridate::floor_date(dplyr::first(cgm_data$cgm_timestamp[cgm_data$study_week==PageNumber]), unit = "day"),
                                           lubridate::ceiling_date(dplyr::last(cgm_data$cgm_timestamp[cgm_data$study_week==PageNumber]), unit = "day"))) +
      ggplot2::scale_y_continuous(breaks = c(3, 3.9, 10, 15)) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position="none",
                     axis.title.x = ggplot2::element_blank(),
                     panel.grid.minor.y = ggplot2::element_blank(),
                     plot.margin = ggplot2::unit(c(0.25, 0.75, 0.25, 0.25), "cm"))

    if(DataType == "stepcount"){
    activity_plot <-
      ggplot2::ggplot(cgm_data, ggplot2::aes(x=cgm_timestamp, y=step_count)) +
      ggplot2::geom_rect(ggplot2::aes(xmin = cgm_timestamp, xmax = dplyr::lead(cgm_timestamp), ymin = -Inf, ymax = Inf, fill=time_of_day), alpha=0.5) +
      ggplot2::scale_fill_manual(values = c("transparent", "grey")) +
      ggplot2::geom_bar(stat="identity", fill="cadetblue", width = 75) +
      ggplot2::ylab("Step count (n/min)") +
      ggplot2::xlab("Date (DD/MM/YY HH:MM)") +
      ggplot2::scale_x_datetime(date_labels = "%d/%m/%y \n %H:%M",
                                date_breaks = "1 day",
                                date_minor_breaks = "6 hours",
                                expand = c(0,0),
                                limits = c(lubridate::floor_date(dplyr::first(cgm_data$cgm_timestamp[cgm_data$study_week==PageNumber]), unit = "day"),
                                           lubridate::ceiling_date(dplyr::last(cgm_data$cgm_timestamp[cgm_data$study_week==PageNumber]), unit = "day"))) +
      ggplot2::scale_y_continuous(breaks = c(50, 100, 150, 200),
                                  expand = c(0,0)) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position="none",
                     panel.grid.minor.y = ggplot2::element_blank(),
                     panel.grid.minor.x = ggplot2::element_blank(),
                     plot.margin = ggplot2::unit(c(0.25, 0.75, 0.25, 0.25), "cm"))

    combined_plot <- ggpubr::ggarrange(cgm_plot, activity_plot,
                                       heights = c(1, 0.5),
                                       ncol=1,
                                       nrow=2, align="v", common.legend = F)

    } else if(DataType == "heartrate"){

      hr_plot <- ggplot2::ggplot(cgm_data, ggplot2::aes(x=cgm_timestamp, y=heart_rate)) +
        ggplot2::geom_rect(ggplot2::aes(xmin = cgm_timestamp, xmax = dplyr::lead(cgm_timestamp), ymin = -Inf, ymax = Inf, fill=time_of_day), alpha=0.5) +
        ggplot2::scale_fill_manual(values = c("transparent", "grey")) +
        ggplot2::geom_line(ggplot2::aes(y=heart_rate), colour = "darkred") +
        ggplot2::ylab("Heart rate (bpm)") +
        ggplot2::xlab("Date (DD/MM/YY HH:MM)") +
        ggplot2::scale_x_datetime(date_labels = "%d/%m/%y \n %H:%M",
                                  date_breaks = "1 day",
                                  date_minor_breaks = "6 hours",
                                  expand = c(0,0),
                                  limits = c(lubridate::floor_date(dplyr::first(cgm_data$cgm_timestamp[cgm_data$study_week==PageNumber]), unit = "day"),
                                             lubridate::ceiling_date(dplyr::last(cgm_data$cgm_timestamp[cgm_data$study_week==PageNumber]), unit = "day"))) +
        ggplot2::scale_y_continuous(breaks = c(50, 100, 150),
                                    expand=c(0,0)) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position="none",
                       panel.grid.minor.y = ggplot2::element_blank(),
                       panel.grid.minor.x = ggplot2::element_blank(),
                       plot.margin = ggplot2::unit(c(0.25, 0.75, 0.25, 0.25), "cm"))

      combined_plot <- ggpubr::ggarrange(cgm_plot, hr_plot,
                                         heights = c(1, 0.5),
                                         ncol=1,
                                         nrow=2, align="v", common.legend = F)

}
    return(combined_plot)

  } else if(TimeBreak == "day"){

    cgm_plot <-
      ggplot2::ggplot(cgm_data, ggplot2::aes(x=cgm_timestamp, y=glucose)) +
      ggforce::facet_grid_paginate(~study_day, ncol=1, nrow=1, page = PageNumber, scales = "free") +
      ggplot2::geom_rect(ggplot2::aes(xmin = cgm_timestamp, xmax = dplyr::lead(cgm_timestamp), ymin = -Inf, ymax = Inf, fill=time_of_day), alpha=0.5) +
      ggplot2::scale_fill_manual(values = c("transparent", "grey")) +
      ggplot2::geom_line(ggplot2::aes(y=glucose), color = "blue4", size=0.5) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 3.9), colour="green4", size=0.5) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 10), colour="green4", size=0.5) +
      ggplot2::ylab("Glucose concentration (mmol/L)") +
      #ggplot2::xlab("Date (DD/MM/YY HH:MM)") +
      ggplot2::scale_x_datetime(date_labels = "%d/%m/%y \n %H:%M",
                                date_breaks = "6 hours",
                                expand = c(0,0),
                                limits = c(lubridate::floor_date(dplyr::first(cgm_data$cgm_timestamp[cgm_data$study_day==PageNumber]), unit = "day"),
                                           lubridate::ceiling_date(dplyr::last(cgm_data$cgm_timestamp[cgm_data$study_day==PageNumber]), unit = "day"))) +
      ggplot2::scale_y_continuous(breaks = c(3, 3.9, 10, 15)) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position="none",
                     axis.title.x = ggplot2::element_blank(),
                     panel.grid.minor.y = ggplot2::element_blank(),
                     panel.grid.minor.x = ggplot2::element_blank(),
                     plot.margin = ggplot2::unit(c(0.25, 0.75, 0.25, 0.25), "cm"))

if(DataType == "stepcount"){

    activity_plot <-
      ggplot2::ggplot(cgm_data, ggplot2::aes(x=cgm_timestamp, y=step_count)) +
      ggplot2::geom_rect(ggplot2::aes(xmin = cgm_timestamp, xmax = dplyr::lead(cgm_timestamp), ymin = -Inf, ymax = Inf, fill=time_of_day), alpha=0.5) +
      ggplot2::scale_fill_manual(values = c("transparent", "grey")) +
      ggplot2::geom_bar(stat="identity", fill="cadetblue",width =75) +
      ggplot2::ylab("Step count (n/min)") +
      ggplot2::xlab("Date (DD/MM/YY HH:MM)") +
      ggplot2::scale_x_datetime(date_labels = "%d/%m/%y \n %H:%M",
                                date_breaks = "6 hours",
                                expand = c(0,0),
                                limits = c(lubridate::floor_date(dplyr::first(cgm_data$cgm_timestamp[cgm_data$study_day==PageNumber]), unit = "day"),
                                           lubridate::ceiling_date(dplyr::last(cgm_data$cgm_timestamp[cgm_data$study_day==PageNumber]), unit = "day"))) +
      ggplot2::scale_y_continuous(breaks = c(50, 100, 150, 200),
                                  expand = c(0,0)) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position="none",
                     panel.grid.minor.y = ggplot2::element_blank(),
                     panel.grid.minor.x = ggplot2::element_blank(),
                     plot.margin = ggplot2::unit(c(0.25, 0.75, 0.25, 0.25), "cm"))

    combined_plot <- ggpubr::ggarrange(cgm_plot, activity_plot,
                                       heights = c(1, 0.5),
                                       ncol=1,
                                       nrow=2, align="v", common.legend = F)
} else if(DataType == "heartrate"){

  hr_plot <- ggplot2::ggplot(cgm_data, ggplot2::aes(x=cgm_timestamp, y=heart_rate)) +
    ggplot2::geom_rect(ggplot2::aes(xmin = cgm_timestamp, xmax = dplyr::lead(cgm_timestamp), ymin = -Inf, ymax = Inf, fill=time_of_day), alpha=0.5) +
    ggplot2::scale_fill_manual(values = c("transparent", "grey")) +
    ggplot2::geom_line(ggplot2::aes(y=heart_rate), colour = "darkred") +
    ggplot2::ylab("Heart rate (bpm)") +
    ggplot2::xlab("Date (DD/MM/YY HH:MM)") +
    ggplot2::scale_x_datetime(date_labels = "%d/%m/%y \n %H:%M",
                              date_breaks = "6 hours",
                              expand = c(0,0),
                              limits = c(lubridate::floor_date(dplyr::first(cgm_data$cgm_timestamp[cgm_data$study_day==PageNumber]), unit = "day"),
                                         lubridate::ceiling_date(dplyr::last(cgm_data$cgm_timestamp[cgm_data$study_day==PageNumber]), unit = "day"))) +
    ggplot2::scale_y_continuous(breaks = c(50, 100, 150),
                                expand=c(0,0)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position="none",
                   panel.grid.minor.y = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   plot.margin = ggplot2::unit(c(0.25, 0.75, 0.25, 0.25), "cm"))

  combined_plot <- ggpubr::ggarrange(cgm_plot, hr_plot,
                                     heights = c(1, 0.5),
                                     ncol=1,
                                     nrow=2, align="v", common.legend = F)

}
    return(combined_plot)

  }

}


#' @title Links Continuous Glucose Monitoring (CGM) Time Series Data with Person-Reported Hypoglycaemia (PRH) Data
#'
#' @description Creates a dataset containing longitudinal CGM time series data with PRH episodes
#' on the same row as the closest (in time) CGM timestamps. To run this function, the PRH dataset must be first built
#' using the prhLink function.
#'
#' @param CgmDataFrame A dataframe containing CGM data. Must have columns id, cgm_timestamp, glucose.
#' @param PrhDataFrame A dataframe containing all PRH episodes reported with a time. Output of the \link[hypometrics]{umotifLink}
#' function.
#'
#' @return A dataset containing CGM data with timestamp of PRH episode aligned with closest CGM timestamp.
#'
#' @examples
#'
#' \dontrun{
#' hypometrics::cgmprhLink(CgmDataFrame = cgm,
#'                         PrhDataFrame = prh_map)
#' }
#'
#' @export
cgmprhLink <- function(CgmDataFrame,
                       PrhDataFrame){


  #### Check function inputs and that the prh dataset includes the columns that are outputted using the uMotif link functions ####

  chk::check_names(CgmDataFrame, names = c("id", "cgm_timestamp", "glucose"))

  chk::chk_character(CgmDataFrame$id)

  checkmate::assertPOSIXct(CgmDataFrame$cgm_timestamp)

  chk::chk_numeric(CgmDataFrame$glucose)

  chk::check_names(PrhDataFrame, names = c("id", "checkin_prh_timestamp", "motif_prh_timestamp"))


  #### Function within the function so it can be called at the participant level ####

  cgm_prh_link <- function(cgm_dataset, prh_dataset){

  #Create new helping variable identical to cgm timestamp column to be able to roll nearest later
  cgm_to_match_closest <- cgm_dataset %>%
    dplyr::select(cgm_timestamp) %>%
    dplyr::mutate(datetime = cgm_timestamp)

  #Create new helping variable identical to prh timestamp columns (motif timing takes priority) to be able to roll nearest later
  prh_dataset <- prh_dataset %>%
    dplyr::mutate(datetime = dplyr::if_else(!is.na(motif_prh_timestamp), motif_prh_timestamp, checkin_prh_timestamp))

  #Return the closest cgm timestamp to the prh timestamp from the cgm dataset
  prh_dataset$cgm_timestamp <- data.table::setDT(cgm_to_match_closest)[prh_dataset, cgm_timestamp, roll="nearest", on = "datetime"]

  #Remove returned cgm timestamp when the gap between cgm timestamp and prh timestamp is over an hour (i.e. would remove when there is for example a gap in cgm data and closest cgm timestamp returned is in fact not that close in time to prh timestamp)
  prh_dataset <- prh_dataset %>%
    #after returning the closest CGM timestamp if the diff between the PRH and CGM timestamps is >1h (e.g. PRH submitted when CGM data had not started yet) then this will turn to NA
    transform(cgm_timestamp = replace(cgm_timestamp,
                                      abs(difftime(datetime, cgm_timestamp, units = "hour"))>1, NA)) %>%
    #the NA rows will then be removed
    dplyr::filter(!is.na(cgm_timestamp)) %>%
  #If two prhs are submitted very close to each other (e.g. technical error) the line would get rid of the second returned cgm timestamp and hence the duplicated prh
    dplyr::distinct(cgm_timestamp, .keep_all = T) %>%
    dplyr::select(id, dplyr::everything())

##Returning PRH timestamps to CGM dataset based on cgm_timestamp column created (with closest CGM timestamp)
#the returning dataset is CGM timeseries with two extra columns: checkin_prh_timestamp and motif_prh_timestamp
  #those columns will be NA when there is no match or populated when there was a match in time between CGM timestamp and PRH timestamp
  cgm_final <- dplyr::full_join(cgm_dataset,
                                prh_dataset[,c("id", "checkin_prh_timestamp", "motif_prh_timestamp" , "cgm_timestamp")])


  }

  #### Splitting the CGM and PRH DataFrames by id ####
  cgm_dataset <- split(CgmDataFrame, CgmDataFrame$id)
  prh_dataset <- split(PrhDataFrame, PrhDataFrame$id)

  #### Calling the function for each id within data frame containing individual participants data ####
  list_linked <- mapply(cgm_prh_link, cgm_dataset, prh_dataset, SIMPLIFY = F)

  #### Row binding individual participants data in a single dataframe ####
  final_linked_df <- dplyr::bind_rows(list_linked, .id = "id")

  #### Return output ####
  return(final_linked_df)


}


#' @title Links Sensor-Detected Hypoglycaemia (SDH) with Person-Reported Hypoglycaemia (PRH) episodes
#'
#' @description Creates a dataset containing SDH episodes (from CGM data)
#' linked with PRH episodes (from uMotif data). There is one row per hypoglycaemic episode.
#' When PRH episodes fall within  +/- 1h the SDH interval, they are reported on the same row.
#' To run this function, SDH and PRH datasets must be first built using sdhDetection and umotifprhLink functions.
#'
#' @param SdhDataFrame A dataframe containing all SDH episodes. Output of the \link[hypometrics]{sdhDetection}
#' function.
#' @param PrhDataFrame A dataframe containing all PRH episodes reported with a time. Output of the \link[hypometrics]{umotifLink}
#' function.
#'
#' @return A dataset containing both SDHs and PRHs in chronological order
#' for each participant. Where PRHs fall within -/+ 1h of SDH interval,
#' those have been matched (i.e considered to be the same event and shown on a unique row).
#'
#' @examples
#'
#' \dontrun{
#' hypometrics::sdhprhLink(SdhDataFrame = sdh,
#'                         PrhDataFrame = prh)
#' }
#'
#' @export

sdhprhLink <- function(SdhDataFrame,
                       PrhDataFrame){

  #### Check function inputs - specifically that the datasets include the columns that are outputted using the sdhDetection and uMotif link functions ####

  chk::check_names(SdhDataFrame, names = c("id", "sdh_number", "sdh_interval", "sdh_duration_mins",
                                           "sdh_nadir", "sdh_night_status"))

  chk::check_names(PrhDataFrame, names = c("id", "checkin_prh_timestamp", "motif_prh_timestamp",
                                           "symptomatic_prh", "prevented_prh",
                                           "Howdidyoudetectyourhypoorahypothatwasabouttohappen",
                                           "Howdidyoudetectyourhypoorahypothatwasabouttohappenotherpleasespecify",
                                           "Whathappened", "glucose_concentration", "sweating", "heart_palpitation",
                                           "shaking", "hunger", "confusion", "difficulties_speaking", "movement_coordination",
                                           "headache"
                                           ))


  #### Define lubridate::within() function and loading the dplyr package ####
  `%within%` <- lubridate::`%within%`
  library(dplyr) #so that the use of the n() function does not return an error


  #### Function within the function so it can be called at the participant level ####

  sdh_prh_link <- function(sdh_dataset, prh_dataset){

   #Applying the SDH PRH matching rules
    matching_rules <- prh_dataset %>%
      #Creating a column with PRH timestamps (to later determine whether it falls within a SDH interval) - giving priority to motif PRH timestamps when they are not missing as reported in real time
      dplyr::mutate(all_timestamps = dplyr::case_when(!is.na(checkin_prh_timestamp) & is.na(motif_prh_timestamp) ~ checkin_prh_timestamp,
                                                      !is.na(motif_prh_timestamp) ~ motif_prh_timestamp)) %>%
      dplyr::inner_join(sdh_dataset) %>%
      dplyr::select(-c(sdh_duration_mins, sdh_nadir, sdh_night_status)) %>%
      # ##Applying the matching rules (will filter the ones that fall within the relevant intervals) - note the differences between motif and check-ins (giving a bit more flexibility to check-ins as reported retrospectively and not in real time like the motifs)
                                     #1. filter if only checkin PRH was reported and fell within -/+1h of interval of SDH
      dplyr::filter(dplyr::case_when(all_timestamps == checkin_prh_timestamp ~ all_timestamps %within% lubridate::as.interval(lubridate::int_start(sdh_interval)-3600, lubridate::int_end(sdh_interval)+3600), #sdh-60min+60min
                                     #2. filter if only motif PRH was reported and fell within -15min/+1h of interval of SDH
                                     all_timestamps == motif_prh_timestamp & is.na(checkin_prh_timestamp) ~ all_timestamps %within% lubridate::as.interval(lubridate::int_start(sdh_interval)-900, lubridate::int_end(sdh_interval)+3600),
                                     #3. filter if both motif and checkin PRH were reported and either motif PRH fell within -15min/+1h of interval of SDH OR checkin fell within -/+1h of interval of SDH
                                     all_timestamps == motif_prh_timestamp & !is.na(checkin_prh_timestamp) ~ all_timestamps %within% lubridate::as.interval(lubridate::int_start(sdh_interval)-900, lubridate::int_end(sdh_interval)+3600) | checkin_prh_timestamp %within% lubridate::as.interval(lubridate::int_start(sdh_interval)-3600, lubridate::int_end(sdh_interval)+3600))) %>%
      # #First condition here: does the prh falls within the sdh? If yes, flag as 1, if not flag as 0 - the ultimate aim is to filter out those who do not fall within an interval or those who are not the closest to an interval
    #note that the cases where the prh is unique for a sdh will lead to NA rows
      dplyr::group_by(all_timestamps) %>%
      dplyr::mutate(one_prh_multi_sdh = dplyr::case_when(all_timestamps == dplyr::lead(all_timestamps) & all_timestamps %within% sdh_interval ~ 1,
                                                         all_timestamps == dplyr::lag(all_timestamps) & all_timestamps %within% sdh_interval ~ 1,
                                                         all_timestamps == dplyr::lead(all_timestamps) & !all_timestamps %within% sdh_interval ~ 0,
                                                         all_timestamps == dplyr::lag(all_timestamps) & !all_timestamps %within% sdh_interval ~ 0)) %>%
      # #if the prh doesn't fall within an interval then look for other possibilities
      dplyr::mutate(one_prh_multi_sdh = dplyr::case_when(one_prh_multi_sdh == 0 &
                                                           #1 if the prh falls in between 2 sdh intervals
                                                          (all_timestamps > lubridate::int_end(sdh_interval) & all_timestamps < lubridate::int_start(dplyr::lead(sdh_interval))) &
                                                           # AND if difftime on that row is inferior to difftime on the next row then flagged as 1, i.e. the sdh interval on the row marked as 1 is the one we want to keep as prh timestamp is closest to that interval
                                                          (abs(difftime(all_timestamps, lubridate::int_end(sdh_interval))) < abs(difftime(all_timestamps, lubridate::int_start(dplyr::lead(sdh_interval))))) ~ 1,
                                                          #2 if the prh falls in between 2 sdh intervals
                                                          one_prh_multi_sdh==0 &
                                                          (all_timestamps < lubridate::int_start(sdh_interval) & all_timestamps > lubridate::int_end(dplyr::lag(sdh_interval))) &
                                                          # AND if difftime on that row is inferior to difftime on the previous row then flagged as 1, i.e. the sdh interval on the row marked as 1 is the one we want to keep as prh timestamp is closest to that interval
                                                           (abs(difftime(all_timestamps, lubridate::int_start(sdh_interval))) < abs(difftime(all_timestamps, lubridate::int_end(dplyr::lag(sdh_interval))))) ~ 1,
                                                          #3 if the prh occurs after 2+ close sdhs, in which case  the sdh interval closest to the prh will be flagged as 1, i.e. the last row of the grouped prh timestamp (as sdh_intervals are in chronological order i.e. from oldest to more recent)
                                                         one_prh_multi_sdh==0 &
                                                         all_timestamps > lubridate::int_end(dplyr::last(sdh_interval)) & dplyr::row_number() == n() ~ 1,
                                                         #4 if the prh occurs before 2+ sdhs, in which case the sdh interval that is closest to the prh will be flagged as 1, i.e. the first row of the grouped prh timestamp (as sdh_intervals are in chronological order, from oldest to more recent)
                                                         one_prh_multi_sdh==0 &
                                                         all_timestamps < lubridate::int_start(dplyr::first(sdh_interval)) & dplyr::row_number() == 1 ~ 1,
                                                         ##5 if do not meet any of the conditions above, leave variable as is
                                                         TRUE ~ one_prh_multi_sdh)) %>%
      dplyr::ungroup()

 #Filtering out the duplicate PRHs (Where applicable) to create a data frame with unique prh for each sdh
    prhs_in_sdh <- matching_rules %>%
      dplyr::filter(one_prh_multi_sdh ==1 | is.na(one_prh_multi_sdh)) %>%
      dplyr::select(-all_timestamps)

  ##Creating the sdh prh maps by merging the original sdh map, the dataset with lmatched sdh and prhs and the original motif prh map
    sdh_prh_map <- sdh_dataset %>%
      list(prhs_in_sdh, prh_dataset) %>%
      purrr::reduce(dplyr::full_join) %>%
      #Creating an all_timestamps column so we can order dataset in chronological order
      dplyr::mutate(all_timestamps = dplyr::case_when(!is.na(sdh_interval) & is.na(checkin_prh_timestamp) & is.na(motif_prh_timestamp) ~ lubridate::int_start(sdh_interval),
                                                      is.na(sdh_interval) & !is.na(checkin_prh_timestamp) & is.na(motif_prh_timestamp) ~ checkin_prh_timestamp,
                                                      !is.na(motif_prh_timestamp) ~ motif_prh_timestamp,
                                                      !is.na(sdh_interval) & !is.na(checkin_prh_timestamp) & is.na(motif_prh_timestamp) ~ checkin_prh_timestamp)) %>%
      dplyr::arrange(all_timestamps) %>%
      dplyr::mutate(date = as.Date(all_timestamps)) %>%
      dplyr::select(-all_timestamps) %>%
      dplyr::relocate(date, .after = id)


  }

  #### Splitting the SDH and PRH DataFrames by id ####
  sdh_dataset <- split(SdhDataFrame, SdhDataFrame$id)
  prh_dataset <- split(PrhDataFrame, PrhDataFrame$id)

  #### Calling the function for each id within data frame containing individual participants data ####
  list_linked <- mapply(sdh_prh_link, sdh_dataset, prh_dataset, SIMPLIFY = F)

  #### Row binding individual participants data in a single dataframe ####
  final_linked_df <- dplyr::bind_rows(list_linked, .id = "id")

  #### Return output ####
  return(final_linked_df)



}

#' @title Visualise Glucose linked with Person-Reported Hypoglycaemia (PRH) Data
#'
#' @description Plots Glucose with PRH Data Over Time
#'
#' @param DataFrame A dataframe of CGM data linked with PRH data. Output of the \link[hypometrics]{cgmprhLink}
#' function.
#' @param StudyID ID of participant for whom CGM data will be plotted.
#' @param TimeBreak Character object which defines whether plot outputs should be split
#' by time period. Default is "No" in which case there will be a single plot produce
#' including data for the whole period. Other options are "week" and "day" in which case multiple
#' plots will be produced according to TimeBreak.
#' @param PageNumber Vector indicating which page (i.e. week/day) number selected for
#' visualisation as plot is faceted according to TimeBreak.
#' @param AddSleep Character object for the user to specify whether the CGM plot will have added day/night
#' visualisation ("no" - default) or sleep/awake visualisation ("yes"). If set to "yes", the function requires a CGM dataset
#' with a sleep_status (Awake vs asleep) column as input - this can be achieved by running the \link[hypometrics]{cgmsleepLink}
#' followed by the \link[hypometrics]{cgmprhLink} function.
#'
#' @details
#' This functions plots CGM with PRH data over time with red shaded area corresponding to the time period
#' between 00:00 and 06:00 as typically used to describe nocturnal hypoglycaemia. The function
#' offers the options to look at the  glucose data over the entire study period, for a specific week or
#' specific date. It also offers the option to plot data with corresponding sleep status (awake vs asleep)
#' if sleep tracker data is available. Where sleep data is missing, the area is coloured grey.
#'
#' @return A graphical representation showing glucose trace wtih PRH time points over time with shaded area
#' representing night or sleep time.
#'
#' @examples
#' \dontrun{
#' hypometrics::cgmprhVisualise(DataFrame,
#'                             StudyID = "001",
#'                             TimeBreak = "day",
#'                             PageNumber = 7,
#'                             AddSleep = "no")
#' }
#'
#' @export
cgmprhVisualise <- function(DataFrame,
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
                    study_week = match(as.Date(week_start), unique(as.Date(week_start))),
                    all_timestamps = dplyr::if_else(!is.na(motif_prh_timestamp), motif_prh_timestamp, checkin_prh_timestamp))


  } else if(AddSleep=="yes"){

    cgm_data <- DataFrame %>%
      dplyr::filter(id == StudyID) %>%
      dplyr::mutate(time_of_day = factor(sleep_status, levels = c("Awake", "Asleep")),
                    study_day =  match(as.Date(cgm_timestamp), unique(as.Date(cgm_timestamp))),
                    week_start = cut((as.Date(cgm_timestamp)),
                                     breaks = seq.Date(min((as.Date(cgm_timestamp))),
                                                       max((as.Date(cgm_timestamp))) + 7,
                                                       by = "week")),
                    study_week = match(as.Date(week_start), unique(as.Date(week_start))),
                    all_timestamps = dplyr::if_else(!is.na(motif_prh_timestamp), motif_prh_timestamp, checkin_prh_timestamp))


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
      ggplot2::geom_point(ggplot2::aes(x = all_timestamps, y=3), colour = "red", size=2) +
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
      ggplot2::geom_point(ggplot2::aes(x = all_timestamps, y=3), colour = "red", size=2) +
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
      ggplot2::geom_point(ggplot2::aes(x = all_timestamps, y=3), colour = "red", size=2) +
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



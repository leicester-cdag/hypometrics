#' @title Unzip, Read and Combine Raw Activity Files Dowloaded
#' From User's Fitbit account
#'
#' @description This function enables the unzipping, reading and
#' combining of raw activity files following download from user's
#' Fitbit account.
#'
#' @param Unzip Logical string (TRUE/FALSE) which determines whether fitbit folder needs to be
#' unzipped or not
#' @param FolderPath Character object indicating path to folder where Fitbit data is stored
#' @param FileType Character object indicating what type of file is to be read. Can be either
#' "json" or "csv".
#' @param FilePattern Character object indicating the pattern in the file name that will be used to
#' extract the activity files of interest. For example, "^heart" for heart rate files, "steps"
#' for step count files, "sedentary" for sedentary minutes files.
#' @param StudyID ID of participant for whom activity data will be read.
#'
#' @return A dataset containing original Fitbit activity (e.g. steps, heart rate)
#' data with an additional column with participant's ID.
#'
#' @examples
#'
#' \dontrun{
#' hypometrics::activityRead(Unzip = TRUE,
#'                           FolderPath = "~/Documents",
#'                           FileType = "json",
#'                           FilePattern = "steps",
#'                           StudyID = "001")
#' }
#'
#' @export
activityRead <- function(Unzip = FALSE,
                         FolderPath,
                         FileType,
                         FilePattern,
                         StudyID){

  #### Check function arguments ####

  chk::chk_flag(Unzip)

  chk::check_dirs(FolderPath)

  if(!FileType %in% c("json", "csv")){
    stop("FileType must be `json` or `csv` only.")
  }

  chk::chk_character(FilePattern)

  chk::chk_character(StudyID)


if(Unzip == TRUE){

  #### Unzip original Fitbit download and create a new unzipped folder ####
  setwd(FolderPath)

  fitbit_files <- utils::unzip(list.files(pattern = "fitbit.*zip", ignore.case = T),
                  list=T,
               exdir = paste0(FolderPath, "/Unzipped Fitbit"))

  setwd(paste0(FolderPath, "/Unzipped Fitbit"))
  fitbit_files <- list.files(recursive = T)

}else{
  #### If unzipped fitbit folder already exists, will directly list the fitbit files ####
  setwd(paste0(FolderPath, "/Unzipped Fitbit"))
  fitbit_files <- list.files(recursive = T)
}

  if(FileType == "json"){

  #### Extract files of interest from list of all files ####
  files_of_interest <- fitbit_files[grepl(paste0(FilePattern, ".*json"), fitbit_files, ignore.case = T)]

  #### Row bind JSON files ####
  original_file <- jsonlite::rbind_pages(lapply(files_of_interest, jsonlite::fromJSON)) %>%
    dplyr::mutate(id = StudyID) %>% # need this step as fitbit individual files do not contain participant's ID
    dplyr::select(id, dplyr::everything())


  ### Return output ####
  return(original_file)

  } else if(FileType == "csv"){

    #### Extract files of interest from list of all files ####
    files_of_interest <- fitbit_files[grepl(paste0(FilePattern, ".*csv"), fitbit_files, ignore.case = T)]

  #### Row bind csv files ####
    original_file <- vroom::vroom(files_of_interest) %>%
      dplyr::mutate(id = StudyID) %>% # need this step as fitbit individual files do not contain participant's ID
      dplyr::select(id, dplyr::everything())

    #### Return output ####
    return(original_file)
  }


}

#' @title Visualise Activity data
#'
#' @description Creates a bar chart of the number of steps or heart rate
#' minute by minute over the study period
#'
#' @param DataFrame A dataframe containing Fitbit activity data.
#' @param DataType Character object determining which data to visualise. 2 options available: "stepcount"
#' or "heartrate".
#' @param TimeBreak Character object which defines whether plot outputs should be split
#' by time period. Default is "No" in which case there will be a single plot produce
#' including data for the whole period. Other options are "week" and "day" in which case multiple
#' plots will be produced according to TimeBreak.
#' @param PageNumber Vector indicating which page (i.e. week/day) number selected for
#' visualisation as plot is faceted according to TimeBreak.
#' @param StudyID ID of participant for whom step count or heart rate data will be plotted.
#'
#' @return Bar chart displaying step count or heart rate over time
#'
#' @examples
#'
#' \dontrun{
#' hypometrics::activityVisualise(DataFrame,
#'                                DataType = "stepcount",
#'                                TimeBreak = "no",
#'                                PageNumber = 1,
#'                                StudyID = "01001")
#' }
#'
#' @export
activityVisualise <- function(DataFrame,
                              DataType,
                              TimeBreak = "no",
                              PageNumber = NA_real_,
                              StudyID){

  #### Check function arguments ####

  if(!DataType %in% c("stepcount", "heartrate")){
    stop("DataType must be `stepcount` or `heartrate` only.")
  }

  chk::chk_character(TimeBreak)

  chk::chk_numeric(PageNumber)

  chk::chk_character(StudyID)


  ##### Prep data ####

  if(DataType=="stepcount"){

    ### Check data frame columns ####

    chk::check_names(DataFrame, names = c("id", "step_timestamp", "count"))

    chk::chk_character(DataFrame$id)

    checkmate::assertPOSIXct(DataFrame$step_timestamp)

    chk::chk_numeric(DataFrame$count)

    #### Add additional columns to allow for further filtering by TimeBreak ####

  DataFrame <- DataFrame %>%
  dplyr::filter(id == StudyID) %>%
  dplyr::mutate(study_day =  match(as.Date(step_timestamp), unique(as.Date(step_timestamp))),
                week_start = cut((as.Date(step_timestamp)),
                                   breaks = seq.Date(min((as.Date(step_timestamp))),
                                                     max((as.Date(step_timestamp))) + 7,
                                                     by = "week")),
                study_week = match(as.Date(week_start), unique(as.Date(week_start))))
  } else{

    ### Check data frame columns ####

    chk::check_names(DataFrame, names = c("id", "hr_timestamp", "heart_rate"))

    chk::chk_character(DataFrame$id)

    checkmate::assertPOSIXct(DataFrame$hr_timestamp)

    chk::chk_numeric(DataFrame$heart_rate)

    #### Add additional columns to allow for further filtering by TimeBreak ####

    DataFrame <- DataFrame %>%
      dplyr::filter(id == StudyID) %>%
      dplyr::mutate(study_day =  match(as.Date(hr_timestamp), unique(as.Date(hr_timestamp))),
                    week_start = cut((as.Date(hr_timestamp)),
                                     breaks = seq.Date(min((as.Date(hr_timestamp))),
                                                       max((as.Date(hr_timestamp))) + 7,
                                                       by = "week")),
                    study_week = match(as.Date(week_start), unique(as.Date(week_start))))


  }



  #### Visualise data according to parameters selected ####
  if(TimeBreak == "no"){
    if(DataType=="stepcount"){

  #### Plot the data ###
  activity_plot <- DataFrame %>%
    ggplot2::ggplot(ggplot2::aes(x = step_timestamp, y = count)) +
    ggplot2::geom_bar(stat="identity", fill="cadetblue4") +
    ggplot2::ylab("Step count (n/min)") +
    ggplot2::xlab("Date (DD/MM/YY)") +
    ggplot2::scale_x_datetime(date_labels = "%d/%m/%y",
                              date_breaks = "1 week",
                              expand = c(0,0)) +
    ggplot2::scale_y_continuous(expand = c(0,0)) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank())

    #### Return plots ####
    return(activity_plot)

    }else{

      #### Plot the data ###
      hr_plot <- DataFrame %>%
        ggplot2::ggplot(ggplot2::aes(x = hr_timestamp, y = heart_rate)) +
        ggplot2::geom_line(ggplot2::aes(y=heart_rate), colour = "darkred") +
        ggplot2::ylab("Heart rate (beats/min)") +
        ggplot2::xlab("Date (DD/MM/YY)") +
        ggplot2::scale_x_datetime(date_labels = "%d/%m/%y",
                                  date_breaks = "1 week",
                                  expand = c(0,0)) +
        ggplot2::scale_y_continuous(expand = c(0,0)) +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank(),
                       panel.grid.minor.y = ggplot2::element_blank(),
                       legend.title = ggplot2::element_blank())

      #### Return plots ####
      return(hr_plot)}

  }else if(TimeBreak == "week"){

    if(DataType=="stepcount"){

      #### Plot the data ###
      activity_plot <- DataFrame %>%
        ggplot2::ggplot(ggplot2::aes(x = step_timestamp, y = count)) +
        ggplot2::geom_bar(stat="identity", fill="cadetblue4") +
        ggforce::facet_grid_paginate(~study_week, ncol=1, nrow=1, page = PageNumber, scales = "free") +
        ggplot2::ylab("Step count (n/min)") +
        ggplot2::xlab("Date (DD/MM/YY HH:MM)") +
        ggplot2::scale_x_datetime(date_labels = "%d/%m/%y \n %H:%M",
                                  date_breaks = "1 day",
                                  date_minor_breaks = "6 hours",
                                  expand = c(0,0),
                                  limits = c(lubridate::floor_date(dplyr::first(DataFrame$step_timestamp[DataFrame$study_week==PageNumber]), unit = "day"),
                                             lubridate::ceiling_date(dplyr::last(DataFrame$step_timestamp[DataFrame$study_week==PageNumber]), unit = "day"))) +
        ggplot2::scale_y_continuous(expand = c(0,0)) +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank(),
                       panel.grid.minor.y = ggplot2::element_blank(),
                       legend.title = ggplot2::element_blank())

      #### Return plots ####
      return(activity_plot)

    }else{

      #### Plot the data ###
      hr_plot <- DataFrame %>%
        ggplot2::ggplot(ggplot2::aes(x = hr_timestamp, y = heart_rate)) +
        ggplot2::geom_line(ggplot2::aes(y=heart_rate), colour = "darkred") +
        ggforce::facet_grid_paginate(~study_week, ncol=1, nrow=1, page = PageNumber, scales = "free") +
        ggplot2::ylab("Heart rate (beats/min)") +
        ggplot2::xlab("Date (DD/MM/YY HH:MM)") +
        ggplot2::scale_x_datetime(date_labels = "%d/%m/%y \n %H:%M",
                                  date_breaks = "1 day",
                                  date_minor_breaks = "6 hours",
                                  expand = c(0,0),
                                  limits = c(lubridate::floor_date(dplyr::first(DataFrame$hr_timestamp[DataFrame$study_week==PageNumber]), unit = "day"),
                                             lubridate::ceiling_date(dplyr::last(DataFrame$hr_timestamp[DataFrame$study_week==PageNumber]), unit = "day"))) +
        ggplot2::scale_y_continuous(expand = c(0,0)) +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank(),
                       panel.grid.minor.y = ggplot2::element_blank(),
                       legend.title = ggplot2::element_blank())

      #### Return plots ####
      return(hr_plot)
      }


  } else if(TimeBreak == "day"){

    if(DataType=="stepcount"){

      #### Plot the data ###
      activity_plot <- DataFrame %>%
        ggplot2::ggplot(ggplot2::aes(x = step_timestamp, y = count)) +
        ggplot2::geom_bar(stat="identity", fill="cadetblue4") +
        ggforce::facet_grid_paginate(~study_day, ncol=1, nrow=1, page = PageNumber, scales = "free") +
        ggplot2::ylab("Step count (n/min)") +
        ggplot2::xlab("Date (DD/MM/YY HH:MM)") +
        ggplot2::scale_x_datetime(date_labels = "%d/%m/%y \n %H:%M",
                                  date_breaks = "6 hours",
                                  expand = c(0,0),
                                  limits = c(lubridate::floor_date(dplyr::first(DataFrame$step_timestamp[DataFrame$study_day==PageNumber]), unit = "day"),
                                             lubridate::ceiling_date(dplyr::last(DataFrame$step_timestamp[DataFrame$study_day==PageNumber]), unit = "day"))) +
        ggplot2::scale_y_continuous(expand = c(0,0)) +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank(),
                       panel.grid.minor.y = ggplot2::element_blank(),
                       legend.title = ggplot2::element_blank())

      #### Return plots ####
      return(activity_plot)

    }else{


      #### Plot the data ###
      hr_plot <- DataFrame %>%
        ggplot2::ggplot(ggplot2::aes(x =hr_timestamp, y = heart_rate)) +
        ggforce::facet_grid_paginate(~study_day, ncol=1, nrow=1, page = PageNumber, scales = "free") +
        ggplot2::geom_line(ggplot2::aes(y=heart_rate), colour = "darkred") +
        ggplot2::ylab("Step count (n/min)") +
        ggplot2::xlab("Date (DD/MM/YY HH:MM)") +
        ggplot2::scale_x_datetime(date_labels = "%d/%m/%y \n %H:%M",
                                  date_breaks = "6 hours",
                                  expand = c(0,0),
                                  limits = c(lubridate::floor_date(dplyr::first(DataFrame$hr_timestamp[DataFrame$study_day==PageNumber]), unit = "day"),
                                             lubridate::ceiling_date(dplyr::last(DataFrame$hr_timestamp[DataFrame$study_day==PageNumber]), unit = "day"))) +
        ggplot2::scale_y_continuous(expand = c(0,0)) +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank(),
                       panel.grid.minor.y = ggplot2::element_blank(),
                       legend.title = ggplot2::element_blank())

      #### Return plots ####
      return(hr_plot)


    }

  }
}

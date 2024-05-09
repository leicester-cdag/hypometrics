#' @title Detection and Description of Episodes of Continuous Glucose Monitoring (CGM) Hypoglycaemia
#'
#' @description Function which summarises raw CGM data and produces an output which
#' includes key characteristics (e.g nadir, duration) of each CGM-detected hypoglycaemic episode.
#'
#' @param DataFrame A dataframe with CGM data in which hypoglycaemic episodes will be detected.
#' Must have columns id, cgm_timestamp, glucose.
#' @param DetectionLimit  Object of type numeric or integer corresponding to
#' the glucose value used to detect hypoglycaemia. Default is 3.9 (mmol/L).
#' @param DetectionDuration Object of type numeric or integer corresponding to
#' the duration at or below the DetectionLimit for a hypolgycaemic episode to be detected.
#' Default is 15 (minutes).
#'
#' @return A dataframe with one hypoglycaemic episode per row with characteristics in each column
#'
#' @examples
#' \dontrun{
#' hypometrics::sdhDetection(DataFrame,
#'                           DetectionLimit = 3,
#'                           DetectionDuration = 30)
#' }
#'
#' @export
#'
sdhDetection <- function(DataFrame,
                         DetectionLimit = 3.9,
                         DetectionDuration = 15){

  #### Check function inputs and data frame columns ####
  if(ncol(DataFrame) != 3){
    stop("Unexpected number of columns for `DataFrame`,
         `DataFrame` must have exactly three columns: `id`, `cgm_timestamp`, `glucose`.")
  }

  if(all.equal(colnames(DataFrame), c("id", "cgm_timestamp", "glucose")) != TRUE){
    stop("Unexpected column names in `DataFrame`. `DataFrame` columns must have
         the following names: `id`, `cgm_timestamp`, `glucose`.")
  }

 if(is.character(DataFrame$id) == FALSE){
   stop("Column `id` is of unexpected type. Must be character.")
 }

 if(lubridate::is.POSIXct(DataFrame$cgm_timestamp) == FALSE){
    stop("Column `cgm_timestamp` is of unexpected type. Must be of type date-time.")
 }

 if(is.numeric(DataFrame$glucose) == FALSE){
    stop("Column `glucose` is of unexpected type. Must be numeric.")

  }

  if(is.na(DetectionLimit)){
    stop("DetectionLimit cannot be NA. Must be populated entry of type numeric or integer.")
  }

  if(!class(DetectionLimit) %in% c("numeric", "integer")){
    stop("DetectionLimit is of unexpected type. Must be numeric or integer.")
  }

  if(is.na(DetectionDuration)){
    stop("DetectionDuration cannot be NA. Must be populated entry of type numeric or integer.")
  }

  if(!class(DetectionDuration) %in% c("numeric", "integer")){
    stop("DetectionDuration is of unexpected type. Must be numeric or integer.")
  }


  #### Start data manipulation ####

  #### Original CGM data in long format with added information on time of data and values below threshold####
  cgm_data_long <- DataFrame %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(time_of_day = ifelse(as.numeric(format(cgm_timestamp, "%H"))<6 & as.numeric(format(cgm_timestamp, "%H"))>=0, "Night", "Day")) %>%
    dplyr::select(id, time_of_day, cgm_timestamp, glucose) %>%
    dplyr::mutate(below_threshold = dplyr::case_when(glucose < DetectionLimit ~ 1,
                                                     glucose >= DetectionLimit ~ 0),
                  seq_event = data.table::rleid(below_threshold))

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

  #return to a map format with one row per event
  sdh_map <- combined_cgm %>%
    dplyr::filter(sdh_grouped==1) %>%
    dplyr::group_by(id, sdh_seq, sdh_grouped, sdh_interval, sdh_duration_mins, sdh_nadir, sdh_night_status) %>%
    dplyr::summarise() %>%
    dplyr::ungroup() %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(sdh_number = data.table::rleid(sdh_seq)) %>%
    dplyr::select(id, sdh_number, sdh_interval, sdh_duration_mins, sdh_nadir, sdh_night_status) %>%
    dplyr::mutate_if(is.numeric, round, digits = 1)


  #Return final output - 1 row per event per individual
  return(sdh_map = data.frame(sdh_map))

}

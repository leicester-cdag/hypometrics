#' @title sdhDetection
#' @description Function which summarises raw CGM data and produces an output which
#' includes key characteristics (e.g nadir, duration) of each hypoglycaemic episode
#'
#' @param DataFrame A dataframe of CGM in which hypoglycaemic episodes will be detected.
#' Must have columns id, cgm_timestamp, glucose.
#' @param DetectionLimit  The glucose value used to detect hypoglycaemia
#' @param DetectionDuration The amount of time an individual needs to be at or below the DetectionLimit for a hypolgycaemic episode to be detected
#'
#' @return A dataframe with one hypoglycaemic episode per row with characteristics in each column
#' @export
#'
#' @examples
sdhDetection <- function(DataFrame, DetectionLimit, DetectionDuration){

  #### Original CGM data in long format with added niformation on time of data and values below threshold####
  cgm_data_long <-  DataFrame %>%
    group_by(id) %>%
    mutate(Time_of_day = ifelse(as.numeric(format(cgm_timestamp, "%H"))<6 & as.numeric(format(cgm_timestamp, "%H"))>=0, "Night", "Day")) %>%
    select(id, Time_of_day, cgm_timestamp, glucose) %>%
    mutate(below_threshold = case_when(glucose < DetectionLimit ~ 1,
                                       glucose >= DetectionLimit ~ 0),
           seq_event = rleid(below_threshold))

  #### Transform CGM data to wide format ####
  #Data frame displays the sequence of events i.e. below threshold, above threshold, NA... and how long for
  cgm_data_wide <- cgm_data_long %>%
    group_by(id) %>%
    mutate(last_timestamp = last(cgm_timestamp)) %>%
    group_by(id, seq_event, below_threshold,last_timestamp) %>%
    summarise(start_seq = first(cgm_timestamp)) %>%
    group_by(id, last_timestamp) %>%
    mutate(end_seq = lead(start_seq)) %>%
    transform(end_seq = if_else(is.na(end_seq), last_timestamp, end_seq)) %>%
    mutate(interval_seq = as.interval(start_seq, end_seq),
           duration_seq= time_length(interval_seq, unit = "min")) %>%
    select(-last_timestamp)

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
  combined_cgm <- full_join(cgm_data_long, cgm_data_wide, by = c("id", "seq_event", "below_threshold"))

  #Add when there is an SDH, internal, duration and nadir information (dataset still in long format)
  combined_cgm <- combined_cgm %>%
    group_by(id) %>%
    mutate(sdh_seq = ifelse(sdh_grouped==1, rleid(sdh_grouped), NA)) %>%
    group_by(id, sdh_seq, sdh_grouped) %>%
    mutate(sdh_interval = if_else(!is.na(sdh_seq), as.interval(first(start_seq), last(end_seq)), NA), #note if_else here, as opposed to ifelse will preserve the interval format and not coerce it to numeric
           sdh_duration_mins = ifelse(!is.na(sdh_seq), time_length(sdh_interval, unit = "mins"), NA),
           sdh_nadir = ifelse(!is.na(sdh_seq), min(glucose), NA))

#Add information as to whether there was a change in night status during a single SDH
  combined_cgm <- combined_cgm %>%
    group_by(id, sdh_seq) %>%
    mutate(timeofday_change_flag = case_when(!is.na(sdh_seq) & Time_of_day == first(Time_of_day) ~ 0,
                                             !is.na(sdh_seq) & Time_of_day !=lag(Time_of_day) ~ 1,
                                             !is.na(sdh_seq) & Time_of_day == lag(Time_of_day) ~ 0,
                                             is.na(sdh_seq) ~ NA_real_))

#Indicate whether SDH night status is overlap if group night status is 1
  combined_cgm <- combined_cgm %>%
    group_by(id) %>%
    mutate(sdh_night_status = ifelse(!is.na(sdh_seq), Time_of_day, NA)) %>%
    group_by(id, sdh_seq) %>%
    mutate(group_night_status = ifelse(!is.na(timeofday_change_flag), sum(timeofday_change_flag), NA)) %>%
    #essentially this will flag if no change in status then will be 0s if change in status the sum will be 1 for that sdh!
    transform(sdh_night_status = ifelse(group_night_status == 0, sdh_night_status, "Overlap"))

  #return to a map format with one row per event
  sdh_map <- combined_cgm %>%
    filter(sdh_grouped==1) %>%
    group_by(id, sdh_seq, sdh_grouped, sdh_interval, sdh_duration_mins, sdh_nadir, sdh_night_status) %>%
    summarise() %>%
    ungroup() %>%
    group_by(id) %>%
    mutate(sdh_number = rleid(sdh_seq)) %>%
    select(id, sdh_number, sdh_interval, sdh_duration_mins, sdh_nadir, sdh_night_status) %>%
    mutate_if(is.numeric, round, digits = 1)


  #Return final output - 1 row per event per individual
  return(sdh_map = data.frame(sdh_map))

}

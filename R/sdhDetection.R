#' @title sdhDetection
#'
#' @param df A dataframe of CGM in which hypoglycaemic episodes will be detected. Must have columns ID, CGM_Timestamp, Gl.
#' @param DetectionLimit  The glucose value used to detect hypoglycaemia
#' @param DetectionDuration The amount of time an individual needs to be at or below the DetectionLimit for a hypolgycaemic episode to be detected
#'
#' @return A dataframe with one hypoglycaemic episode per row with characteristics in each column
#' @export
#'
#' @examples
sdhDetection <- function(df, DetectionLimit, DetectionDuration){

  cgm <-  df %>%
    group_by(ID) %>%
    mutate(Time_of_day = ifelse(as.numeric(format(CGM_Timestamp, "%H"))<6 & as.numeric(format(CGM_Timestamp, "%H"))>=0, "Night", "Day")) %>%
    select(ID, Time_of_day, CGM_Timestamp, Gl, diabetes_type) %>%
    mutate(below_threshold = case_when(Gl < DetectionLimit ~ 1,
                                       Gl >= DetectionLimit ~ 0),
           seq_event = rleid(below_threshold))

  sdh_wide <- cgm %>%
    #select(-Sleep_status, -Motif_hypo_timestamp, -Checkin_hypo_timestamp) %>%
    mutate(last_timestamp = last(CGM_Timestamp)) %>%
    group_by(ID, seq_event, below_threshold,last_timestamp) %>%
    summarise(start_seq = first(CGM_Timestamp)) %>%
    group_by(ID, last_timestamp) %>%
    mutate(end_seq = lead(start_seq)) %>%
    transform(end_seq = if_else(is.na(end_seq), last_timestamp, end_seq)) %>%
    mutate(interval_seq = as.interval(start_seq, end_seq),
           duration_seq= time_length(interval_seq, unit = "min")) %>%
    select(-last_timestamp)

  for (i in 1:nrow(sdh_wide)){
    if(isTRUE(sdh_wide$below_threshold[i]==1) & isTRUE(sdh_wide$duration_seq[i]>=DetectionDuration)){
      sdh_wide$sdh_grouped[i] <- 1
    }else if(isTRUE(sdh_wide$below_threshold[i-1]==1) & isTRUE(sdh_wide$duration_seq[i-1]>=DetectionDuration) & isTRUE(sdh_wide$below_threshold[i]==0) & isTRUE(sdh_wide$duration_seq[i]<=DetectionDuration) & isTRUE(sdh_wide$below_threshold[i+1]==1)){
      sdh_wide$sdh_grouped[i] <- 1
    }else if(isTRUE(sdh_wide$below_threshold[i]==1) & isTRUE(sdh_wide$duration_seq[i]<DetectionDuration) & isTRUE(sdh_wide$below_threshold[i-1]==0) & isTRUE(sdh_wide$duration_seq[i-1]<=DetectionDuration) & isTRUE(sdh_wide$below_threshold[i-2]==1) & isTRUE(sdh_wide$duration_seq[i-2]>=DetectionDuration)){
      sdh_wide$sdh_grouped[i] <- 1
    } else if(isTRUE(is.na(sdh_wide$below_threshold[i])=="TRUE")){
      sdh_wide$sdh_grouped[i] <- NA
    } else if(isTRUE(sdh_wide$below_threshold[i]==0) & isTRUE(sdh_wide$duration_seq[i]<=DetectionDuration) & isTRUE(sdh_wide$sdh_grouped[i-1]==1) & isTRUE(sdh_wide$sdh_grouped[i+1]==1)){
      sdh_wide$sdh_grouped[i] <-1
    } else if(isTRUE(sdh_wide$below_threshold[i]==0) & isTRUE(sdh_wide$duration_seq[i]<=DetectionDuration) & isTRUE(sdh_wide$sdh_grouped[i-1]==1) & isTRUE(sdh_wide$below_threshold[i+1]==1)) {
      sdh_wide$sdh_grouped[i] <-1
    } else if(isTRUE(sdh_wide$below_threshold[i]==1) & isTRUE(sdh_wide$duration_seq[i]<DetectionDuration) & isTRUE(sdh_wide$sdh_grouped[i-2]==1) & isTRUE(sdh_wide$below_threshold[i-1]==0) & isTRUE(sdh_wide$duration_seq[i-1]<=DetectionDuration)){
      sdh_wide$sdh_grouped[i] <-1
    } else{
      sdh_wide$sdh_grouped[i] <- 0
    }
  }

  test <- full_join(cgm, sdh_wide, by = c("ID", "seq_event", "below_threshold"))

  test <- test %>%
    group_by(ID) %>%
    mutate(sdh_seq = ifelse(sdh_grouped==1, rleid(sdh_grouped), NA)) %>%
    group_by(ID, sdh_seq, sdh_grouped) %>%
    mutate(sdh_interval = ifelse(!is.na(sdh_seq), as.interval(first(start_seq), last(end_seq)), NA_real_), #note if_else here, as opposed to ifelse will preserve the interval format and not coerce it to numeric
           sdh_duration = ifelse(!is.na(sdh_seq), time_length(sdh_interval, unit = "mins"), NA),
           sdh_nadir = ifelse(!is.na(sdh_seq), min(Gl), NA))
  print("Got to the end of defining sdh characteristics in long format df: nadir, duration")

  test <- test %>%
    group_by(ID, sdh_seq) %>%
    mutate(timeofday_change_flag = case_when(!is.na(sdh_seq) & Time_of_day == first(Time_of_day) ~ 0,
                                             !is.na(sdh_seq) & Time_of_day !=lag(Time_of_day) ~ 1,
                                             !is.na(sdh_seq) & Time_of_day == lag(Time_of_day) ~ 0,
                                             is.na(sdh_seq) ~ NA_real_))

  test <- test %>%
    group_by(ID) %>%
    mutate(sdh_night_status = ifelse(!is.na(sdh_seq), Time_of_day, NA)) %>%
    group_by(ID, sdh_seq) %>%
    mutate(group_night_status = ifelse(!is.na(timeofday_change_flag), sum(timeofday_change_flag), NA)) %>%
    #essentially this will flag if no change in status then will be 0s if change in status the sum will be 1 for that sdh!
    transform(sdh_night_status = ifelse(group_night_status == 0, sdh_night_status, "Overlap"))

  test <- test %>%
    group_by(ID, sdh_seq, sdh_night_status) %>%
    mutate(night_status_order = rleid(Time_of_day)) %>% #this is to ensure I'm capturing times were there is more than 1 sleep status change e.g. going from sleep to awake to sleep again
    group_by(ID, sdh_seq, sdh_night_status, night_status_order, Time_of_day) %>%
    mutate(timing_auc=ifelse(!is.na(sdh_seq), c(1:n()), NA_real_), #just creating this variable to have numerical coordinates for the trapz function as it does not accept a POSIXct object
           Gl_level_auc = ifelse(!is.na(sdh_seq), Gl-3, NA_real_),#this is to shift the area we're interested in by the glucose level thresholds - i.e. because trapz will work out the area between the maxima and the x axis, we need to shift this area to the x-axis (i.e. by substracting by the threshold)
           AUC_below =  -trapz(timing_auc, Gl_level_auc)) %>% #this calculates the AUC below the 3.9 threshold (in reality below the x-axis)- as we are looking at the area below need to add a negative sign otherwise the area will be negative (note if we were looking at the area above, there would have been no need for the minus sign)
    group_by(ID, sdh_seq, sdh_night_status) %>% #note about the negative sign above - I still get negative aucs I think mainly because sometimes glucose goes above the threshold within the time interval so ends up with area above the curve as well - which would mean in that case the area above the threshold is greater than the one under? so no need for absolute value in reality?
    mutate(sdh_auc_no_overlap = ifelse(sdh_night_status!="Overlap", AUC_below, NA_real_),
           sdh_auc_day = ifelse(sdh_night_status=="Overlap", #determining the AUC for overlapping sdh in their awake stage
                                sum(unique(AUC_below[Time_of_day=="Day"])), #note I've added the sum here in case there are more than 1 sleep status change!
                                NA_real_),
           sdh_auc_night = ifelse(sdh_night_status=="Overlap", #determining the AUC for overlapping sdhs in their asleep stage
                                  sum(unique(AUC_below[Time_of_day=="Night"])),
                                  NA_real_))

  map <- test %>%
    filter(sdh_grouped==1) %>%
    group_by(ID, sdh_seq, sdh_grouped, sdh_interval, sdh_duration, sdh_nadir, sdh_auc_no_overlap,
             sdh_night_status, sdh_auc_day, sdh_auc_night, diabetes_type) %>%
    summarise() %>%
    ungroup() %>%
    group_by(ID) %>%
    mutate(sdh_number = rleid(sdh_seq),
           overall_sdh_night_status = ifelse(is.na(sdh_night_status), NA_character_,#that's needed when there is no sdh e.g. sdh 2.2
                                             case_when(sdh_night_status!="Overlap" ~ sdh_night_status,
                                                       sdh_night_status=="Overlap" & sdh_auc_night > sdh_auc_day ~ "Night",
                                                       sdh_night_status=="Overlap" & sdh_auc_night < sdh_auc_day ~ "Day"))) %>%
    select(ID, sdh_number, sdh_interval, sdh_duration, sdh_nadir, sdh_auc_no_overlap,
           sdh_night_status, sdh_auc_day, sdh_auc_night, overall_sdh_night_status, sdh_seq, diabetes_type) %>%
    #mutate(awareness_status = .self$baseline_hypo_awareness$Gold,
    #   diabetes_status = .self$baseline_characteristics$diabetes_type) %>%
    #transform(awareness_status = ifelse(awareness_status>=4, "IAH", "NAH")) %>%
    mutate_if(is.numeric, round, digits = 1)

  return(map = data.frame(map))

}

#### 1) SLEEP WITH CGM ####

#' sleep_df_modified <- .self$sleep_combined %>%
#'   #select(-levels) %>% #as levels is a df in itself and was creating issues when doing the non-equi join with DT
#'   mutate(Sleep_status = ifelse(!is.na(logId), "Asleep", NA_character_)) #so that we get the sleep status defined stragiht away - will be NA when the night was missing
#'
#' vec <- .self$CGM_PRH$CGM_Timestamp #creating a vector of the length of CGM timestamps to determine which ones fall within a sleep interval
#'
#' combine <- setDT(sleep_df_modified)[data.table(vec), on = .(startTime <= vec, endTime >= vec)] %>% # using the method as indicated in the link:https://stackoverflow.com/questions/41132081/find-which-interval-row-in-a-data-frame-that-each-element-of-a-vector-belongs-in
#'   #essentially you end up with a df of the same length as your vector (i.e. as your cgm prh dataframe with your sleep data on the rows where your cgm timestamps fall within the sleep intervals - it will be NA otherwise)
#'   select(Hypometrics_ID, dateOfSleep, logId, startTime, Sleep_status) %>%#selecting the variables we are really interested in
#'   rename(CGM_Timestamp = startTime) %>% #renaming here as Start Time and end Time here will be identifcal and both correspond to CGM timestamps - please check out the link if you are confused
#'   distinct(CGM_Timestamp, .keep_all = T) %>%  #this is to remove extra rows when there are multiple missing nights in a row - it creates duplicates in the CGM_Timestamp(one for each missing night in a row)
#'   transform(Sleep_status = ifelse(CGM_Timestamp >= first(sleep_df_modified$startTime) & CGM_Timestamp <= last(sleep_df_modified$endTime) & is.na(dateOfSleep) & is.na(logId),
#'                                   "Awake", Sleep_status)) #here we are transforming the sleep status if the CGM timestamp is not before the 1st night we have sleep info and not after the last night and when both date of sleep and log id are missing (i.e. outside of the boundaries determined in the sleep df - otherwise it it's just a missing night, log ID will be bmissing but not date of sleep)
#'
#' combine <- combine %>%
#'   transform(Hypometrics_ID = c(paste("HypoM", .self$patientNumber, sep = ""))) %>% #just because the Hypometrics ID will be NA on the rows where the CGM timestamp is not falling within a sleep interval so just making sure there are no missing IDs
#'   select(Hypometrics_ID, CGM_Timestamp, Sleep_status)
#' print("Got to the end of prepping the data frame that will be used to return sleep status to the CGM data frame")
#'
#' .self$CGM_PRH <- full_join(.self$CGM_PRH, combine) %>%
#'   select(Hypometrics_ID, Sleep_status, everything())
#'
#' #### 2) ACTIVITY WITH CGM ####
#'
#'   if((.self$summary_files$has_step_count_data == "No" & .self$summary_files$has_heart_rate_data == "No") | .self$summary_files$has_cgm_data == "No"){
#'     print("Participant has no fitbit heart rate/step count OR CGM data so cgm+fitbit metrics will not be produced")
#'
#'   } else if(last(.self$steps$Step_Timestamp) < first(.self$CGM_PRH$CGM_Timestamp) & last(.self$heart$HR_Timestamp) < first(.self$CGM_PRH$CGM_Timestamp) | (nrow(.self$steps)==0 & nrow(.self$heart)==0)){#Here adding a condition for those with steps and heart data that falls within the study period but are before the first cgm timestamps (e.g. 03090) so unusable in the context of the cgm fitbit metrics
#'     #here added as a different else if otherwise is partiapnts has no step data or heart rate the function is not able to evaluate this condition
#'     #the nrow condition is about people who have step count data but because all fall outside the study window the steps and heart rate data frames end up being empty (e.g. 07069)
#'     print("Participant has all step and heart falling before the first cgm timestamp OR all step and heart rate data points fall outside study window so cgm+fitbit metrics will not be produced")
#'   } else{
#'     print("Participant has CGM+fitbit heart rate/step count data so further data processing is starting")
#'
#'     ##Retrieving CGM dataframe reference
#'     cgm <- .self$CGM_PRH %>%
#'       #select(-Checkin_hypo_timestamp, -Motif_hypo_timestamp, -Matched_timestamp) %>%
#'       select(-Checkin_hypo_timestamp, -Motif_hypo_timestamp) %>%
#'       mutate(interval_number = rleid(CGM_Timestamp),
#'              time_interval = lubridate::interval(CGM_Timestamp, lead(CGM_Timestamp)))
#'     cgm <- as.data.frame(cgm)
#'     print("Got to the end of retrieving the cgm data frame that will serve as the reference for timings")
#'
#'     ##Retrieving step count data
#'     .self$steps_for_cgm <- .self$steps %>%
#'       filter(Step_Timestamp %within% lubridate::interval(first(cgm$CGM_Timestamp), #filtering specifically to match the CGM dataframe
#'                                                          last(cgm$CGM_Timestamp))) %>%
#'       complete(Step_Timestamp = seq(min(Step_Timestamp), max(Step_Timestamp), by=60)) %>% #this will identify the missing minutes of step count data
#'       transform(Hypometrics_ID = c(paste("HypoM", .self$patientNumber, sep = ""))) %>% #adding that here because the complete function will also create NAs in Hypometrics ID when step count is missing
#'       select(Hypometrics_ID, everything())
#'
#'
#'     .self$steps_for_cgm$interval_number <- findInterval(.self$steps_for_cgm$Step_Timestamp, cgm$CGM_Timestamp)#this will look through every single step timestamp to determine which cgm timestamp interval it falls within - which will help for my merging
#'     # .self$steps_for_cgm$interval_number <- sapply(.self$steps_for_cgm$Step_Timestamp, function(x){ #this was the original idea but takes way too long to loop through the step timestamps
#'     #   cgm$interval_number[ x >= cgm$start_int & x < cgm$end_int]
#'     # })
#'
#'     ##Creating a smaller step dataframe with summarised values by interval which will be used to merge with the reference cgm dataframe
#'     .self$steps_summary <- .self$steps_for_cgm %>%
#'       group_by(Hypometrics_ID, interval_number) %>%#'[PLEASE NOTE IT MAY BE AN OPTION TO ROLL NEAREST HERE GIVEN THE GRANULARITY OF THE DATA - INITIALLY DID THE SUM BECAUSE CGM WAS AT THE 5-MIN LEVEL AND FITBIT AT THE 1 MIN LEVEL - 23/01/23 tried it - it works but would need a little more time to work on it as still returns duplicates in the cgm_Timestamp matching with the same hr timestamp so do you need to do the sum of step count after grouping by?]
#'       #'[And also isnt it a simpler to say that between those two cgm timestamps (i.e. in this cgm_interval), the person has walked X number of steps based on fitbit data? i.e. code below]
#'       #summarise(sum_steps_by_int = case_when(n()==1 & is.na(count) ~ NA_real_,# given the data is now at the 1-min level this ensures that if count is missing and there is only one row of data then count is marked as missing
#'       #TRUE ~ sum(count, na.rm=T)), #in any other case can ignore the missingness when  summing the steps (counting (if I didnt add the line above, it would ignore missingness even where there is only 1 minute of data and the step count would therefore be 0 even if missing in the original data frame)
#'       #Getting rid of the two lines above as they were creating duplicates in the cgm_fitbit metrics data (when you had more than 1 cgm timestamps falling within the cgm interval)
#'       summarise(sum_steps_by_int = sum(count, na.rm=T), #this will count the number of steps by interval . if number is na it will count as 0 but with the other two columns created I will know if all timestamps are missing or not
#'                 n_step_timestamps_in_int = n(), #this will highlight number of fitbit steps timestamps that fall within the cgm interval
#'                 n_missing_min_steps_by_int = sum(is.na(count)))#this will highlight the number of missing step data i.e. timestamp with no step data that fall within the interval
#'     print("Made it to the end of prepping the step data for a merge with the CGM data frame")
#'
#'     ##Retrieving heart rate data
#'     .self$heart_for_cgm <- .self$heart %>%
#'       filter(HR_Timestamp %within% lubridate::interval(first(cgm$CGM_Timestamp),
#'                                                        last(cgm$CGM_Timestamp))) %>%
#'       group_by(Hypometrics_ID, HR_Timestamp=format(HR_Timestamp, "%Y-%m-%d %H:%M")) %>% #reformatting so I get a min-by-min average similarly to the step count data
#'       summarise(heart_rate=mean(heart_rate)) %>%
#'       transform(heart_rate = round(heart_rate, digits=0),
#'                 HR_Timestamp =ymd_hm(HR_Timestamp)) %>%
#'       complete(HR_Timestamp = seq(min(HR_Timestamp), max(HR_Timestamp), by=60)) %>%
#'       transform(Hypometrics_ID = c(paste("HypoM", .self$patientNumber, sep = ""))) %>% #adding that here because the complete function will also create NAs in Hypometrics ID when step count is missing
#'       select(Hypometrics_ID, everything())
#'
#'     .self$heart_for_cgm$interval_number <- findInterval(.self$heart_for_cgm$HR_Timestamp, cgm$CGM_Timestamp)#THIS IS REALLY QUICK ~0.05 sec (hence preferable to apply methods)
#'     # .self$heart_for_cgm$interval_number <- sapply(.self$heart_for_cgm$HR_Timestamp, function(x){#this will look through every single HR timestamp to determine which cgm timestamp interval it falls within - which will help for my merging
#'     #   cgm$interval_number[ x >= cgm$start_int & x < cgm$end_int]
#'     # })
#'
#'     ##Creating a smaller heart rate dataframe with summarised values by interval which will be used to merge with the reference cgm dataframe
#'     .self$heart_summary <- .self$heart_for_cgm %>%
#'       group_by(Hypometrics_ID, interval_number) %>%
#'       summarise(mean_hr_by_int = mean(heart_rate, na.rm=T),
#'                 n_hr_timestamps_in_int = n(), #see explanation in steps_summary
#'                 n_missing_min_hr_by_int = sum(is.na(heart_rate))) %>%
#'       transform(mean_hr_by_int = round(mean_hr_by_int, digits=0),
#'                 interval_number = as.numeric(interval_number))
#'     print("Made it to the end of prepping the heart rate data for a merge with the CGM data frame")
#'
#'     ##Combining the cgm, step count and heart rate data in one data frame
#'     .self$cgm_fitbit_metrics <- list(cgm, .self$steps_summary, .self$heart_summary) %>%
#'       reduce(left_join) %>%
#'       select(Hypometrics_ID, Sleep_status, CGM_Timestamp, Gl,
#'              interval_number, time_interval,
#'              sum_steps_by_int, n_step_timestamps_in_int, n_missing_min_steps_by_int,
#'              mean_hr_by_int, n_hr_timestamps_in_int, n_missing_min_hr_by_int)
#'     print("Got to the end of combining CGM and fitbit metrics")
#'
#' }

na_check = function(.self, ...){


  if(.self$summary_files$has_cgm_data=="No"){
    print("Participant has no CGM data so CGM data not complete")

    Hypometrics_ID <- paste0("HypoM", .self$patientNumber)
    .self$CGM_completeness_check <- data.frame(Hypometrics_ID) %>%
      mutate(total_cgm_hours_per_day = c(0)) #because participant has no CGM data

    column_names <- c(Hypometrics_ID = NA_character_, CGM_Timestamp = NA_POSIXct_, Gl = NA_real_,
                      total_cgm_hours_per_day = NA_real_)
    .self$CGM_completeness_check <- add_column(.self$CGM_completeness_check, !!!column_names[setdiff(names(column_names), names(.self$CGM_completeness_check))])
    #here adding extra columns so that the merge is possible later on

  } else{

    print("Participant has CGM data and doing the cgm data completeness check")

    .self$CGM_completeness_check <- .self$CGM_explicitgaps_final %>%
      mutate(Date = as.Date(CGM_Timestamp)) %>%
      group_by(Date) %>%
      mutate(gluc_complete = ifelse(!is.na(Gl) | !is.na(lag(Gl)), 1, 0), #this ensures we get the full duration of when CGM data is non-missing - identifies the timestamp up until when CGM-data isn't missing
             seq = ifelse(gluc_complete==1, rleid(gluc_complete), NA_real_)) %>%
      group_by(Date, seq) %>%
      mutate(time_diff = ifelse(!is.na(seq),
                                time_length(as.interval(lag(CGM_Timestamp), CGM_Timestamp), unit = "hour"),
                                NA_real_)) %>%
      group_by(Date) %>%
      mutate(total_cgm_hours_per_day = sum(time_diff, na.rm=T)) %>%
      select(-c(gluc_complete, seq, time_diff))
    print("Got to the end of working out the number of non-missing CGM data per day")
  }

  if(.self$summary_files$has_fitbit_data=="No"){
    print("Participant has no fitbit data so sleep data not complete")

    Hypometrics_ID <- paste0("HypoM", .self$patientNumber)
    .self$sleep_completeness_check <- data.frame(Hypometrics_ID)

    column_names <- c(Hypometrics_ID = NA_character_, dateOfSleep = NA_Date_)
    .self$sleep_completeness_check <- add_column(.self$sleep_completeness_check, !!!column_names[setdiff(names(column_names), names(.self$sleep_completeness_check))])
    #here adding extra columns so that the merge is possible later on (based on Hypometrics_ID and date)


  } else{
    print("Participant has fitbit and further processing steps are starting")

    .self$sleep_completeness_check <- .self$sleep_combined[,c("Hypometrics_ID", "dateOfSleep", "logId")]
    .self$sleep_completeness_check <- .self$sleep_completeness_check %>%
      transform(Hypometrics_ID = c(paste("HypoM", .self$patientNumber, sep = "")),
                dateOfSleep = ymd(dateOfSleep)) %>%
      mutate(Date = dateOfSleep) %>%
      distinct(dateOfSleep, .keep_all = T) %>% #to get rid of the instances where the patients slept then woke up, creating two entries for that night
      filter(!is.na(logId)) %>%
      select(-logId)
    print("Got to the end of prep the sleep df")
  }

  .self$AM_completeness_check <- .self$AM_original["localTimestamp"]
  .self$AM_completeness_check <- .self$AM_completeness_check %>%
    mutate(Hypometrics_ID = c(paste("HypoM", .self$patientNumber, sep = ""))) %>%
    select(Hypometrics_ID, localTimestamp) %>%
    rename(AM_localTimestamp = localTimestamp) %>%
    transform(AM_localTimestamp = ymd_hms(AM_localTimestamp)) %>%
    mutate(Date = as.Date(AM_localTimestamp)) %>%
    distinct(Date,  .keep_all = T) #just in case there's something weird going on with the duplicate entries etc
  print("Got to the end of prep the AM df")

  .self$AFT_completeness_check <- .self$AFT_original["localTimestamp"]
  .self$AFT_completeness_check <- .self$AFT_completeness_check %>%
    mutate(Hypometrics_ID = c(paste("HypoM", .self$patientNumber, sep = ""))) %>%
    select(Hypometrics_ID, localTimestamp) %>%
    rename(AFT_localTimestamp = localTimestamp) %>%
    transform(AFT_localTimestamp = ymd_hms(AFT_localTimestamp)) %>%
    mutate(Date = as.Date(AFT_localTimestamp)) %>%
    distinct(Date,  .keep_all = T)
  print("Got to the end of prep the AFT df")

  .self$PM_completeness_check <- .self$PM_original["localTimestamp"]
  .self$PM_completeness_check <- .self$PM_completeness_check %>%
    mutate(Hypometrics_ID = c(paste("HypoM", .self$patientNumber, sep = ""))) %>%
    select(Hypometrics_ID, localTimestamp) %>%
    rename(PM_localTimestamp = localTimestamp) %>%
    transform(PM_localTimestamp = ymd_hms(PM_localTimestamp)) %>%
    mutate(Date = as.Date(PM_localTimestamp)) %>%
    distinct(Date,  .keep_all = T)
  print("Got to the end of prep the PM df")

  .self$motif_completeness_check <- .self$motif_original[,c("uMotifTime", "EntryDate")]

  #Below we are fixing the fact that the columns in the motif df do not behave consistently for iPhone vs Android users (see Excel file if you don't remember what it's all about)
  setwd(paste(localPath, .self$patientNumber, "/Unzipped uMotif/" , sep = ""))
  consent_form <- read.csv(list.files(pattern = "form-econsent\ .csv"), header=T)

  #adding here a condition for when the consent form file is empty (probably some bug that happened for 03049)
  if(nrow(consent_form)==0){
    consent_form <- read.csv(list.files(pattern = "form-hm-afternoon-checkin\ .csv"), header=T)["platform"]#will fetch the platform used from the afternoon questionnaire instead
  }

  if(all(grepl("iPhone|iPad", consent_form$platform))){#here we are trying to cover the instances when there are multiple rows in the content data frame e.g. when the participant had issues when registrating. here the condition if all the rows indicate it's an iPhone than apply the change
    .self$motif_completeness_check <-  .self$motif_completeness_check %>%
      transform(uMotifTime = if_else(ymd_hms(EntryDate)<"2022-01-21 00:00:00", EntryDate, uMotifTime))
    #see excel file for further explanation but this should take care of the fact that the time in the columns with the iphones is wrong, even more so in the EU
    #Please note the date is likely to change but based on the participants I have so far (n=312), the fix seemed to have been done and effective from 21/01/2022
    #Yes - still strue on 26/05/2022 with n=380 participants
    print("iPhone/iPad user - uMotif time changed where required")
  } else if(all(!grepl("iPhone|iPad", consent_form$platform))){#here the condition is if all the rows are not an iphone i.e. it was only an android used at registration (this way I can flag out when participants used two different devices when registering)
    print("Android user - uMotif time unchanged")
  }

  .self$motif_completeness_check <- .self$motif_completeness_check %>%
    mutate(Hypometrics_ID = c(paste("HypoM", .self$patientNumber, sep = ""))) %>%
    select(Hypometrics_ID, uMotifTime) %>%
    transform(uMotifTime = ymd_hms(uMotifTime)) %>%
    mutate(Date = as.Date(uMotifTime)) %>%
    distinct(Date, .keep_all = T)
  print("Got to the end of prep motif complete data df")

  .self$overall_completeness_check_long <-
    list(.self$study_days_reference, .self$CGM_completeness_check, .self$sleep_completeness_check, .self$AM_completeness_check, .self$AFT_completeness_check, .self$PM_completeness_check, .self$motif_completeness_check) %>%
    reduce(left_join) #left join here so we only keep the relevant study days
  print("Got to the end of combining all dfs to check for completeness - Long format")


  .self$overall_completeness_check_wide <- .self$overall_completeness_check_long %>%
    group_by(Hypometrics_ID, Study_day, Date, total_cgm_hours_per_day, AM_localTimestamp, AFT_localTimestamp, PM_localTimestamp, uMotifTime, dateOfSleep) %>%
    summarise()
  print("Got to the end of changing format to wide - 1 line per day")


  .self$overall_completeness_check_wide <- .self$overall_completeness_check_wide %>%
    ungroup() %>%
    mutate(valid_day = case_when(total_cgm_hours_per_day>=16.8 & !is.na(AM_localTimestamp) & !is.na(PM_localTimestamp) ~ 1, #note:  16.8 being at least 70% of cgm data on that day, and also got rid of the condition on sleep status following discussion with Paddy
                                 total_cgm_hours_per_day>=16.8 & !is.na(uMotifTime) ~ 1,
                                 TRUE ~ 0)) %>%
    mutate(valid_cgm_day = ifelse(total_cgm_hours_per_day<16.8, NA_real_, total_cgm_hours_per_day),
           total_valid_days = sum(valid_day))
  print("Got to the end of defining our valid days")

  .self$CGM_NA_distribution_plot <-
    ggplot_na_distribution(x = .self$overall_completeness_check_long$Gl,
                           x_axis_labels = .self$overall_completeness_check_long$CGM_Timestamp,
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
    scale_x_datetime(date_labels = "%d-%m-%Y",
                     date_breaks = "1 day",
                     expand = c(0,0)) +
    scale_y_continuous(#limits = c(0, 30),
      breaks = c(2.2, 3.9, 10, 20),
      expand=c(0,0)) +
    theme(axis.text.x = element_text(angle = 90), panel.grid.minor = element_blank(), panel.grid.major.y = element_blank())
  print("Got to the end of defining our NA_distribution_plot")

  .self$CGM_NA_percent_plot <-
    ggplot_na_intervals(.self$overall_completeness_check_long$Gl,
                        interval_size = 288, #this is because there should be about 288 timestamps per day so that means that R looks for blocks of 288 observations (24h blocks) and determine % missing data within that window
                        title = "Gaps per day",
                        xlab = "Study day") +
    scale_x_continuous(breaks = seq(288, 20160, 288),
                       labels = c(1:70),
                       expand=c(0,0)) +
    theme(axis.text.x = element_text(hjust = 1.3, vjust = 2))
  print("Got to the end of defining our plot showing %of missing data per day")

  .self$CGM_NA_pattern_plot <-
    ggplot_na_gapsize(.self$overall_completeness_check_long$Gl,
                      title = "Occurence of gaps sizes in CGM data",
                      include_total=F,
                      limit=50) +
    scale_y_continuous(breaks = seq(0, 50, 2),
                       expand=c(0,0))
  print("Got to the end of defining the pattern of missingness plot")


}

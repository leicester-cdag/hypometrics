heart_rate = function(.self, ...){

  #if(.self$summary_files$has_fitbit_data == "No"){
  if(.self$summary_files$has_fitbit_data == "No"){
    print("Participant has no fitbit  data so heart rate data will not be processed")
  } else{
    print("Participant has fitbit data so heart rate data processing is starting")

    if(file.exists(paste(localPath,  .self$patientNumber, "/Unzipped Fitbit", sep=""))){#trying this to see if it helps with the error messages that come up after unzipping the files
      print("Unzipped fitbit folder already exists")
      setwd(paste(localPath,  .self$patientNumber, "/Unzipped Fitbit", sep=""))
      fitbit_files <- list.files(recursive = T)
    } else{
      setwd(paste(localPath, .self$patientNumber, sep=""))
      fitbit_files <- utils::unzip(list.files(pattern = "fitbit.*hypom|hypom.*fitbit", ignore.case = T), list=F, exdir = paste(localPath, .self$patientNumber, "/Unzipped Fitbit", sep = ""))
    }

    if(.self$summary_files$has_heart_rate_data =="Yes"){
      print("participant has heart rate data so it will be processed")

      heart_files <- fitbit_files[grepl("/heart_rate-", fitbit_files)]

      .self$heart_rate_original <- rbind_pages(lapply(heart_files, fromJSON))
      .self$heart_rate_original <- jsonlite::flatten(.self$heart_rate_original) %>% #the flatten package was conflicting with purrr flatten
        mutate(Hypometrics_ID = c(paste0("HypoM", .self$patientNumber))) %>%
        select(Hypometrics_ID, everything()) %>%
        transform(dateTime = mdy_hms(dateTime),
                  value.bpm = as.numeric(value.bpm)) %>%
        filter(as.Date(dateTime) %within% lubridate::interval(.self$admin_data$date_umotif_consent, .self$admin_data$date_umotif_end)) #here filtering the HR data that falls outside the study period window

      print("Got to the end of fetching the original heart rate files")

      .self$heart <- .self$heart_rate_original %>%
        rename(heart_rate = value.bpm,
               HR_Timestamp = dateTime) #%>%
      # mutate(Hypometrics_ID = c(paste("HypoM", .self$patientNumber, sep = ""))) %>%
      # select(Hypometrics_ID, everything()) %>%
      #filter(as.Date(HR_Timestamp) %within% lubridate::interval(.self$admin_data$date_umotif_consent, .self$admin_data$date_umotif_end)) #here filtering the HR data that falls outside the study period window

      print("Made it to the end cleaning heart rate data")
    }

    if(.self$summary_files$has_heart_rate_zones_data =="Yes"){
      print("participant has heart rate zones data so it will be processed")

      time_in_hr_zones_files <- fitbit_files[grepl("time_in_heart_rate_zones", fitbit_files)]

      .self$hr_zones_original <- rbind_pages(lapply(time_in_hr_zones_files, fromJSON)) %>%#Consider flattening?
        mutate(Hypometrics_ID = c(paste0("HypoM", .self$patientNumber))) %>%
        select(Hypometrics_ID, everything()) %>%
        transform(dateTime = mdy_hms(dateTime)) %>%
        filter(as.Date(dateTime) %within% lubridate::interval(.self$admin_data$date_umotif_consent, .self$admin_data$date_umotif_end)) #here filtering the HR data that falls outside the study period window

      print("Got to the end of fetching the original time in heart rate zones files")
    }

    if(.self$summary_files$has_hr_variability_summary_data=="Yes"){
      print("participant has hr variability summary data so it will be processed")

      hr_variability_summary_files <- fitbit_files[grepl("heart rate variability summary.*csv", fitbit_files, ignore.case = T)]

      .self$hr_variability_summary_original <- vroom(hr_variability_summary_files) %>%
        mutate(Hypometrics_ID = c(paste0("HypoM", .self$patientNumber))) %>%
        select(Hypometrics_ID, everything()) %>%
        filter(as.Date(timestamp) %within% lubridate::interval(.self$admin_data$date_umotif_consent, .self$admin_data$date_umotif_end))

      print("Got to the end of fetching the original hr variability summary file")

    }

    if(.self$summary_files$has_hr_variability_details_data=="Yes"){
      print("participant has hr variability details data so it will be processed")

      hr_variability_details_files <- fitbit_files[grepl("heart rate variability details.*csv", fitbit_files, ignore.case = T)]

      .self$hr_variability_details_original <- vroom(hr_variability_details_files) %>%
        mutate(Hypometrics_ID = c(paste0("HypoM", .self$patientNumber))) %>%
        select(Hypometrics_ID, everything()) %>%
        transform(timestamp = ymd_hms(timestamp)) %>%
        filter(as.Date(timestamp) %within% lubridate::interval(.self$admin_data$date_umotif_consent, .self$admin_data$date_umotif_end)) #here filtering the HR data that falls outside the study period window

      print("Got to the end of fetching the original hr variability details file")

    }

  }
}

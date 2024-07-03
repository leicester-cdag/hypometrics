#' #### 1) Note to Jonah: Possible to turn this into a function to simply read in fitbit data as located in multilple files
#' #and tend to be quite messy naturally? Don't worry if you dont think it's necessary
#'
#' activity = function(.self, ...){
#'
#'   if(.self$summary_files$has_fitbit_data == "No"){
#'     print("Participant has no fitbit data so activity data will not be processed")
#'   } else{
#'     print("Participant has fitbit data so activity data processing is starting")
#'
#'     if(file.exists(paste(localPath,  .self$patientNumber, "/Unzipped Fitbit", sep=""))){#trying this to see if it helps with the error messages that come up after unzipping the files
#'       print("Unzipped fitbit folder already exists")
#'       setwd(paste(localPath,  .self$patientNumber, "/Unzipped Fitbit", sep=""))
#'       fitbit_files <- list.files(recursive = T)
#'     } else{#HERE i Could add a condition for when I'm processing it for the first time and specify how many fitbit files are available but shouldnt need to cos technically sleep step will always unzip it first
#'       setwd(paste(localPath, .self$patientNumber, sep=""))
#'       fitbit_files <- utils::unzip(list.files(pattern = "fitbit.*hypom|hypom.*fitbit", ignore.case = T), list=F, exdir = paste(localPath, .self$patientNumber, "/Unzipped Fitbit", sep = ""))
#'     }
#'
#'
#'     ##Step count
#'     if(.self$summary_files$has_step_count_data == "Yes"){
#'       print("Participant has step count data so step count data will  be processed")
#'
#'       step_files <- fitbit_files[grepl("steps", fitbit_files)]
#'
#'       .self$step_count_original <- rbind_pages(lapply(step_files, fromJSON)) %>%
#'         mutate(Hypometrics_ID = c(paste0("HypoM", .self$patientNumber))) %>%
#'         select(Hypometrics_ID, everything()) %>%
#'         transform(dateTime = mdy_hms(dateTime),
#'                   value = as.numeric(value)) %>%
#'         filter(as.Date(dateTime) %within% lubridate::interval(.self$admin_data$date_umotif_consent, .self$admin_data$date_umotif_end)) #here filtering the step data that falls outside the study period window
#'
#'       print("Got to the end of fetching the original step count file")
#'
#'       .self$steps <- .self$step_count_original %>%
#'         rename(count = value,
#'                Step_Timestamp = dateTime) #%>%
#'       # mutate(Hypometrics_ID = c(paste("HypoM", .self$patientNumber, sep = ""))) %>%
#'       # select(Hypometrics_ID, everything()) %>%
#'       #filter(as.Date(Step_Timestamp) %within% lubridate::interval(.self$admin_data$date_umotif_consent, .self$admin_data$date_umotif_end)) #here filtering the step data that falls outside the study period window
#'       print("Made it to the end of cleaning the step count data")
#'     }
#'     #'*please note you get a data frame with totla number of minutes spent being 'sedentary', 'lightly active', etc per day*
#'     #'*When working on  those data frames later can merge them all into a df, as if you add up the minutes in each of the acitivty levels you get 1440 (i.e. all the minutes within a day)*
#'     #'Please note the minutes below seems to be included in all fitbit files by default so that's why I'm not adding conditions here
#'     ##Sedentary minutes
#'     sedentary_files <- fitbit_files[grepl("sedentary", fitbit_files)]
#'
#'     .self$sedentary_min_original<- rbind_pages(lapply(sedentary_files, fromJSON)) %>%
#'       mutate(Hypometrics_ID = c(paste0("HypoM", .self$patientNumber))) %>%
#'       select(Hypometrics_ID, everything()) %>%
#'       transform(dateTime = mdy_hms(dateTime)) %>%
#'       filter(as.Date(dateTime) %within% lubridate::interval(.self$admin_data$date_umotif_consent, .self$admin_data$date_umotif_end)) #here filtering the step data that falls outside the study period window
#'     print("Got to the end of fetching the original sedentary minute files")
#'
#'     ##Lightly active minutes
#'     lightly_active_files <- fitbit_files[grepl("lightly", fitbit_files)]
#'
#'     .self$lightly_active_min_original<- rbind_pages(lapply(lightly_active_files, fromJSON)) %>%
#'       mutate(Hypometrics_ID = c(paste0("HypoM", .self$patientNumber))) %>%
#'       select(Hypometrics_ID, everything()) %>%
#'       transform(dateTime = mdy_hms(dateTime)) %>%
#'       filter(as.Date(dateTime) %within% lubridate::interval(.self$admin_data$date_umotif_consent, .self$admin_data$date_umotif_end)) #here filtering the step data that falls outside the study period window
#'
#'     print("Got to the end of fetching the original lightly active minute files")
#'
#'     ##Moderately active minutes
#'     moderately_active_files <- fitbit_files[grepl("moderately", fitbit_files)]
#'
#'     .self$moderately_active_min_original<- rbind_pages(lapply(moderately_active_files, fromJSON)) %>%
#'       mutate(Hypometrics_ID = c(paste0("HypoM", .self$patientNumber))) %>%
#'       select(Hypometrics_ID, everything()) %>%
#'       transform(dateTime = mdy_hms(dateTime)) %>%
#'       filter(as.Date(dateTime) %within% lubridate::interval(.self$admin_data$date_umotif_consent, .self$admin_data$date_umotif_end)) #here filtering the step data that falls outside the study period window
#'
#'     print("Got to the end of fetching the original moderately active minute files")
#'
#'     ##Very active minutes
#'     very_active_files <- fitbit_files[grepl("very", fitbit_files)]
#'
#'     .self$very_active_min_original<- rbind_pages(lapply(very_active_files, fromJSON)) %>%
#'       mutate(Hypometrics_ID = c(paste0("HypoM", .self$patientNumber))) %>%
#'       select(Hypometrics_ID, everything()) %>%
#'       transform(dateTime = mdy_hms(dateTime)) %>%
#'       filter(as.Date(dateTime) %within% lubridate::interval(.self$admin_data$date_umotif_consent, .self$admin_data$date_umotif_end)) #here filtering the step data that falls outside the study period window
#'     print("Got to the end of fetching the original very active minute files")
#'
#'     ##Exercise type
#'     if(.self$summary_files$has_exercise_type_data == "Yes"){
#'       print("participant has exercise type data")
#'
#'       exercise_files <- fitbit_files[grepl("exercise", fitbit_files)] #'*USE script importing all fitbit data into R when you want to process this data as it requires some flattening and unnesting etc*
#'
#'       .self$exercise_type_original <- rbind_pages(lapply(exercise_files, fromJSON)) %>% #'[it works: testing here if even only 1 exercise file (which should be the norm), then it works -as for some people who had excess data (e.g. 01079) I left exercise file as is so you end up with
#'         mutate(Hypometrics_ID = c(paste0("HypoM", .self$patientNumber))) %>%
#'         select(Hypometrics_ID, everything()) %>%
#'         transform(startTime = mdy_hms(startTime)) %>%
#'         filter(as.Date(startTime) %within% lubridate::interval(.self$admin_data$date_umotif_consent, .self$admin_data$date_umotif_end))
#'
#'       print("Got to the end of fetching the original exercise files")
#'     }
#'     ##Active zone minutes
#'     if(.self$summary_files$has_active_zone_minutes_data == "Yes"){
#'       print("participant has active zone minutes data")
#'
#'       active_zone_files <- fitbit_files[grepl("Active Zone", fitbit_files)]#
#'
#'       .self$active_zone_minutes_original <- vroom(active_zone_files) %>%
#'         mutate(Hypometrics_ID = c(paste("HypoM", .self$patientNumber, sep = ""))) %>%
#'         select(Hypometrics_ID, everything()) %>%
#'         transform(date_time = ymd_hms(date_time)) %>%
#'         filter(as.Date(date_time) %within% lubridate::interval(.self$admin_data$date_umotif_consent, .self$admin_data$date_umotif_end))
#'
#'     }
#'
#'   }
#'
#' }

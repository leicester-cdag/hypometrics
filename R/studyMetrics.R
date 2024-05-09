metrics = function(.self, ...) {
  print("getting started with the metrics function")

  if(.self$summary_files$has_cgm_data=="No"){
    print("Participant has no CGM data so CGM metrics won't be produced")

    Hypometrics_ID <- paste0("HypoM", .self$patientNumber)
    .self$key_metrics <- data.frame(Hypometrics_ID)
    df_lig3.9 <- data.frame(Hypometrics_ID)
    df_lig3.0 <- data.frame(Hypometrics_ID)
    df_longlig3.0 <- data.frame(Hypometrics_ID)
    df_lig2.2 <- data.frame(Hypometrics_ID)
    df_longlig2.2 <-data.frame(Hypometrics_ID)



  } else{
    print("Participant has CGM data and further processing steps are starting")

    df_glu <- .self$overall_completeness_check_long %>% #THIS WILL ONLY GET THE TIMESTAMPS FOR THE STUDY DURATION AND NOT EXTRA CGM TIMESTAMPS
      rename(gl = Gl, #getting it in an iglu format so we can get our glucometrics
             id = Hypometrics_ID,
             time = CGM_Timestamp) %>%
      transform(gl = gl * 18) %>%
      select(id, time, gl)

    #1 - getting our key glucose metrics using the functions within the iglu package
    .self$key_metrics <- list(active_percent(df_glu),
                              mean_glu(df_glu), sd_glu(df_glu),
                              median_glu(df_glu), quantile_glu(df_glu, quantiles = c(25,75)),
                              cv_glu(df_glu),
                              in_range_percent(df_glu, target_ranges = list(c(70, 180))),
                              above_percent(df_glu, targets_above = 250),
                              above_percent(df_glu, targets_above = 180),
                              below_percent(df_glu, targets_below = 70),
                              below_percent(df_glu, targets_below = 54)) %>%
      reduce(full_join) %>%
      transform(mean = mean/18,
                SD = SD/18,
                median = median/18,
                `25` = `25`/18,
                `75` = `75`/18) %>%
      mutate(nweeks = ndays/7) %>%
      transform(nweeks = as.numeric(nweeks)) %>%
      select(id, start_date, end_date, nweeks, ndays, active_percent, everything()) %>%
      rename(Hypometrics_ID = id, start_date_cgm = start_date, end_date_cgm = end_date, mean_glu = mean, sd_glu = SD,
             median_glu = median, Q1_glu = X25, Q3_glu = X75, cv_glu = CV, #R automatically adds an 'X' for the quartile columns as they are originally numbers i assume
             percent_in_range_3.9_10 = in_range_70_180,
             percent_above_13.9 = above_250, percent_above_10 = above_180,
             percent_below_3.9 = below_70, percent_below_3 = below_54) %>%
      mutate_if(is.numeric, round, digits = 1)
    print("Got to the end of creating a df with the key glucometrics")

    #2 - getting the number of ligs from the maps

    print("Entering the function within the function to get the number of ligs according to threshold and make the naming of the variables more generalisable")

    lig_metrics <- function(df_, threshold){

      varnames <- c(paste0("n_lig", threshold), #for paste0 the default separator is ""
                    paste0("n_lig", threshold, "_awake"),
                    paste0("n_lig", threshold, "_asleep"),
                    paste0("n_lig", threshold, "_na_asleep"),
                    paste0("duration_lig", threshold),
                    paste0("duration_lig", threshold, "_awake"),
                    paste0("duration_lig", threshold, "_asleep"))
      print("Got to the end of setting the variable names for our df according to threshold")

      df_lig <- df_ %>%
        summarise(n_distinct(lig_number),
                  n_distinct(lig_number[overall_lig_sleep_status=="Awake" & !is.na(overall_lig_sleep_status)]),
                  n_distinct(lig_number[overall_lig_sleep_status=="Asleep" & !is.na(overall_lig_sleep_status)]),
                  n_distinct(lig_number[is.na(overall_lig_sleep_status)]),
                  mean(lig_duration),
                  mean(lig_duration[overall_lig_sleep_status=="Awake" & !is.na(overall_lig_sleep_status)]),
                  mean(lig_duration[overall_lig_sleep_status=="Asleep" & !is.na(overall_lig_sleep_status)])) %>%
        set_names(varnames) %>%
        mutate(Hypometrics_ID = c(paste("HypoM", .self$patientNumber, sep = "")))
      print("Got to the end of getting a df with just the number/duration of ligs, and according to sleep status")

      return (df_lig = data.frame(df_lig))
    }

    ##Calling the function for the different thresholds
    print("Calling the function")

    df_lig3.9 <- lig_metrics(.self$LIG3.9_map, 3.9)
    print("Got to the end of getting a df with just the number of lig3.9s")

    df_lig3.0 <- lig_metrics(.self$LIG3.0_map, "3.0")#3.0 as a character here just so I keep the 0 here in the 3.0 otherwise if left as numeric, it automatically rounds it up
    print("Got to the end of getting a df with just the number of lig3.0s")

    df_lig2.2 <- lig_metrics(.self$LIG2.2_map, 2.2)
    print("Got to the end of getting a df with just the number of lig2.2s")

    #3 - getting the number of long ligs from the maps
    print("Entering the function within the function to get the number of ligs according to threshold and make the naming of the variables more generalisable")

    longlig_metrics <- function(df_,threshold){

      varnames <- c(paste0("n_longlig", threshold),
                    paste0("n_day", threshold),
                    paste0("percent_day", threshold))
      print("Got to the end of setting the variable names for our df according to threshold")

      df_longlig <- df_ %>%
        filter(lig_duration >= 120) %>%
        summarise(n_distinct(lig_number),
                  n_distinct(as.Date(int_start(lig_interval))),
                  n_distinct(as.Date(int_start(lig_interval)))/70*100) %>%
        set_names(varnames) %>%
        mutate(Hypometrics_ID = c(paste("HypoM", .self$patientNumber, sep = "")))
      print("Got to the end of getting a df with just the number of long ligs at a specific threshold ")

      return(df_longlig = data.frame(df_longlig))

    }

    ##Calling the function for the different thresholds
    print("Calling the function")

    df_longlig3.0 <- longlig_metrics(.self$LIG3.0_map, "3.0")#3.0 as a character here just so I keep the 0 here in the 3.0 otherwise if left as numeric, it automatically rounds it up
    print("Got to the end of getting a df with just the number of long lig3.0s")

    df_longlig2.2 <- longlig_metrics(.self$LIG2.2_map, 2.2)
    print("Got to the end of getting a df with just the number of long lig2.2s")
  }

  #4 - Getting the number of prhs

  if(.self$summary_files$has_fitbit_data=="No"){
    print("Participant has no fitbit data so overall sleep status metrics from the overall prh maps cannot be extracted")
    df_prh <- .self$Overall_PRH_map %>%
      filter(!grepl("^I ate / drank to prevent a hypo$", Whathappened)) %>% #remove prevented hypos
      summarise(n_prh = n(),
                n_prh_na_asleep = n_prh) %>%#because fitbit data is missing altogether
      mutate(Hypometrics_ID = c(paste("HypoM", .self$patientNumber, sep = "")))
    print("Got to the end of getting a df with just the number of prhs")

  }else{
    print("Participant has fitbit data so overall sleep status metrics can be extracted from overall prh maps")
    df_prh <- .self$Overall_PRH_map %>%
      filter(!grepl("^I ate / drank to prevent a hypo$", Whathappened)) %>% #remove prevented hypos
      summarise(n_prh = n(),#'[MIGHT BE WORTH HAVING BOTH RAW PRH NUMBERS AS WELL AS WHEN PREVENTED PRHS HAVE BEEN REMOVED IN OUR TOTAL COUNTS]
                n_prh_asleep = nrow(filter(., !is.na(overall_prh_sleep_status) & overall_prh_sleep_status=="Asleep")),
                n_prh_awake = nrow(filter(., !is.na(overall_prh_sleep_status) & overall_prh_sleep_status=="Awake")),
                n_prh_na_asleep = nrow(filter(., is.na(overall_prh_sleep_status)))) %>%
      mutate(Hypometrics_ID = c(paste("HypoM", .self$patientNumber, sep = "")))
    print("Got to the end of getting a df with just the number of prhs")
  }

  #5 - getting the number of questionnaires completed and overall compliance
  files <-  file.info(list.files(paste(localPath, "Datasets/uMotif_Weekly reports/", sep=""),
                                 full.names = T)) # this gives the list of reports received from umotif

  path <- paste(rownames(files)[which.max(files$ctime)], "/", sep="") #this creates a path into the most recent umotif folder

  .self$app_qu_count <- read.csv(paste(path, list.files(path, pattern = "Count"), sep="")) %>%
    mutate(across(where(is.character), str_trim)) %>% #Added that to trim trailing and leading white space because 06004 had trailing space in the uotif code so wouldnt recognise the code for the next row
    filter(Invite.code==.self$all_ids$umotif_code) %>%
    mutate_at(c(3:62), as.numeric) %>% #this gets all the questionnaire counts in a numeric format
    mutate(n_questionnaire = rowSums(select_if(., is.numeric)), #total sum of questionnaires completed
           qu_completion_rate = n_questionnaire/302*100,
           Hypometrics_ID = c(paste("HypoM", .self$patientNumber, sep = ""))) %>% #overall questionnaire completion rate
    select(Hypometrics_ID, everything())
  print("Got to the end of creating a df with the app questionnaire count info")

  #6 - returning the lig, prh and app questionnaire dfs to the glucometrics df


  .self$key_metrics <- list(.self$key_metrics, df_lig3.9, df_lig3.0, df_longlig3.0,
                            df_lig2.2, df_longlig2.2, df_prh, .self$app_qu_count[,c("Hypometrics_ID", "n_questionnaire", "qu_completion_rate")]) %>%
    reduce(left_join)

  if(.self$summary_files$has_fitbit_data=="No" & .self$summary_files$has_cgm_data=="No"){
    print("Participant has cgm and fitbit data missing so umotif metrics only can be defined")

    .self$key_metrics <- .self$key_metrics %>%
      mutate(n_prh_week = n_prh/10, #for now getting the average number of prhs over the whole 10 weeks
             awareness_status = .self$baseline_hypo_awareness$Gold,
             diabetes_status = .self$baseline_characteristics$diabetes_type) %>%
      transform(awareness_status = ifelse(awareness_status>=4, "IAH", "NAH"))
    print("Got to the end of the final key metrics data frame including n_prhs and n_ques completed")

  } else if(.self$summary_files$has_fitbit_data=="No" & .self$summary_files$has_cgm_data=="Yes"){
    print("Participant has cgm but no fitbit data so not all metrics can be produced")

    .self$key_metrics <- .self$key_metrics %>%
      mutate(n_lig3.9_week = n_lig3.9/nweeks, #getting the number of ligs only for the number of weeks cgm was active
             n_lig3.9_asleep_week = n_lig3.9_asleep/nweeks,
             n_lig3.9_awake_week = n_lig3.9_awake/nweeks,
             n_lig3.0_week = n_lig3.0/nweeks,
             n_lig3.0_asleep_week = n_lig3.0_asleep/nweeks,
             n_lig3.0_awake_week = n_lig3.0_awake/nweeks,
             n_longlig3.0_week = n_longlig3.0/nweeks,
             n_lig2.2_week = n_lig2.2/nweeks,
             n_lig2.2_asleep_week = n_lig2.2_asleep/nweeks,
             n_lig2.2_awake_week = n_lig2.2_awake/nweeks,
             n_longlig2.2_week = n_longlig2.2/nweeks,
             n_prh_week = n_prh/10, #for now getting the average number of prhs over the whole 10 weeks
             awareness_status = .self$baseline_hypo_awareness$Gold,
             diabetes_status = .self$baseline_characteristics$diabetes_type) %>%
      transform(awareness_status = ifelse(awareness_status>=4, "IAH", "NAH"))
    print("Got to the end of the final key metrics data frame including n_prhs and n_ques completed")

  } else if(.self$summary_files$has_fitbit_data=="Yes" & .self$summary_files$has_cgm_data=="No"){
    print("Participant has fitbit but no cgm data so umotif metrics according to sleep status can be defined")

    .self$key_metrics <- .self$key_metrics %>%
      mutate(n_prh_week = n_prh/10, #for now getting the average number of prhs over the whole 10 weeks
             n_prh_asleep_week = n_prh_asleep/10,
             n_prh_awake_week = n_prh_awake/10,
             awareness_status = .self$baseline_hypo_awareness$Gold,
             diabetes_status = .self$baseline_characteristics$diabetes_type) %>%
      transform(awareness_status = ifelse(awareness_status>=4, "IAH", "NAH"))
    print("Got to the end of the final key metrics data frame including n_prhs and n_ques completed")

  }else{
    print("Participant has no data missing so all metrics can be produced")

    .self$key_metrics <- .self$key_metrics %>%
      mutate(n_lig3.9_week = n_lig3.9/nweeks, #getting the number of ligs only for the number of weeks cgm was active
             n_lig3.9_asleep_week = n_lig3.9_asleep/nweeks,
             n_lig3.9_awake_week = n_lig3.9_awake/nweeks,
             n_lig3.0_week = n_lig3.0/nweeks,
             n_lig3.0_asleep_week = n_lig3.0_asleep/nweeks,
             n_lig3.0_awake_week = n_lig3.0_awake/nweeks,
             n_longlig3.0_week = n_longlig3.0/nweeks,
             n_lig2.2_week = n_lig2.2/nweeks,
             n_lig2.2_asleep_week = n_lig2.2_asleep/nweeks,
             n_lig2.2_awake_week = n_lig2.2_awake/nweeks,
             n_longlig2.2_week = n_longlig2.2/nweeks,
             n_prh_week = n_prh/10, #for now getting the average number of prhs over the whole 10 weeks
             n_prh_asleep_week = n_prh_asleep/10,
             n_prh_awake_week = n_prh_awake/10,
             awareness_status = .self$baseline_hypo_awareness$Gold,
             diabetes_status = .self$baseline_characteristics$diabetes_type) %>%
      transform(awareness_status = ifelse(awareness_status>=4, "IAH", "NAH"))
    print("Got to the end of the final key metrics data frame including glucometrics, n_ligs, n_prhs and n_ques completed")


  }
}

explicit_gaps_and_linear_interpol = function(.self, ...){


  #if(nrow(.self$CGM_5min) == 0 & nrow(.self$CGM_15min) == 0){
  if(.self$summary_files$has_cgm_data == "No"){
    print("Participant has no CGM data so explicit gaps and linear interpolation won't be needed")

    # Hypometrics_ID <- paste0("HypoM", .self$patientNumber)#here making sure that they have a Hypo-Metrics ID so I can rbind.fill based on Hypometrics ID later on at the trial level and it will set the other columns to NA
    # .self$CGM_explicitgaps_step1 <- data.frame(Hypometrics_ID)
    # .self$CGM_explicitgaps_final <- data.frame(Hypometrics_ID)

  } else{

    cgmtimestamps <- c(.self$CGM_full_implicitgaps$CGM_Timestamp)
    diff_time_sec <- difftime(cgmtimestamps, lag(cgmtimestamps), units="secs")#using difftime instead of diff means I can control the units, although it returns an NA as the first value (for the first CGM timestamp) but getting rid of that in the next row
    diff_time_sec <- diff_time_sec[!is.na(diff_time_sec)]
    timediff_timestamps <- as.numeric(diff_time_sec)
    print("Got to the end of creating a vector with the time differences in secs between all the CGM timestamps")

    ####Below is the 5-min interpolation but had to change this to 1-min so that Zeinab doesnt have to do that retrospectively
    # indices_of_interest <- which(timediff_timestamps>=600 & timediff_timestamps<=1800)
    #
    # new_timestamps_smallgaps <- sapply(indices_of_interest, function (x)
    #   if (timediff_timestamps[x] %% 300 !=0){
    #     seq(cgmtimestamps[x]+300, cgmtimestamps[x+1]-(timediff_timestamps[x] %% 300), 300)
    #   }else{
    #     seq(cgmtimestamps[x]+300, cgmtimestamps[x+1]-300, 300)
    #   })

    indices_of_interest <- which(timediff_timestamps>=120 & timediff_timestamps<=1800) #aim being interpolating gaps of minimum 2 mins (so you can fit in a timestamp in between) and maximum of 30mins

    new_timestamps_smallgaps <- sapply(indices_of_interest, function (x)
      if (timediff_timestamps[x] %% 60 !=0){
        seq(cgmtimestamps[x]+60, cgmtimestamps[x+1]-(timediff_timestamps[x] %% 60), 60)#this will create timestamps in between the original timestamps up until it can't fit any more timestamps - note if I don't want to do that (check with Zeinab) I can fill it up until  cgmtimestamps[x+1]-((timediff_timestamps[x] %% 60)+60) and that would avoid very close in time timestamps
      }else{
        seq(cgmtimestamps[x]+60, cgmtimestamps[x+1]-60, 60)
      })


    timestamps_for_smallgaps <- c(unlist(new_timestamps_smallgaps))
    timestamps_for_smallgaps <- data.frame(timestamps_for_smallgaps)

    timestamps_for_smallgaps <- timestamps_for_smallgaps %>%
      rename(CGM_Timestamp = timestamps_for_smallgaps) %>%
      transform(CGM_Timestamp = as_datetime(CGM_Timestamp)) %>%
      arrange(CGM_Timestamp)

    .self$CGM_explicitgaps_step1 <- full_join(.self$CGM_full_implicitgaps, timestamps_for_smallgaps, by = "CGM_Timestamp")
    .self$CGM_explicitgaps_step1 <- arrange(.self$CGM_explicitgaps_step1, CGM_Timestamp)

    .self$CGM_explicitgaps_step1$Gl <-
      approx(.self$CGM_full_implicitgaps$CGM_Timestamp, .self$CGM_full_implicitgaps$Gl, .self$CGM_explicitgaps_step1$CGM_Timestamp, method = "linear")$y
    print("Got to the end of linearly interpolating gaps in CGM data that are between 2 and 30 mins")


    indices_for_largegaps <- which(timediff_timestamps>1800)

    # new_timestamps_largegaps <- sapply(indices_for_largegaps, function (x)
    #   if (timediff_timestamps[x] %% 300 !=0){
    #     seq(cgmtimestamps[x]+300, cgmtimestamps[x+1]-(timediff_timestamps[x] %% 300), 300)
    #   }else{
    #     seq(cgmtimestamps[x]+300, cgmtimestamps[x+1]-300, 300)
    #   })

    if(isempty(indices_for_largegaps)){#i.e. if participant has no gaps that are larger than 30 mins than do not create further explicit gaps in their data e.g. 07073 who has only 1 day worth of 15-min data
      print("CGM data has no gaps larger than 30min")

      .self$CGM_explicitgaps_final <-  .self$CGM_explicitgaps_step1 %>%
        arrange(CGM_Timestamp) %>%
        mutate_if(is.numeric, round, digits = 2) %>%
        transform(Hypometrics_ID = c(paste("HypoM", .self$patientNumber, sep = "")))

    } else{
      print("CGM data has gaps larger than  30min ")


      new_timestamps_largegaps <- sapply(indices_for_largegaps, function (x)
        if (timediff_timestamps[x] %% 60 !=0){
          seq(cgmtimestamps[x]+60, cgmtimestamps[x+1]-(timediff_timestamps[x] %% 60), 60)
        }else{
          seq(cgmtimestamps[x]+60, cgmtimestamps[x+1]-60, 60)
        })

      timestamps_for_largegaps <- c(unlist(new_timestamps_largegaps))
      timestamps_for_largegaps <- data.frame(timestamps_for_largegaps)

      timestamps_for_largegaps <- timestamps_for_largegaps %>%
        rename(CGM_Timestamp = timestamps_for_largegaps) %>%
        transform(CGM_Timestamp = as_datetime(CGM_Timestamp)) %>%
        arrange(CGM_Timestamp)

      .self$CGM_explicitgaps_final <- full_join(.self$CGM_explicitgaps_step1, timestamps_for_largegaps, by = "CGM_Timestamp")
      print("Got to the end of creating explicits gaps in CGM data for when gap is longer than 30 mins")

      .self$CGM_explicitgaps_final <- .self$CGM_explicitgaps_final %>%
        arrange(CGM_Timestamp) %>%
        mutate_if(is.numeric, round, digits = 2) %>%
        transform(Hypometrics_ID = c(paste("HypoM", .self$patientNumber, sep = "")))
    }
  }
  }




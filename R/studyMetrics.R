#     #2 - getting the number of ligs from the maps
#
#     print("Entering the function within the function to get the number of ligs according to threshold and make the naming of the variables more generalisable")
#
#     lig_metrics <- function(df_, threshold){
#
#       varnames <- c(paste0("n_lig", threshold), #for paste0 the default separator is ""
#                     paste0("n_lig", threshold, "_awake"),
#                     paste0("n_lig", threshold, "_asleep"),
#                     paste0("n_lig", threshold, "_na_asleep"),
#                     paste0("duration_lig", threshold),
#                     paste0("duration_lig", threshold, "_awake"),
#                     paste0("duration_lig", threshold, "_asleep"))
#       print("Got to the end of setting the variable names for our df according to threshold")
#
#       df_lig <- df_ %>%
#         summarise(n_distinct(lig_number),
#                   n_distinct(lig_number[overall_lig_sleep_status=="Awake" & !is.na(overall_lig_sleep_status)]),
#                   n_distinct(lig_number[overall_lig_sleep_status=="Asleep" & !is.na(overall_lig_sleep_status)]),
#                   n_distinct(lig_number[is.na(overall_lig_sleep_status)]),
#                   mean(lig_duration),
#                   mean(lig_duration[overall_lig_sleep_status=="Awake" & !is.na(overall_lig_sleep_status)]),
#                   mean(lig_duration[overall_lig_sleep_status=="Asleep" & !is.na(overall_lig_sleep_status)])) %>%
#         set_names(varnames) %>%
#         mutate(Hypometrics_ID = c(paste("HypoM", .self$patientNumber, sep = "")))
#       print("Got to the end of getting a df with just the number/duration of ligs, and according to sleep status")
#
#       return (df_lig = data.frame(df_lig))
#     }
#
#     ##Calling the function for the different thresholds
#     print("Calling the function")
#
#     df_lig3.9 <- lig_metrics(.self$LIG3.9_map, 3.9)
#     print("Got to the end of getting a df with just the number of lig3.9s")
#
#     df_lig3.0 <- lig_metrics(.self$LIG3.0_map, "3.0")#3.0 as a character here just so I keep the 0 here in the 3.0 otherwise if left as numeric, it automatically rounds it up
#     print("Got to the end of getting a df with just the number of lig3.0s")
#
#     df_lig2.2 <- lig_metrics(.self$LIG2.2_map, 2.2)
#     print("Got to the end of getting a df with just the number of lig2.2s")
#
#     #3 - getting the number of long ligs from the maps
#     print("Entering the function within the function to get the number of ligs according to threshold and make the naming of the variables more generalisable")
#
#     longlig_metrics <- function(df_,threshold){
#
#       varnames <- c(paste0("n_longlig", threshold),
#                     paste0("n_day", threshold),
#                     paste0("percent_day", threshold))
#       print("Got to the end of setting the variable names for our df according to threshold")
#
#       df_longlig <- df_ %>%
#         filter(lig_duration >= 120) %>%
#         summarise(n_distinct(lig_number),
#                   n_distinct(as.Date(int_start(lig_interval))),
#                   n_distinct(as.Date(int_start(lig_interval)))/70*100) %>%
#         set_names(varnames) %>%
#         mutate(Hypometrics_ID = c(paste("HypoM", .self$patientNumber, sep = "")))
#       print("Got to the end of getting a df with just the number of long ligs at a specific threshold ")
#
#       return(df_longlig = data.frame(df_longlig))
#
#     }
#
#     ##Calling the function for the different thresholds
#     print("Calling the function")
#
#     df_longlig3.0 <- longlig_metrics(.self$LIG3.0_map, "3.0")#3.0 as a character here just so I keep the 0 here in the 3.0 otherwise if left as numeric, it automatically rounds it up
#     print("Got to the end of getting a df with just the number of long lig3.0s")
#
#     df_longlig2.2 <- longlig_metrics(.self$LIG2.2_map, 2.2)
#     print("Got to the end of getting a df with just the number of long lig2.2s")
#   }
#
#
#
#   #6 - returning the lig, prh and app questionnaire dfs to the glucometrics df
#
#
#   .self$key_metrics <- list(.self$key_metrics, df_lig3.9, df_lig3.0, df_longlig3.0,
#                             df_lig2.2, df_longlig2.2, df_prh, .self$app_qu_count[,c("Hypometrics_ID", "n_questionnaire", "qu_completion_rate")]) %>%
#     reduce(left_join)



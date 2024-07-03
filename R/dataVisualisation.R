# #### 1) VISUALISING CGM DATA ONLY
#
# overview_plots = function(.self, ...){ #get an error for 01005 for some reason - this is because the the length of umotif days is longer than the CGM data length (as patient is missing first 2 weeks of CGM data) - need to fix
#   print("Entering the overview plots")
#
#   week1 <- as.interval(as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==1])),
#                        as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==7])))
#   week2 <- as.interval(as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==7])),
#                        as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==14])))
#   week3 <- as.interval(as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==14])),
#                        as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==21])))
#   week4 <- as.interval(as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==21])),
#                        as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==28])))
#   week5 <- as.interval(as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==28])),
#                        as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==35])))
#   week6 <- as.interval(as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==35])),
#                        as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==42])))
#   week7 <- as.interval(as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==42])),
#                        as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==49])))
#   week8 <- as.interval(as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==49])),
#                        as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==56])))
#   week9 <- as.interval(as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==56])),
#                        as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==63])))
#   week10 <- as.interval(as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==63])),
#                         as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==70])))
#   print("Got to the end of defining the study weeks")
#
#   .self$CGM_PRH_outline <- .self$CGM_PRH_outline %>%
#     mutate(PRH_Checkin=ifelse(!is.na(Checkin_hypo_timestamp), 3, NA_real_),
#            PRH_Motif=ifelse(!is.na(Motif_hypo_timestamp), 3.4, NA_real_),
#            Sleep_cont = case_when(Sleep_status == "Awake" ~ 13,
#                                   Sleep_status == "Asleep" ~ 2.2))
#   print("Got to the end of prepping the dataframe that will be used for the plots")
#
#   ##Extra merging step here (will be based on study days) so the data frame with the PRHs we're using for the plots is of the same length of the data frame we're using with the study reference days
#   #this is especially needed for the patients who have not completed the full 10 weeks as I would get an error message regarding differing lengths between data frames e.g. 05002, 01005
#   CGM_df_for_plots <- left_join(.self$overall_completeness_check_long, .self$CGM_PRH_outline[,c(1,3:13)]) #here getting rid of the cgm prh outline study day column as if patient has started cgm after umotif technically there will be a discrepancy regarding day 1 between the two dfs here
#
#   #Function within the function to create the plots
#   print("Starting the function within the function that will be used to generalise plot generation")
#
#   plots_create <- function(week_posixct, week_char) {
#
#     weekly_plot <- ggplot(subset(CGM_df_for_plots, Date %within% week_posixct),
#                           aes(x=`CGM_Timestamp`, y=`Gl`)) +
#       geom_line(aes(y=`Gl`, colour = "Glucose")) +
#       geom_line(aes (y=`Sleep_cont`, colour="Sleep status\n (High = Awake, Low = Asleep)")) +
#       geom_hline(aes(yintercept = 3.9), colour="purple") +
#       geom_hline(aes(yintercept = 10), color="purple") +
#       geom_point(aes(y = `PRH_Checkin`, colour = "Check-in PRH")) +
#       geom_point(aes(y = `PRH_Motif`, colour = "Motif PRH")) +
#       #theme(axis.text.x = element_text(hjust = 0.7)) +
#       ylab("Glucose concentration") +
#       xlab("Date") +
#       scale_colour_manual("Legend", values = c("Glucose"="darkred", "Sleep status\n (High = Awake, Low = Asleep)"="chocolate1",
#                                                "Check-in PRH" ="blue", "Motif PRH"="green")) +
#       scale_x_datetime(date_labels = "%d/%m/%Y \n %H:%M",
#                        date_breaks = "1 day",
#                        date_minor_breaks = "6 hours",
#                        expand = c(0,0),
#                        limits = c(int_start(week_posixct), int_end(week_posixct))) +
#       scale_y_continuous(breaks = c(3, 3.9, 10),
#                          minor_breaks = NULL) +
#       theme(legend.position = "top") +
#       ggtitle(paste(CGM_df_for_plots$Hypometrics_ID, week_char, sep="_"))
#
#     print("Got to the end of building the plot")
#
#     return(weekly_plot)
#   }
#
#   plot_week1 <- plots_create(week1, "Week1")
#   plot_week2 <- plots_create(week2, "Week2")
#   plot_week3 <- plots_create(week3, "Week3")
#   plot_week4 <- plots_create(week4, "Week4")
#   plot_week5 <- plots_create(week5, "Week5")
#   plot_week6 <- plots_create(week6, "Week6")
#   plot_week7 <- plots_create(week7, "Week7")
#   plot_week8 <- plots_create(week8, "Week8")
#   plot_week9 <- plots_create(week9, "Week9")
#   plot_week10 <- plots_create(week10, "Week10")
#
#   .self$CGM_PRH_plots <- ggarrange(plot_week1, plot_week2, plot_week3, plot_week4, plot_week5, plot_week6, plot_week7, plot_week8, plot_week9, plot_week10,
#                                    ncol=1, nrow=1)
#   print("Got to the end of ggarranging the cgm prh plots")
#
# }
#
#
#
# #### 2) Combining CGM, SLEEP AND ACTIVITY DATA IN ONE PLOT OR THREE SEPARATE PLOTS ####
#
# #####'[Figure]
# test_week1_01001 <- as.POSIXct(strptime(c("2020-10-30 22:36:23", "2020-11-06 14:13:10"), format = "%Y-%m-%d %H:%M:%S"))
# test_week1_01001 <- as.POSIXct(strptime(c("2020-10-31 15:00:00", "2020-10-31 18:45:00"), format = "%Y-%m-%d %H:%M:%S"))
#
# test_plus_plus <- test_debug$CGM_PRH
# test_plus_plus <- test_plus_plus %>%
#   filter(!is.na(Sleep_status)) %>%
#   mutate(#PRH_Scan = ifelse(!is.na(Device.Timestamp), 1.4, NA),
#     PRH_Checkin=ifelse(!is.na(Checkin_hypo_timestamp), 3, NA))#,
# #PRH_Motif=ifelse(!is.na(Motif_hypo_timestamp), 3.3, NA))#,
# #Sleep_cont = case_when(Sleep_status == "Awake" ~ 15,
# #Sleep_status == "Asleep" ~ 0))
# View(test_plus_plus)
#
#
# ggplot(test_plus_plus, aes(x=CGM_Timestamp, y=Gl)) + geom_line(aes(y=Gl), color = "blue4", size=1)
#
# #note changing the order of geoms changes the emphasis of colours
# test_cgm_sleep_plot <-
#   ggplot(test_plus_plus,
#          aes(x=CGM_Timestamp, y=Gl)) +
#   geom_rect(aes(xmin = CGM_Timestamp, xmax = lead(CGM_Timestamp), ymin = -Inf, ymax = Inf, fill=Sleep_status), alpha=1.5) +
#   scale_fill_manual(values = c("grey", "transparent")) +
#   geom_line(aes(y=Gl), color = "blue4", size=1) +
#   geom_hline(aes(yintercept = 3.9), colour="green4", size=1) +
#   geom_hline(aes(yintercept = 10), colour="green4", size=1) +
#   geom_hline(aes(yintercept = 3.0), colour="dark grey", size=0.2, alpha=0.2) +
#   geom_hline(aes(yintercept = 15), colour="dark grey", size=0.2, alpha=0.2) +
#   geom_point(aes(y = PRH_Checkin), colour = "red1", size=4) +
#   #scale_color_manual(values = c("Person-reported hypoglycaemia" = "red1")) +
#   #geom_point(aes(y = PRH_Motif), colour = "purple", size=4) +
#   #geom_point(aes(x=as.POSIXct("2020-11-04 13:13:24"), y=3.3), colour = "purple", size = 4) + #just an added motif
#   ylab("Glucose concentration (mmol/L)") +
#   #xlab("Date") +
#   scale_x_datetime(date_labels = "%H:%M",
#                    date_breaks = "2 hours",
#                    #date_minor_breaks = "6 hour",
#                    limits = test_week1_01001,
#                    expand = c(0,0)) +
#   scale_y_continuous(breaks = c(3, 3.9, 10, 15),
#                      limits = c(2.2,15)) +
#   theme_bw() +
#   theme(panel.grid.minor.x = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         axis.title.x = element_blank(),
#         legend.title = element_blank())
# #panel.background = element_rect(color=NA, fill=NA),
# #panel.ontop = T)
#
# #theme_classic()
#
# #theme(panel.grid.minor.x = element_blank(),
# #panel.grid.minor.y = element_blank())#,
# #panel.grid = element_blank())#,
# #panel.background = element_blank())
# #theme(axis.text.x = element_text(angle = 90)) +
# #ggtitle("HypoM01001 Week 3 PRH Overlaps"), legend = "top")
# #theme(panel.grid.major.x =  element_line(colour = "black"),
# #panel.grid.minor.x =  element_line(colour = "blue")) +
#
#
#
#
# #Activity data
# setwd("C:/Users/k2038095/OneDrive - King's College London/Hypometrics data/01001/Unzipped Fitbit/HypoMetrics/Physical Activity")
#
# test_activity_files  <- list.files(pattern = "steps")
#
# test_all_steps <- rbind_pages(lapply(test_activity_files, fromJSON))
# View(test_all_steps)
# str(test_all_steps)
#
# test_all_steps <- test_all_steps %>%
#   transform(dateTime = mdy_hms(dateTime)) %>%
#   rename(step_count = value)
#
# test_all_steps$step_count <- as.numeric(test_all_steps$step_count)
#
# test_all_steps <- test_all_steps %>%
#   mutate(Sleep_status = ifelse(sapply(dateTime, function(x)
#     any(x %within% test_sleep_intervals_logged)), "Asleep", "Awake")) %>%
#   transform(Sleep_status = ifelse(sapply(dateTime, function(x)
#     any(x %within% test_missing_nights)), NA_character_, Sleep_status))
#
# View(test_hi)
#
# test_step_plot <-
#   ggplot(test_all_steps,
#          aes(x=`dateTime`, y=`step_count`)) +
#   geom_rect(aes(xmin = dateTime, xmax = lead(dateTime), ymin = 0, ymax = Inf, fill=Sleep_status), alpha=1.5) +
#   scale_fill_manual(values = c("grey", "transparent")) +
#   geom_bar(stat="identity", fill="cadetblue4") +
#   ylab("Step count (n/min)") +
#   #theme(axis.title.x=element_blank()) +
#   scale_x_datetime(date_labels = "%H:%M",
#                    date_breaks = "2 hours",
#                    limits = test_week1_01001,
#                    expand = c(0,0)) +
#   geom_hline(aes(yintercept = 40), colour="dark grey", size=0.2, alpha=0.2) +
#   geom_hline(aes(yintercept = 80), colour="dark grey", size=0.2, alpha=0.2) +
#   geom_hline(aes(yintercept = 120), colour="dark grey", size=0.2, alpha=0.2) +
#   geom_hline(aes(yintercept = 160), colour="dark grey", size=0.2, alpha=0.2) +
#   scale_y_continuous(expand = c(0,0)) +
#   theme_bw() +
#   theme(panel.grid.minor.x = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         axis.title.x = element_blank(),
#         legend.title = element_blank())
#
# View(test_all_steps)
#
#
#
#
#
# # Heart rate data
# # test_heart_rate_files <- list.files(pattern = "^heart")
# # test_all_hr <- rbind_pages(lapply(test_heart_rate_files, fromJSON))
# # View(test_all_hr)
#
#
# View(test_debug$heart)
# View(test_debug$heart_for_cgm)
#
# test_all_hr <- test_debug$heart_for_cgm
#
# #Flattening the file due to nested dfs
# # test_all_hr <- flatten(test_all_hr)
# # str(test_all_hr)
# # test_all_hr <- test_all_hr %>%
# #   transform(dateTime = mdy_hms(dateTime),
# #             value.bpm = as.numeric(value.bpm)) %>%
# #   rename(heart_rate = value.bpm)
#
# test_sleep_intervals_logged <- as.interval(test_debug$sleep_combined$startTime, test_debug$sleep_combined$endTime)[!is.na(test_debug$sleep_combined$logId)]
# test_missing_nights <- as.interval(test_debug$sleep_combined$startTime, test_debug$sleep_combined$endTime)[is.na(test_debug$sleep_combined$logId)]
#
# test_all_hr <- test_all_hr %>%
#   mutate(Sleep_status = ifelse(sapply(HR_Timestamp, function(x)
#     any(x %within% test_sleep_intervals_logged)), "Asleep", "Awake")) %>%
#   transform(Sleep_status = ifelse(sapply(HR_Timestamp, function(x)
#     any(x %within% test_missing_nights)), NA_character_, Sleep_status))
#
# test_hr_plot <-
#   ggplot(test_all_hr,
#          aes(x=`HR_Timestamp`, y=`heart_rate`)) +
#   geom_rect(aes(xmin = HR_Timestamp, xmax = lead(HR_Timestamp), ymin = -Inf, ymax = Inf, fill=Sleep_status), alpha=1.5) +
#   scale_fill_manual(values = c("grey", "transparent")) +
#   geom_line(aes(y=`heart_rate`), colour = "brown") +
#   geom_hline(aes(yintercept = 50), colour="dark grey", size=0.2, alpha=0.2) +
#   geom_hline(aes(yintercept = 75), colour="dark grey", size=0.2, alpha=0.2) +
#   geom_hline(aes(yintercept = 100), colour="dark grey", size=0.2, alpha=0.2) +
#   geom_hline(aes(yintercept = 125), colour="dark grey", size=0.2, alpha=0.2) +
#   geom_hline(aes(yintercept = 150), colour="dark grey", size=0.2, alpha=0.2) +
#   geom_hline(aes(yintercept = 175), colour="dark grey", size=0.2, alpha=0.2) +
#   #theme(axis.title.x=element_blank()) +
#   ylab("Heart rate (bpm)") +
#   xlab("Time") +
#   scale_x_datetime(date_labels = "%H:%M",
#                    date_breaks = "2 hours",
#                    limits = test_week1_01001,
#                    expand = c(0,0)) +
#   scale_y_continuous(expand=c(0,0)) +
#   theme_bw()+
#   theme(panel.grid.minor.x = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         legend.title= element_blank())
#
# View(test_all_hr)
#
#
# ######
# ggarrange(test_cgm_sleep_plot, test_step_plot, test_hr_plot,
#           heights = c(1, 0.5, 0.5), ncol=1, nrow=3, align="v", common.legend = T)

#' #### 1) Note to Jonah: Basic data cleaning function below ####
#'
#' sleep = function(.self, ...){
#'
#'
#'
#'
#'         if("levels.shortData" %in% colnames(.self$sleep_original)){#becuase some participants (e..g. 06033) do not have the short wake column and therefore don't need to remove it
#'           .self$sleep_stages <- .self$sleep_stages %>%
#'             select(-levels.shortData)
#'         }
#'         ##'[No need to filter according to study period as it is extracted from sleep original which should only include study period]
#'         print("Got to the end of creating a df that only includes sleep stages")
#'
#'         ##Making a short wake data frame so I can separate it out and use it for specific analyses if needed
#'         if("levels.shortData" %in% colnames(.self$sleep_original)){#becuase some participants (e..g. 06033) do not have the short wake column and therefore don't need to remove it
#'           .self$short_wake_periods <- .self$sleep_original %>%
#'             #tidyr::unnest(cols=c(levels.shortData)) %>% #roginal code which didnt work for people who had different column types (e.g. lists vs dataframes) witin that nested column
#'             mutate_if(is.list, simplify_all) %>%  #solution from: https://stackoverflow.com/questions/38860380/unnesting-a-list-of-lists-in-a-data-frame-column
#'             unnest(levels.shortData) %>%
#'             select(Hypometrics_ID, logId, dateOfSleep, startTime, endTime, dateTime, level, seconds)
#'           ##'[No need to filter according to study period as it is extracted from sleep original which should only include study period]
#'           print("Got to the end of creating a df that only includes the short wake periods")
#'         }
#'         ###Here we are focusing on the start and end time of sleep (not the more granular data about the sleep stages and minutes awake)
#'         .self$sleep_combined <- .self$sleep_original %>%
#'           select(-contains("levels")) %>% #here getting rid of the levels columns as there are dataframes in themselves and creating errorrs when merging uk eu data - in the future consider having it as a df in its own right
#'           arrange(startTime) %>%
#'           transform(dateOfSleep = as.Date(dateOfSleep),
#'                     startTime = ymd_hms(startTime),
#'                     endTime = ymd_hms(endTime)) %>%
#'           distinct(startTime, .keep_all = T) #%>%
#'         #mutate(Hypometrics_ID = c(paste("HypoM", .self$patientNumber, sep = ""))) %>%
#'         #select(Hypometrics_ID, everything()) %>%
#'         #filter(as.Date(startTime) %within% lubridate::interval(.self$admin_data$date_umotif_consent-days(1), .self$admin_data$date_umotif_end+days(1))) #here adding 1 days to make sure we have data on sleep the last day of study (in case patient goes to sleep before midnight!) and days-1 so we know when they woke up on the day they started the study if that data is available
#'
#'         #'*Slight changes following - first step: adding a row for each missing night - I can only assume that it would just do nothing if there are no nights missing*
#'         .self$sleep_combined <- .self$sleep_combined %>%
#'           complete(dateOfSleep = seq(min(dateOfSleep), max(dateOfSleep), by = 1)) %>%
#'           transform(Hypometrics_ID = c(paste("HypoM", .self$patientNumber, sep = ""))) #because complete function creates gaps in all the variables
#'
#'         #'*Adding a start and end time for the periods where we have no info on sleep - so on the newly added rows i.e. when nights are missing*
#'         #'*Again obviously if no nights are missing the code below won't change anything to the dataframe - tested with HypoM01005 who has no missing night and it seems to be fine*
#'
#'         .self$sleep_combined <- .self$sleep_combined %>%
#'           transform(startTime = case_when(is.na(startTime) & !is.na(lag(logId)) ~ na_locf(as.numeric(endTime)+1), #'*This adds a start time for the gap in sleep data (filled with last end sleep time recorded +1sec)*
#'                                           is.na(startTime) & is.na(lag(logId)) ~ na_locf(as.numeric(endTime)+1), #'*this is in case there are multiple missing nights so if the previous row the logid is missing (i.e. previous night is missing) then it takes the last non missing value - note in its head the previous row is still missing*
#'                                           !is.na(startTime) ~ as.numeric(startTime)), #'*if that night is not missing then it keeps the start time value*
#'                     endTime = case_when(is.na(endTime) & !is.na(lag(logId)) ~ na_locf(as.numeric(startTime)-1, option = "nocb"), #'*same reasoning as start time of the gap except here we are filling the NA with the next start time of sleep recorded -1sec)*
#'                                         is.na(endTime) & is.na(lag(logId)) ~ na_locf(as.numeric(startTime)-1, option = "nocb"),
#'                                         !is.na(endTime) ~ as.numeric(endTime))) %>%
#'           transform(startTime = as.POSIXct(startTime, origin = "1970-01-01", tz="UTC"), #'*this is only because na_locf only accepts numeric values so had to return back to being POSIXct here*
#'                     endTime = as.POSIXct(endTime, origin = "1970-01-01", tz="UTC")) %>%
#'           select(Hypometrics_ID, everything())
#'         print("Got to the end of adding info the the sleep data frame to identify missing nights")
#' }
#'
#'
#'
#' #### 2) Note to Jonah: Potentially other function below? sleep features ####
#'
#' #Please note time in bed = duration
#' #and duration is in ms
#' sleep_features <- Trial_sleep_fitbit %>%
#'   filter(!is.na(logId)) %>% #removing the missing nights
#'   mutate(time_go_to_sleep = as_hms(startTime),
#'          time_get_up = as_hms(endTime)) %>%
#'   #transform(time_go_to_sleep = as_hms(format(strptime(time_go_to_sleep, '%H:%M:%S'), '%I:%M:%S')),
#'   #time_get_up = format(strptime(time_get_up, '%H:%M:%S'), '%I:%M %p')) %>%
#'   select(Hypometrics_ID, dateOfSleep, logId, startTime, endTime, duration, timeInBed, time_go_to_sleep, time_get_up,everything()) %>%
#'   transform(duration = as.numeric(difftime(endTime, startTime, units = "hours"))) #getting it in hours
#'
#' sleep_descriptive <- sleep_features %>%
#'   group_by(Hypometrics_ID) %>%
#'   summarise(n_nights = n_distinct(dateOfSleep),
#'             average_sleep_duration = mean(duration),
#'             average_time_in_bed = mean(timeInBed/60),#should be same as abovee - it is (03/01/23)
#'             average_time_asleep = mean(minutesAsleep/60),
#'             average_time_awake = mean(minutesAwake/60),
#'             sleep_efficiency = mean(efficiency)) %>%
#'   mutate_if(is.numeric, round, digits=1)
#'
#' #### Note to Jonah: Potential to turn code below into data viz funtion?
#'
#' sleep_patterns <- list(analysis_sample,
#'                        Trial_baseline_characteristics[,c("Hypometrics_ID", "diabetes_type")],
#'                        sleep_features) %>%
#'   reduce(left_join) %>%
#'   transform(time_go_to_sleep = as_hms(time_go_to_sleep)) %>%
#'   mutate(time_get_to_bed_shifted_center_midnight = if_else(time_go_to_sleep >= as_hms("00:00:00") & time_go_to_sleep<as_hms("12:00:00"),
#'                                                            time_go_to_sleep + times(86400),
#'                                                            time_go_to_sleep),
#'          time_get_up_shifted_center_six =if_else(time_get_up >= as_hms("00:00:00") & time_get_up<as_hms("18:00:00"),
#'                                                  time_get_up + times(86400),
#'                                                  time_get_up)) %>%
#'   # time_get_to_bed_shifted_center_six = if_else(time_go_to_sleep >= as_hms("00:00:00") & time_go_to_sleep<as_hms("18:00:00"),
#'   #                                              time_go_to_sleep + times(86400),
#'   #                                              time_go_to_sleep),
#'   # time_get_up_shifted_center_six =if_else(time_get_up >= as_hms("00:00:00") & time_get_up<as_hms("18:00:00"),
#'   #                                              time_get_up + times(86400),
#'   #                                              time_get_up)) %>%
#'   select(Hypometrics_ID, diabetes_type, dateOfSleep, logId, startTime, endTime, duration, time_go_to_sleep, time_get_up,
#'          time_get_to_bed_shifted_center_midnight, time_get_up_shifted_center_six)
#'
#' psych::describeBy(sleep_patterns[,c("time_get_to_bed_shifted_center_midnight", "time_get_up_shifted_center_midnight",
#'                                     "time_get_to_bed_shifted_center_six", "time_get_up_shifted_center_six")],
#'                   group=sleep_patterns$diabetes_type,
#'                   quant = c(0.5, 0.25, 0.75), skew = F, range=F)
#' ##Type 1
#' sleep_patterns %>%
#'   #filter(diabetes_type=="Type 2") %>%
#'   summarise(n = n(),
#'             median_going_to_bed = as_hms(median(time_get_to_bed_shifted_center_midnight)),
#'             q1 = quantile(time_get_to_bed_shifted_center_midnight, 0.25),
#'             q3 = quantile(time_get_to_bed_shifted_center_midnight, 0.75),
#'             median_getting_up = as_hms(median(time_get_up_shifted_center_six)),
#'             q1_up = quantile(time_get_up_shifted_center_six, 0.25),
#'             q3_up =  quantile(time_get_up_shifted_center_six, 0.75),
#'             n_before_midnight = sum(time_get_to_bed_shifted_center_midnight<as_hms("24:00:00")),
#'             percent_before_midnight = n_before_midnight/n*100,
#'             n_between_12_24 = sum(time_go_to_sleep>=as_hms("12:00:00") & time_go_to_sleep<as_hms("24:00:00")),
#'             percent_between_12_24 = n_between_12_24/n*100,
#'             n_after_six = sum(time_get_up>as_hms("06:00:00") & time_get_up<as_hms("18:00:00")),
#'             percent_after_six = n_after_six/n*100)
#'
#' sum(sleep_patterns$time_get_to_bed_shifted_center_midnight<as_hms("24:00:00"))
#'
#' time_going_to_bed_type1 <-
#'
#'   sleep_patterns %>%
#'   filter(diabetes_type=="Type 1") %>%
#'   ggplot(aes(x=time_get_to_bed_shifted_center_midnight)) +
#'   geom_histogram(color="blue", fill="blue", alpha=0.6, position = 'identity', bins = 80) +
#'   geom_vline(xintercept = hms("24:00:00"), color = "red", linewidth=1.5) +
#'   geom_vline(xintercept = hms("23:28:00"), color = "black", linewidth=1.5) +
#'   scale_x_time(breaks = c(hms("12:00:00"), hms("16:00:00"),  hms("20:00:00"), hms("23:28:00"),hms("24:00:00"),
#'                           hms("28:00:00"), hms("32:00:00"), hms("36:00:00")), #parse_time(hms("34:00:00")-hours(24), "HMS")
#'                minor_breaks = breaks_width("2 hour"),
#'                labels = c("12:00", "16:00", "20:00", "23:28", "00:00",
#'                           "04:00", "08:00", "12:00"),
#'                expand = c(0,0)
#'                #limits = c(hms("20:00:00"), hms("37:00:00"))
#'   ) +
#'   scale_y_continuous(expand = c(0,0)) +
#'   theme_bw() +
#'   ylab("Count")+
#'   xlab("Time when going to bed (HH:MM)") +
#'   ggtitle("(A) TYPE 1 DIABETES") +
#'   theme(axis.text = element_text(size=10),
#'         axis.title = element_text(face = "bold", size = 11),
#'         axis.text.x = element_text(angle = 45, hjust = 1, size=10),
#'         plot.title = element_text(face="bold")
#'   )
#'
#' time_getting_up_type1 <-
#'
#'   sleep_patterns %>%
#'   filter(diabetes_type=="Type 1") %>%
#'   ggplot(aes(x=time_get_up_shifted_center_six)) +
#'   geom_histogram(color="darkgreen", fill="darkgreen", alpha=0.6, position = 'identity', bins = 80) +
#'   geom_vline(xintercept = hms("30:00:00"), color = "red", linewidth=1.5) +
#'   geom_vline(xintercept = hms("31:30:00"), color = "black", linewidth=1.5) +
#'   # geom_rect(aes(xmin = hms("24:00:00"), xmax = hms("30:00:00"),  ymin = 0, ymax = 4200),
#'   #           fill='grey', alpha=0.01) + #was making the graph plotting extremely long https://stackoverflow.com/questions/58105968/ggplot2-is-slow-when-adding-a-rectangle
#'   # scale_fill_manual(values=c("#69b3a2", "#404080"),
#'   #                   labels = c("Going to bed", "Getting up")) +
#'   scale_x_time(breaks = c(hms("18:00:00"), hms("22:00:00"), hms("26:00:00"), hms("30:00:00"), hms("31:30:00"),
#'                           hms("34:00:00"), hms("38:00:00"), hms("42:00:00")), #parse_time(hms("34:00:00")-hours(24), "HMS")
#'                minor_breaks = breaks_width("2 hour"),
#'                labels = c("18:00", "22:00", "02:00", "06:00", "07:31",
#'                           "10:00", "14:00", "18:00"),
#'                expand = c(0,0)
#'                #limits = c(hms("20:00:00"), hms("37:00:00"))
#'   ) +
#'   scale_y_continuous(expand = c(0,0)) +
#'   theme_bw() +
#'   ylab("")+
#'   xlab("Time when getting up (HH:MM)") +
#'   theme(axis.text = element_text(size=10),
#'         axis.title = element_text(face = "bold", size = 11),
#'         axis.text.x = element_text(angle = 45, hjust = 1, size=10)
#'         #axis.text.x = element_text(angle = 45, hjust = 1, size=9)
#'   )
#'
#'
#' ggarrange(time_going_to_bed_type1, time_getting_up_type1, ncol=2)
#'
#' #Data viz 2
#' #type 1 person
#' fig_type1 <-
#'   sleep_patterns %>%
#'   filter(duration>5) %>%
#'   #filter(as.interval(startTime, endTime) %within% as.interval()) %>%
#'   filter(Hypometrics_ID=="HypoM01001") %>%
#'   mutate(unique_id = rleid(logId)) %>%
#'   filter(unique_id!=60) %>%
#'   transform(unique_id = rleid(logId)) %>%
#'   ggplot(aes(y=unique_id)) +
#'   #geom_point()#
#'   geom_segment(aes(x=time_get_to_bed_shifted_center_midnight, xend=time_get_up_shifted_center_six, yend=unique_id)) +
#'   geom_point(aes(x=time_get_up_shifted_center_six, colour="time_get_up_shifted_center_six"), alpha=0.6) +
#'   geom_point(aes(x=time_get_to_bed_shifted_center_midnight, colour="time_get_to_bed_shifted_center_midnight"), alpha=0.6) +
#'
#'   geom_rect(aes(xmin = hms("24:00:00"), xmax = hms("30:00:00"), ymin = -Inf, ymax = Inf),
#'             fill='grey', alpha=0.01) +
#'   scale_y_continuous(#breaks = seq(0, 70, 10),
#'     breaks = c(1, 7, 14, 21, 28, 35, 42, 49, 56, 63),
#'     minor_breaks = NULL,
#'     expand = c(0.01,0))+
#'   ylab("Study night") +
#'   # scale_x_time(breaks = breaks_width("2 hour"),
#'   #              minor_breaks = breaks_width("1 hour"),
#'   #               labels =function(x) format(parse_date_time(hms(x)-hours(24), "%H:%M:%S"), "%H:%M")) +#https://stackoverflow.com/questions/65485401/converting-lubridate-period-class-time-to-time-stamp-format-in-r
#'   #https://www.r-bloggers.com/2018/06/customizing-time-and-date-scales-in-ggplot2/
#'   #labels = seq(hms("00:00:00"), hms("15:00:00"), hms("01:00:00"))) +
#'   scale_x_time(breaks = c(hms("20:00:00"), hms("24:00:00"), hms("30:00:00"), hms("36:00:00")), #parse_time(hms("34:00:00")-hours(24), "HMS")
#'                minor_breaks = breaks_width("2 hour"),
#'                labels = c("20:00", "00:00", "06:00", "12:00"),
#'                limits = c(hms("20:00:00"), hms("37:00:00"))) +
#'   xlab("Time (HH:MM)") +
#'   labs(colour="Legend") +
#'   scale_colour_manual(labels = c("Going to bed", "Getting up"),
#'                       values=c("blue", "darkgreen")) +
#'   theme_bw() +
#'   #theme(legend.position="top") +
#'   theme(legend.position="top",
#'         #plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
#'         axis.text = element_text(size=12),
#'         #axis.text.y = element_text(size=10),
#'         axis.title = element_text(face = "bold", size = 13),
#'         legend.text = element_text(face = "bold", size = 10),
#'         legend.title = element_text(face = "bold"),
#'         plot.title = element_text(size = 15, face = "bold", hjust = 0)) +
#'   ggtitle("(A) PARTICIPANT WITH TYPE 1 DIABETES")

overview_plots = function(.self, ...){ #get an error for 01005 for some reason - this is because the the length of umotif days is longer than the CGM data length (as patient is missing first 2 weeks of CGM data) - need to fix
   print("Entering the overview plots")

   week1 <- as.interval(as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==1])),
                        as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==7])))
   week2 <- as.interval(as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==7])),
                        as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==14])))
   week3 <- as.interval(as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==14])),
                        as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==21])))
   week4 <- as.interval(as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==21])),
                        as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==28])))
   week5 <- as.interval(as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==28])),
                        as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==35])))
   week6 <- as.interval(as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==35])),
                        as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==42])))
   week7 <- as.interval(as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==42])),
                        as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==49])))
   week8 <- as.interval(as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==49])),
                        as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==56])))
   week9 <- as.interval(as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==56])),
                        as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==63])))
   week10 <- as.interval(as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==63])),
                         as.POSIXct(unique(.self$overall_completeness_check_long$Date[.self$overall_completeness_check_long$Study_day==70])))
   print("Got to the end of defining the study weeks")

   .self$CGM_PRH_outline <- .self$CGM_PRH_outline %>%
     mutate(PRH_Checkin=ifelse(!is.na(Checkin_hypo_timestamp), 3, NA_real_),
            PRH_Motif=ifelse(!is.na(Motif_hypo_timestamp), 3.4, NA_real_),
            Sleep_cont = case_when(Sleep_status == "Awake" ~ 13,
                                   Sleep_status == "Asleep" ~ 2.2))
   print("Got to the end of prepping the dataframe that will be used for the plots")

   ##Extra merging step here (will be based on study days) so the data frame with the PRHs we're using for the plots is of the same length of the data frame we're using with the study reference days
   #this is especially needed for the patients who have not completed the full 10 weeks as I would get an error message regarding differing lengths between data frames e.g. 05002, 01005
   CGM_df_for_plots <- left_join(.self$overall_completeness_check_long, .self$CGM_PRH_outline[,c(1,3:13)]) #here getting rid of the cgm prh outline study day column as if patient has started cgm after umotif technically there will be a discrepancy regarding day 1 between the two dfs here

   #Function within the function to create the plots
   print("Starting the function within the function that will be used to generalise plot generation")

   plots_create <- function(week_posixct, week_char) {

     weekly_plot <- ggplot(subset(CGM_df_for_plots, Date %within% week_posixct),
                         aes(x=`CGM_Timestamp`, y=`Gl`)) +
     geom_line(aes(y=`Gl`, colour = "Glucose")) +
     geom_line(aes (y=`Sleep_cont`, colour="Sleep status\n (High = Awake, Low = Asleep)")) +
     geom_hline(aes(yintercept = 3.9), colour="purple") +
     geom_hline(aes(yintercept = 10), color="purple") +
     geom_point(aes(y = `PRH_Checkin`, colour = "Check-in PRH")) +
     geom_point(aes(y = `PRH_Motif`, colour = "Motif PRH")) +
     #theme(axis.text.x = element_text(hjust = 0.7)) +
     ylab("Glucose concentration") +
     xlab("Date") +
     scale_colour_manual("Legend", values = c("Glucose"="darkred", "Sleep status\n (High = Awake, Low = Asleep)"="chocolate1",
                                              "Check-in PRH" ="blue", "Motif PRH"="green")) +
     scale_x_datetime(date_labels = "%d/%m/%Y \n %H:%M",
                      date_breaks = "1 day",
                      date_minor_breaks = "6 hours",
                      expand = c(0,0),
                      limits = c(int_start(week_posixct), int_end(week_posixct))) +
     scale_y_continuous(breaks = c(3, 3.9, 10),
                        minor_breaks = NULL) +
     theme(legend.position = "top") +
     ggtitle(paste(CGM_df_for_plots$Hypometrics_ID, week_char, sep="_"))

     print("Got to the end of building the plot")

     return(weekly_plot)
}

   plot_week1 <- plots_create(week1, "Week1")
   plot_week2 <- plots_create(week2, "Week2")
   plot_week3 <- plots_create(week3, "Week3")
   plot_week4 <- plots_create(week4, "Week4")
   plot_week5 <- plots_create(week5, "Week5")
   plot_week6 <- plots_create(week6, "Week6")
   plot_week7 <- plots_create(week7, "Week7")
   plot_week8 <- plots_create(week8, "Week8")
   plot_week9 <- plots_create(week9, "Week9")
   plot_week10 <- plots_create(week10, "Week10")

   .self$CGM_PRH_plots <- ggarrange(plot_week1, plot_week2, plot_week3, plot_week4, plot_week5, plot_week6, plot_week7, plot_week8, plot_week9, plot_week10,
                                    ncol=1, nrow=1)
   print("Got to the end of ggarranging the cgm prh plots")

}

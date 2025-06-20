#' @title Unzips and Reads Raw uMotif Files Downloaded From the Data Download Portal
#'
#' @description This function enables the unzipping and reading of raw uMotif csv files
#' following download from data download portal.
#'
#' @param Unzip Logical string (TRUE/FALSE) which determines whether uMotif folder needs to be
#' unzipped or not
#' @param FolderPath Character object indicating path to folder where uMotif data is stored
#' @param FilePattern Character object indicating the pattern in the file name that will be used to
#' extract the uMotif files of interest. For example, "evening-checkin" for evening questionnaires,
#' "wpai" for work productivity questionnaire, "motif_segmentvalue" for symptoms files.
#'
#' @return A dataset containing original uMotif data (e.g. daily questionnaires, wpai)
#'
#' @examples
#'
#' \dontrun{
#' hypometrics::umotifRead(Unzip = TRUE,
#'                         FolderPath = "~/Documents",
#'                         FilePattern = "morning-checkin"
#'                         )
#' }
#'
#' @export
umotifRead <- function(Unzip = FALSE,
                       FolderPath,
                       FilePattern){

  #### Check function arguments ####

  chk::chk_flag(Unzip)

  chk::check_dirs(FolderPath)

  chk::chk_character(FilePattern)


  if(Unzip == TRUE){

    #### Unzip original uMotif download and create a new unzipped folder ####
    setwd(FolderPath)

    umotif_files <- utils::unzip(list.files(pattern = "umotif.*zip", ignore.case = T),
                                 list=T,
                                 exdir = paste0(FolderPath, "/Unzipped uMotif"))

    setwd(paste0(FolderPath, "/Unzipped uMotif"))
    umotif_files <- list.files(recursive = T)

  }else{
    #### If unzipped uMotif folder already exists, will directly list the uMotif files ####
    setwd(paste0(FolderPath, "/Unzipped uMotif"))
    umotif_files <- list.files(recursive = T)
  }

    #### Extract files of interest from list of all files ####
    files_of_interest <- umotif_files[grepl(paste0(FilePattern, ".*csv"),
                                            umotif_files,
                                            ignore.case = T)]

    #### Read file of interest ####
    original_file <- vroom::vroom(files_of_interest)

    #### Return output ####
    return(original_file)

}

#' @title Cleans Raw uMotif Files Downloaded From the Data Download Portal
#'
#' @description This function enables the cleaning of raw uMotif data according to the type of file
#'
#' @param DataFrame A dataframe containing uMotif data.
#' @param FileType Character object indicating what type of file is to be cleaned. Can be either
#' "motif", "checkin", "promis", "wpai" or "eq5d5l".
#'
#' @return A dataset containing clean uMotif data (e.g. daily questionnaires, wpai)
#'
#' @examples
#'
#' \dontrun{
#' hypometrics::umotifClean(DataFrame = raw_checkin,
#'                         FileType = "checkin"
#'                         )
#' }
#'
#' @export
umotifClean <- function(DataFrame,
                        FileType){


  ### Check function arguments ###
  if(!FileType %in% c("motif", "checkin", "promis", "wpai", "eq5d5l")){
    stop("FileType must be `motif`, `checkin`, `promis`, `wpai` or `eq5d5l` only.")
  }

##### Data cleaning #####
  if(FileType == "motif"){
    ##### Create map of hypos reported in real time using the motif flower ####
    motif_long <- DataFrame %>%
      #extracting the last digit of segment number (Varies depending on the participant hence the simplication)
      dplyr::mutate(segment_simple = stringr::str_extract(SegmentId, "\\d{1}$")) %>%
      #adding a 1 for when the last digit is 0 i.e., this is segment 10
      transform(segment_simple = sub("(0)", "1\\1", segment_simple))

    #reshape to wide format
    motif_map <- stats::reshape(motif_long, idvar = c("UserId","uMotifTime"), timevar = "segment_simple", direction = "wide")


    #Rename columns, change to factor variables and extract exact time of hypo using prh_time column
    motif_map <- motif_map %>%
      dplyr::rename(prh_time = Value.1,
                    glucose_concentration = Value.2,
                    sweating = Value.3,
                    heart_palpitation = Value.4,
                    shaking = Value.5,
                    hunger = Value.6,
                    confusion = Value.7,
                    difficulties_speaking = Value.8,
                    movement_coordination = Value.9,
                    headache = Value.10) %>%
      transform(prh_time = factor(prh_time, levels = c(1,2,3,4,5), labels = c('Now', '15mins ago', '30mins ago', '1h ago', '>1h ago')),
                glucose_concentration = factor(glucose_concentration, levels = c(1,2,3,4,5), labels = c('Not measured', '<2mmol/L', '2-2.9 mmol/L', '3-3.9 mmol/L', '>=4mmol/L')),
                sweating = factor(sweating, levels = c(1,2,3,4,5), labels = c('Not at all', 'A little bit', 'Somewhat', 'Quite a bit', 'Very much')),
                heart_palpitation = factor(heart_palpitation, levels = c(1,2,3,4,5), labels = c('Not at all', 'A little bit', 'Somewhat', 'Quite a bit', 'Very much')),
                shaking = factor(shaking, levels = c(1,2,3,4,5), labels = c('Not at all', 'A little bit', 'Somewhat', 'Quite a bit', 'Very much')),
                hunger = factor(hunger, levels = c(1,2,3,4,5), labels = c('Not at all', 'A little bit', 'Somewhat', 'Quite a bit', 'Very much')),
                confusion = factor(confusion, levels = c(1,2,3,4,5), labels = c('Not at all', 'A little bit', 'Somewhat', 'Quite a bit', 'Very much')),
                difficulties_speaking = factor(difficulties_speaking, levels = c(1,2,3,4,5), labels = c('Not at all', 'A little bit', 'Somewhat', 'Quite a bit', 'Very much')),
                movement_coordination = factor(movement_coordination, levels = c(1,2,3,4,5), labels = c('Not at all', 'A little bit', 'Somewhat', 'Quite a bit', 'Very much')),
                headache = factor(headache, levels = c(1,2,3,4,5), labels = c('Not at all', 'A little bit', 'Somewhat', 'Quite a bit', 'Very much')),
                uMotifTime = lubridate::ymd_hms(uMotifTime)) %>%
      dplyr::mutate(motif_prh_timestamp = dplyr::case_when(prh_time == "Now" ~ uMotifTime,
                                                           prh_time == "15mins ago" ~ uMotifTime-900,
                                                           prh_time == "30mins ago" ~ uMotifTime-1800,
                                                           prh_time == "1h ago" ~ uMotifTime-3600,
                                                           prh_time == ">1h ago" | is.na(prh_time) ~ NA)) %>%
      dplyr::group_by(UserId) %>%
      dplyr::mutate(motif_prh_number = data.table::rleid(uMotifTime)) %>%
      dplyr::ungroup() %>%
      dplyr::select(UserId, motif_prh_number, uMotifTime, prh_time, motif_prh_timestamp, glucose_concentration,
                    sweating, heart_palpitation, shaking, hunger, confusion, difficulties_speaking, movement_coordination, headache)


    #Return output
    return(motif_map)


  } else if(FileType == "checkin"){

    #Clean timing of hypo
    checkin_time_clean <- DataFrame %>%
      transform(localTimestamp = as.POSIXct(localTimestamp, format = "%Y-%m-%dT%H:%M:%OS", tz="UTC")) %>%
      #Add :00 to hypo time reported where missing e.g. "03:" becomes "03:00" to have consistent HH:MM format
      dplyr::mutate(dplyr::across(dplyr::contains("Atwhattimedidthishappen"), ~ sub("^(\\d{2}):$", "\\1:00", .x)),
                    dplyr::across(dplyr::contains("Atwhattimedidthishappen"), ~ sub("^:(\\d{2})$", "00:\\1", .x)),
                    dplyr::across(dplyr::contains("Atwhattimedidthishappen"), ~ sub("^:$", "00:00", .x))) %>%
      #change to format HH:MM:SS to match the format of the rest of the timestmaps used in the package
      dplyr::mutate(dplyr::across(dplyr::contains("Atwhattimedidthishappen"), ~ sub("^(\\d{2}:\\d{2})$", "\\1:00", .x)))

    #change data format to long with 1 episode per row per participant along with its characteristics
    checkin_time_long <- data.table::melt(data.table::setDT(checkin_time_clean),
                              id.vars = c("userid","stage","localTimestamp"),
                              measure.vars = data.table::patterns("Atwhattimedidthishappen", "Howdidyoudetectyourhypoorahypothatwasabouttohappen$", "specify$", "Whathappened"),
                              value.name = c("Atwhattimedidthishappen", "Howdidyoudetectyourhypoorahypothatwasabouttohappen", "Howdidyoudetectyourhypoorahypothatwasabouttohappenotherpleasespecify", "Whathappened"),
                              variable.name = "Which_hypo")[, Which_hypo := c("first", "second", "third", "fourth", "fifth")[Which_hypo]][]

    #Remove empty rows (i.e. occasions where a questionnaire was submitted but participant did not experience a hypoglycaemic episode)
    checkin_time_long <- checkin_time_long %>%
      dplyr::arrange(userid, localTimestamp) %>%
      transform(Atwhattimedidthishappen = ifelse(Atwhattimedidthishappen=="", NA, Atwhattimedidthishappen),
                Howdidyoudetectyourhypoorahypothatwasabouttohappen = ifelse(Howdidyoudetectyourhypoorahypothatwasabouttohappen=="", NA, Howdidyoudetectyourhypoorahypothatwasabouttohappen),
                Howdidyoudetectyourhypoorahypothatwasabouttohappenotherpleasespecify = ifelse(Howdidyoudetectyourhypoorahypothatwasabouttohappenotherpleasespecify=="", NA, Howdidyoudetectyourhypoorahypothatwasabouttohappenotherpleasespecify),
                Whathappened = ifelse(Whathappened=="", NA, Whathappened)) %>%
      dplyr::filter(!is.na(Atwhattimedidthishappen) | !is.na(Howdidyoudetectyourhypoorahypothatwasabouttohappen) | !is.na(Howdidyoudetectyourhypoorahypothatwasabouttohappenotherpleasespecify) | !is.na(Whathappened))

    #Change time of hypo to date time object, if submitted before midnight, get assigned the date before the day of questionnaire submission if after midnight get assigned the date on the day of questionnaire submission
    checkin_time_long <- checkin_time_long %>%
      transform(Atwhattimedidthishappen = dplyr::case_when(hms::as_hms(Atwhattimedidthishappen) <= hms::as_hms(localTimestamp) ~ (lubridate::ymd_hms(paste(as.Date(localTimestamp), hms::as_hms(Atwhattimedidthishappen)))),
                                                           hms::as_hms(Atwhattimedidthishappen) > hms::as_hms(localTimestamp) ~ (lubridate::ymd_hms(paste(as.Date(localTimestamp), hms::as_hms(Atwhattimedidthishappen))))-86400)) %>%
      dplyr::group_by(userid) %>%
      dplyr::mutate(checkin_prh_number = data.table::rleid(localTimestamp, Which_hypo, Atwhattimedidthishappen)) %>%
      dplyr::ungroup() %>%
      dplyr::select(userid, checkin_prh_number, dplyr::everything())

    #Returns output
    return(checkin_time_long)

  } else if(FileType == "promis"){

    #Rename columns
    promis_clean <- DataFrame %>%
      dplyr::rename(restless_sleep = Sleep108, satisfied_sleep = Sleep115, refreshing_sleep = Sleep116, difficulty_falling_asleep = Sleep44, trouble_staying_asleep = Sleep87, trouble_sleeping = Sleep90,
                    got_enough_sleep = Sleep110, sleep_quality = Sleep109)

    #Calculate scores
    promis_clean <- promis_clean %>%
      dplyr::mutate_at(c(4:11), as.numeric) %>%
      dplyr::mutate(raw_score = rowSums(.[,c(4:11)])) %>%
      dplyr::mutate(t_score = dplyr::case_when(raw_score==8 ~ 28.9,
                                               raw_score==9 ~ 33.1,
                                               raw_score==10 ~ 35.9,
                                               raw_score==11 ~ 38.0,
                                               raw_score==12 ~ 39.8,
                                               raw_score==13 ~ 41.4,
                                               raw_score==14 ~ 42.9,
                                               raw_score==15 ~ 44.2,
                                               raw_score==16 ~ 45.5,
                                               raw_score==17 ~ 46.7,
                                               raw_score==18 ~ 47.9,
                                               raw_score==19 ~ 49.0,
                                               raw_score==20 ~ 50.1,
                                               raw_score==21 ~ 51.2,
                                               raw_score==22 ~ 52.2,
                                               raw_score==23 ~ 53.3,
                                               raw_score==24 ~ 54.3,
                                               raw_score==25 ~ 55.3,
                                               raw_score==26 ~ 56.3,
                                               raw_score==27 ~ 57.3,
                                               raw_score==28 ~ 58.3,
                                               raw_score==29 ~ 59.4,
                                               raw_score==30 ~ 60.4,
                                               raw_score==31 ~ 61.5,
                                               raw_score==32 ~ 62.6,
                                               raw_score==33 ~ 63.7,
                                               raw_score==34 ~ 64.9,
                                               raw_score==35 ~ 66.1,
                                               raw_score==36 ~ 67.5,
                                               raw_score==37 ~ 69.0,
                                               raw_score==38 ~ 70.8,
                                               raw_score==39 ~ 73.0,
                                               raw_score==40 ~ 76.5)) %>%
      dplyr::select(userid, stage, localTimestamp, raw_score, t_score)

    #Return output
    return(promis_clean)

  }else if(FileType == "wpai"){

    #Rename columns
    wpai_clean <- DataFrame %>%
      dplyr:: rename(Q1 = areyoucurrentlyemployedworkingforpay,
                     Q2 = duringthepastsevendayshowmanyhoursdidyoumissfromworkbecauseofproblemsassociatedwithyourhypoglycaemia,
                     Q3 = duringthepastsevendayshowmanyhoursdidyoumissfromworkbecauseofanyotherreasonsuchasvacationholidaystimeofftoparticipateinthisstudy,
                     Q4 = duringthepastsevendayshowmanyhoursdidyouactuallywork,
                     Q5 = duringthepastsevendayshowmuchdidhypoglycaemiaaffectyourproductivitywhileyouwereworking,
                     Q6 = duringthepastsevendayshowmuchdidhypoglycaemiaaffectyourabilitytodoyourregulardailyactivitiesotherthanworkatajob)

    #Calculate scores
    wpai_clean <- wpai_clean %>%
      dplyr::mutate(percent_worktime_missed_due_hypo = (Q2/(Q2+Q4)) * 100,
                    percent_impairment_whileworking_due_hypo = (Q5/10) * 100,
                    percent_overall_work_impairment_due_hypo = (Q2/(Q2+Q4) + ((1-(Q2/(Q2+Q4))) * (Q5/10))) * 100,
                    percent_activity_impairment_due_hypo = (Q6/10) * 100) %>%
      dplyr::mutate_if(is.numeric, round, digits=1) %>%
      dplyr::select(userid, stage, localTimestamp, percent_worktime_missed_due_hypo,
                    percent_impairment_whileworking_due_hypo, percent_overall_work_impairment_due_hypo,
                    percent_activity_impairment_due_hypo)

    #Return output
    return(wpai_clean)

  }else if(FileType=="eq5d5l"){

    #Recode answers
    eq5d5l_clean <- DataFrame %>%
      transform(SC = dplyr::recode(SC, "SC1"=1, "SC2"=2, "SC3"=3, "SC4"=4, "SC5"=5),
                AD = dplyr::recode(AD, "AD1"=1, "AD2"=2, "AD3"=3, "AD4"=4, "AD5"=5),
                UA = dplyr::recode(UA, "UA1"=1, "UA2"=2, "UA3"=3, "UA4"=4, "UA5"=5),
                MB = dplyr::recode(MB, "MB1"=1, "MB2"=2, "MB3"=3, "MB4"=4, "MB5"=5),
                PD = dplyr::recode(PD, "PD1"=1, "PD2"=2, "PD3"=3, "PD4"=4, "PD5"=5))

    #Return output
    return(eq5d5l_clean)

  }


}

#' @title Visualise uMotif Symptoms Data
#'
#' @description Plots Symptoms of Hypoglycaemia, Their Combination and Prevalence By Glucose Range
#'
#' @param DataFrame A dataframe of CGM data which will be filled and interpolated
#' where specified. Must contain columns: id, cgm_timestamp and glucose.
#' @param GraphType Type of visualisation to be used. Can be "upset" to visualise combinations of symptoms
#' of hypoglycaemia reported (leveraging \link[UpSetR]{upset} function from UpSetR package)
#' or "heatmap" to visualise frequency of symptoms by glucose concentration reported
#' @param VisualiseAll Logical string (TRUE/FALSE) which determines whether graphs will be plotted for all participants
#' or a selected participant. Default is TRUE.
#' @param UserID umotifID of participant  for whom symptom graph will be plotted. This is only
#' relevant if VisualiseAll = FALSE, and will produce individualised graphs.
#'
#' @details
#' This functions plots person-reported hypoglycaemia symptoms data. The function
#' offers the options to look at overall or individual data, and combination of symptoms or
#' prevalence of symptoms by glucose range.
#'
#' @return An upset plot of the combination of symptoms reported or a heat map of the prevalence
#' of hypoglycaemia symptoms by glucose range.
#'
#' @examples
#' \dontrun{
#' hypometrics::umotifVisualise(DataFrame,
#'                              GraphType = "upset"
#'                              )
#' }
#'
#' @export
umotifVisualise <- function(DataFrame,
                            GraphType,
                            VisualiseAll = TRUE,
                            UserID = ""){

#### Check function arguments and dataframe column names and type ####
  chk::check_names(DataFrame, names = c("UserId", "uMotifTime", "SegmentId", "Value"))

  if(!GraphType %in% c("upset", "heatmap")){
    stop("GraphType must be `upset` or `heatmap` only.")
  }

  chk::chk_flag(VisualiseAll)

  chk::chk_character(UserID)

  ####### Prep data #####

  if(GraphType == "upset"){

    ##Cleaning motif symptoms data and getting ready to plot
    motif_clean <- hypometrics::umotifClean(DataFrame = DataFrame,
                                            FileType = "motif") %>%
      as.data.frame() %>%
      dplyr::mutate(UserId = as.character(UserId)) %>%
      #Change symptoms from factor to numeric
      dplyr::mutate(dplyr::across(
        dplyr::contains( c("sweating", "heart_palpitation", "shaking", "hunger", "confusion", "difficulties_speaking", "movement_coordination", "headache")),
        ~ as.numeric(.x))) %>%
      #Change to binary if more than not at all (i.e. a little bit of more) marked as 1 otherwise 0
      dplyr::mutate(dplyr::across(
        dplyr::contains( c("sweating", "heart_palpitation", "shaking", "hunger", "confusion", "difficulties_speaking", "movement_coordination", "headache")),
        ~ dplyr::if_else(.x >1, 1, 0)))

    #Filtering symptoms data by indivual participant ID where required
    if(VisualiseAll == FALSE){
      motif_clean <- motif_clean %>%
        dplyr::filter(UserId == UserID)
    }

    ##Plot data
    upset_plot <-  UpSetR::upset(motif_clean,
                                 sets = c("sweating", "heart_palpitation", "shaking", "hunger", "confusion", "difficulties_speaking", "movement_coordination", "headache"),
                                 order.by = "freq",
                                 mainbar.y.label = "Hypoglycaemia episode count \n by unique combination of symptoms",
                                 sets.x.label = "Hypoglycaemia episode \n count by symptom",
                                 main.bar.color = c("violetred4"),
                                 sets.bar.color = c("turquoise4"),
                                 matrix.color = c("slateblue4"),
                                 shade.color = c("wheat4")
      )

    ##Return plot
    return(upset_plot)

  }else if(GraphType == "heatmap"){

    ##Cleaning motif symptoms data and getting ready to plot
    motif_clean <- hypometrics::umotifClean(DataFrame = DataFrame,
                                            FileType = "motif") %>%
      as.data.frame() %>%
      dplyr::mutate(UserId = as.character(UserId)) %>%
      #Change symptoms from factor to numeric
      dplyr::mutate(dplyr::across(
        dplyr::contains( c("sweating", "heart_palpitation", "shaking", "hunger", "confusion", "difficulties_speaking", "movement_coordination", "headache")),
        ~ as.numeric(.x))) %>%
      #Change to binary if more than not at all (i.e. a little bit of more) marked as 1 otherwise 0
      dplyr::mutate(dplyr::across(
        dplyr::contains( c("sweating", "heart_palpitation", "shaking", "hunger", "confusion", "difficulties_speaking", "movement_coordination", "headache")),
        ~ dplyr::if_else(.x >1, 1, 0)))


    if(VisualiseAll == TRUE){

      motif_clean <- motif_clean %>%
        dplyr::mutate(UserId = as.character(UserId)) %>%
        dplyr::filter(glucose_concentration!="Not measured" & !is.na(glucose_concentration)) %>%
        dplyr::group_by(glucose_concentration) %>%
        dplyr::summarise(n = dplyr::n(),
                         sweating = sum(sweating)/n*100, # sum of when the symptom was present (i.e present being 1 vs absent being 0)
                         heart_palpitation = sum(heart_palpitation)/n*100,
                         shaking = sum(shaking)/n*100,
                         hunger = sum(hunger)/n*100,
                         confusion = sum(confusion)/n*100,
                         difficulties_speaking = sum(difficulties_speaking)/n*100,
                         movement_coordination = sum(movement_coordination)/n*100,
                         headache = sum(headache)/n*100)

      motif_long <- reshape2::melt(motif_clean, id.vars="glucose_concentration",
                                   measure.vars = c("sweating", "heart_palpitation", "shaking", "hunger", "confusion",
                                                    "difficulties_speaking", "movement_coordination", "headache")) %>%
        dplyr::rename(symptom = variable, frequency = value)

      heatmap_plot <-
        ggplot2::ggplot(motif_long,
                        ggplot2::aes(x=glucose_concentration, y=symptom, fill=frequency)) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_gradientn(colors =c("yellow","orange", "red", "dark red"),
                                      name="Symptom \n prevalence \n", limits = c(0,100), breaks=seq(0, 100, by=20)) +
        ggplot2::theme_bw() +
        ggplot2::scale_y_discrete(labels=c("Sweating", "Heart palpitations", "Shaking", "Hunger", "Confusion", "Difficulties speaking", "Difficulties in \n movement coordination", "Headache")) +
        ggplot2::xlab("Glucose range (mmol/L)") +
        ggplot2::ylab("Symptom") +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 10, face = "bold"),
                       axis.text.x = ggplot2::element_text(size = 9, face="bold"),
                       axis.text.y = ggplot2::element_text(face="bold", size = 9)) +
        ggplot2::ggtitle("Heat map of the prevalence of hypoglycaemic symptoms \n by glucose range")

      ##Return plot##
      return(heatmap_plot)

    } else if(VisualiseAll == FALSE) {

      motif_clean <- motif_clean %>%
        dplyr::mutate(UserId = as.character(UserId)) %>%
        dplyr::filter(glucose_concentration!="Not measured" & !is.na(glucose_concentration)) %>%
        dplyr::group_by(UserId, glucose_concentration) %>%
        dplyr::summarise(n = dplyr::n(),
                         sweating = sum(sweating)/n*100,
                         heart_palpitation = sum(heart_palpitation)/n*100,
                         shaking = sum(shaking)/n*100,
                         hunger = sum(hunger)/n*100,
                         confusion = sum(confusion)/n*100,
                         difficulties_speaking = sum(difficulties_speaking)/n*100,
                         movement_coordination = sum(movement_coordination)/n*100,
                         headache = sum(headache)/n*100)

      motif_clean <- motif_clean %>%
        dplyr::filter(UserId == UserID)

      motif_long <- reshape2::melt(motif_clean, id.vars=c("UserId", "glucose_concentration"),
                                   measure.vars = c("sweating", "heart_palpitation", "shaking", "hunger", "confusion",
                                                    "difficulties_speaking", "movement_coordination", "headache")) %>%
        dplyr::rename(symptom = variable, frequency = value)

      heatmap_plot <-
        ggplot2::ggplot(motif_long,
                        ggplot2::aes(x=glucose_concentration, y=symptom, fill=frequency)) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_gradientn(colors =c("yellow","orange", "red", "dark red"),
                                      name="Symptom \n prevalence \n", limits = c(0,100), breaks=seq(0, 100, by=20)) +
        ggplot2::theme_bw() +
        ggplot2::scale_y_discrete(labels=c("Sweating", "Heart palpitations", "Shaking", "Hunger", "Confusion", "Difficulties speaking", "Difficulties in \n movement coordination", "Headache")) +
        ggplot2::xlab("Glucose range (mmol/L)") +
        ggplot2::ylab("Symptom") +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 10, face = "bold"),
                       axis.text.x = ggplot2::element_text(size = 9, face="bold"),
                       axis.text.y = ggplot2::element_text(face="bold", size = 9)) +
        ggplot2::ggtitle("Heat map of the prevalence of hypoglycaemic symptoms \n by glucose range")

      ## Return plot ##
      return(heatmap_plot)

    }


  }

}

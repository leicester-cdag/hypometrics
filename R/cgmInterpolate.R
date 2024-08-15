#' @title Implicit to Explicit Missingness and Linear Interpolation of Glucose Data.
#'
#' @description Transforms implicit gaps in CGM timestamps into explicit
#' missing glucose values, and linearly interpolate glucose values if needed.
#'
#' @param DataFrame A dataframe of CGM data which will be filled and interpolated
#' where specified. Must contain columns: id, cgm_timestamp and glucose.
#' @param Interpolate Logical value (TRUE/FALSE) which determines whether
#' linear interpolation should be conducted. If FALSE, implicit gaps in CGM timestamps
#' will be turned into explicit gaps and no further changes to the data will be made.D
#' Default is TRUE so that linear interpolation is conducted.
#' @param MinGap Numeric value which defines the minimum duration in seconds
#' of gaps to be filled and interpolated. Default is 120 seconds, i.e. 2 minutes
#' @param MaxGap Numeric value which defines the maximum duration in seconds
#' of gaps to be filled and interpolated. Default is 1800 seconds, i.e. 30 mines
#' @param Granularity Numeric value which defines the granularity of the
#' Glucose data desired in seconds. Typically closely linked to the MinGap parameter e.g.
#' if granularity of data needed is 60 seconds, MinGap would be 120 so that gap is
#' wide enough to add extra CGM timestamps. Default is 60 seconds or 1 min. i.e. the
#' resulting data frame will have CGM data at the 1 -min level.
#'
#' @details
#' This functions firstly turns implicit gaps in CGM timestamps into explicit
#' missing glucose values. Secondly, if needed, it interpolates missing glucose value as defined
#' by the MinGap, MaxGap and Granularity parameters.
#'
#' @return A dataframe of CGM data interpolated. Gaps above the maximum duration
#' are left as is.
#'
#' @examples
#' \dontrun{
#' hypometrics::cgmInterpolate(DataFrame)
#' }
#'
#' @export
cgmInterpolate <- function(DataFrame,
                          Interpolate = TRUE,
                          MinGap = 120,
                          MaxGap = 1800,
                          Granularity = 60){

  #### Check function arguments and data frame columns ####

  chk::check_names(DataFrame, names = c("id", "cgm_timestamp", "glucose"))

  chk::chk_character(DataFrame$id)

  checkmate::assertPOSIXct(DataFrame$cgm_timestamp)

  chk::chk_numeric(DataFrame$glucose)

  chk::chk_flag(Interpolate)

  chk::chk_numeric(MinGap)

  chk::chk_numeric(MaxGap)

  chk::chk_numeric(Granularity)

  #### Function within the function which will turn implicit glucose missingness into explicit and fill gaps where specified for each id ####
  fill <- function(df){

  cgmtimestamps <- c(df$cgm_timestamp)
  diff_time_sec <- difftime(df$cgm_timestamp, dplyr::lag(df$cgm_timestamp), units="secs")
  diff_time_sec <- diff_time_sec[!is.na(diff_time_sec)]
  timediff_timestamps <- as.numeric(diff_time_sec)

  #Identify indices of timestamps where there is a sufficient gap (between min and max) to add missing cgm timestamps
  indices_of_interest <- which(timediff_timestamps>=MinGap & timediff_timestamps<=MaxGap)

  #Add cgm timestamps in between min and max gap up at the Granularity level decided until there is not sufficient space in the time gap
  new_timestamps_smallgaps <- sapply(indices_of_interest, function (x)
    if (timediff_timestamps[x] %% Granularity !=0){
      seq(cgmtimestamps[x]+ Granularity, cgmtimestamps[x+1]-(timediff_timestamps[x] %% Granularity), Granularity)
    }else{
      seq(cgmtimestamps[x]+ Granularity, cgmtimestamps[x+1]- Granularity, Granularity)
    })


  timestamps_for_smallgaps <- c(unlist(new_timestamps_smallgaps))
  timestamps_for_smallgaps <- data.frame(timestamps_for_smallgaps)

  timestamps_for_smallgaps <- timestamps_for_smallgaps %>%
    dplyr::rename(cgm_timestamp = timestamps_for_smallgaps) %>%
    transform(cgm_timestamp = lubridate::as_datetime(cgm_timestamp)) %>%
    dplyr::arrange(cgm_timestamp)

  df <- dplyr::full_join(df,
                         timestamps_for_smallgaps,
                         by = c("cgm_timestamp" = "cgm_timestamp"))
  df <- dplyr::arrange(df, cgm_timestamp)

  if(Interpolate == TRUE){
#If interpolation of glucose values is required, a linear interpolation will be conducted.
  df$glucose <-
    stats::approx(df$cgm_timestamp, df$glucose, df$cgm_timestamp, method = "linear")$y

  }

  indices_for_largegaps <- which(timediff_timestamps>MaxGap)
 #i.e. if participant has no gaps that are larger than MaxGaps than will not create further explicit gaps in their data
  if(pracma::isempty(indices_for_largegaps)){

    df <- df %>%
      dplyr::arrange(cgm_timestamp) %>%
      dplyr::mutate_if(is.numeric, round, digits = 2) %>%
      dplyr::mutate(
        id =  df$id[1],
        cgm_timestamp = lubridate::floor_date(cgm_timestamp, unit = "minutes")
      )

  } #i.e. if participant has gaps longer than MaxGap, the extra timestamps will be created and glucose values marked as NA
  else{



    new_timestamps_largegaps <- sapply(indices_for_largegaps, function (x)
      if (timediff_timestamps[x] %% Granularity !=0){
        seq(cgmtimestamps[x]+Granularity, cgmtimestamps[x+1]-(timediff_timestamps[x] %% Granularity), Granularity)
      }else{
        seq(cgmtimestamps[x]+Granularity, cgmtimestamps[x+1]-Granularity, Granularity)
      })

    timestamps_for_largegaps <- c(unlist(new_timestamps_largegaps))
    timestamps_for_largegaps <- data.frame(timestamps_for_largegaps)

    timestamps_for_largegaps <- timestamps_for_largegaps %>%
      dplyr::rename(cgm_timestamp = timestamps_for_largegaps) %>%
      transform(cgm_timestamp = lubridate::as_datetime(cgm_timestamp)) %>%
      dplyr::arrange(cgm_timestamp)

    df <- dplyr::full_join(df, timestamps_for_largegaps, by = "cgm_timestamp")

    df <- df %>%
      dplyr::arrange(cgm_timestamp) %>%
      dplyr::mutate_if(is.numeric, round, digits = 2) %>%
      dplyr::mutate(id = df$id[1],
                    cgm_timestamp = lubridate::floor_date(cgm_timestamp, unit = "minutes"))

    return(df)
  }

  }

  #### Splitting the DataFrame by id ####
  list_participants <- split(DataFrame, DataFrame$id)

  #### Calling the function for each data frame containing individual participants data ####
  list_data <- lapply(list_participants, fill)

  #### Row binding individual participants data in a single dataframe ####
  final_data <- dplyr::bind_rows(list_data, .id = "id")

  #### Return CGM data with explicit gaps and interpolated glucose if specified ####
  return(final_data = data.frame(final_data))


}




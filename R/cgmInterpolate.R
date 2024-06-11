#' @title cgmInterpolate
#'
#' @description Interpolate CGM values down to one minute epochs.
#'
#' @param DataFrame A dataframe of CGM data which will be interpolated to one minute epochs.
#' Must contain columns: id, time, gl.
#'
#' @return A dataframe of CGM data interpolated to 1 minute epochs.
#' @export
#'
#' @examples
#' \dontrun{
#' hypometrics::cgmInterpolate(DataFrame)
#' }
cgmInterpolate = function(DataFrame){

  cgmtimestamps <- c(DataFrame$time)
  diff_time_sec <- difftime(DataFrame$time, dplyr::lag(DataFrame$time), units="secs")
  diff_time_sec <- diff_time_sec[!is.na(diff_time_sec)]
  timediff_timestamps <- as.numeric(diff_time_sec)

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

  timestamps_for_smallgaps <- timestamps_for_smallgaps |>
    dplyr::rename(CGM_Timestamp = timestamps_for_smallgaps) |>
    transform(CGM_Timestamp = lubridate::as_datetime(CGM_Timestamp)) |>
    dplyr::arrange(CGM_Timestamp)

  DataFrame <- dplyr::full_join(DataFrame, timestamps_for_smallgaps, by = c("time" = "CGM_Timestamp"))
  DataFrame<- dplyr::arrange(DataFrame, time)

  DataFrame$gl <-
    approx(DataFrame$time, DataFrame$gl, DataFrame$time, method = "linear")$y

  indices_for_largegaps <- which(timediff_timestamps>1800)

  if(pracma::isempty(indices_for_largegaps)){#i.e. if participant has no gaps that are larger than 30 mins than do not create further explicit gaps in their data e.g. 07073 who has only 1 day worth of 15-min data
    print("CGM data has no gaps larger than 30min")

    DataFrame <- DataFrame |>
      dplyr::arrange(time) |>
      dplyr::mutate_if(is.numeric, round, digits = 2) |>
      dplyr::mutate(
        id =  DataFrame$id[1],
        time = lubridate::floor_date(time, unit = "minutes")
      )

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

    timestamps_for_largegaps <- timestamps_for_largegaps |>
      dplyr::rename(time = timestamps_for_largegaps) |>
      transform(time = lubridate::as_datetime(time)) |>
      dplyr::arrange(time)

    DataFrame <- dplyr::full_join(DataFrame, timestamps_for_largegaps, by = "time")
    print("Got to the end of creating explicits gaps in CGM data for when gap is longer than 30 mins")

    DataFrame <- DataFrame |>
      dplyr::arrange(time) |>
      dplyr::mutate_if(is.numeric, round, digits = 2) |>
      dplyr::mutate(
        id = DataFrame$id[1],
        time = lubridate::floor_date(time, unit = "minutes")
      )

    return(DataFrame)
  }
}




#' Dataset containing glucose data of 3 participants
#' who used a CGM device
#'
#' @format A data frame with one row per CGM timestamp
#' \describe{
#'     \item{id}{Unique study ID}
#'     \item{cgm_timestamp}{Timestamp indicated on CGM device}
#'     \item{glucose}{Glucose concentration (mmol/L)}
#' }
"cgm"

#' Dataset containing raw glucose data of 2 participants
#' who used a CGM device
#'
#' @format A data frame with one row per CGM timestamp
#'
#' @details This is a raw dataset where implicit gaps in glucose
#' data have not been turned to explicit gaps (i.e. adding
#' missing CGM timestamps and corresponding missing glucose value)
#' and linear interpolation has not been conducting
#'
#' \describe{
#'     \item{id}{Unique study ID}
#'     \item{cgm_timestamp}{Timestamp indicated on CGM device}
#'     \item{glucose}{Glucose concentration (mmol/L)}
#' }
"raw_cgm"

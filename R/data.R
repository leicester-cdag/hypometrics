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

#' Dataset containing raw sleep data of 2 participants
#' who used a Fitbit Charge 4
#'
#' @format A data frame with one row per sleep event
#'
#' @details Dataset containing Fitbit Charge 4 sleep information
#'
#' \describe{
#'     \item{id}{Unique study ID}
#'     \item{logId}{Unique sleep event ID}
#'     \item{dateOfSleep}{Date of the sleep event}
#'     \item{startTime}{Start time of sleep (YYYY-MM-DD HH:MM:SS)}
#'     \item{endTime}{End time of sleep (YYYY-MM-DD HH:MM:SS)}
#' }
"raw_sleep"

#' Dataset containing raw step count data of 2 participants
#' who used a Fitbit Charge 4
#'
#' @format A data frame with one row per minute
#'
#' @details Dataset containing Fitbit Charge 4 step count
#'
#' \describe{
#'     \item{id}{Unique study ID}
#'     \item{step_timestamp}{Timestamp}
#'     \item{count}{Step count}
#' }
"raw_step"

#' Dataset containing raw heart rate data of 2 participants
#' who used a Fitbit Charge 4
#'
#' @format A data frame with one row per minute
#'
#' @details Dataset containing Fitbit Charge 4 heart rate
#'
#' \describe{
#'     \item{id}{Unique study ID}
#'     \item{hr_timestamp}{Timestamp}
#'     \item{count}{Step count}
#' }
"raw_hr"

#' Dataset containing raw umotif data of 2 participants
#' who used the Hypo-METRICS app on the uMotif platform
#'
#' @format A data frame with one row per motif segment per participant
#'
#' @details Dataset containing details on person-reported hypoglycaemic events
#'
#' \describe{
#'     \item{UserId}{Unique uMotif ID}
#'     \item{uMotifTime}{Timestamp of the submission of the hypo episode on the app}
#'     \item{SegmentID}{Motif segment ID/code e.g. 1 for "Hypo time", 2 for "Glucose level"}
#'     \item{Value}{Code for response to segment e.g. 1 for "Now" or "<2mmol/L",
#'     2 for "15 min ago" or "2-2.9mmol/L"}
#' }
"raw_motif_segment"

#' Dataset containing raw morning questionnaire data of 2 participants
#' who used the Hypo-METRICS app on the uMotif platform
#'
#' @format A data frame with one row per questionnaire completed
#'
#' @details Dataset containing timing of hypoglycaemic events reported
#' in the daily app questionnaires
#'
#' \describe{
#'     \item{userid}{Unique uMotif ID}
#'     \item{stage}{Study day when questionnaire was completed}
#'     \item{localTimestamp}{Timestamp of the submission of the questionnaire}
#'     \item{AM_forfirsthypoAtwhattimedidthishappen}{Timestamp of the first hypo since last questionnaire}
#'     \item{AM_forsecondhypoAtwhattimedidthishappen}{Timestamp of the second hypo since last questionnaire}
#'     \item{AM_forthirdhypoAtwhattimedidthishappen}{Timestamp of the third hypo since last questionnaire}
#'     \item{AM_forfourthhypoAtwhattimedidthishappen}{Timestamp of the fourth hypo since last questionnaire}
#'     \item{AM_forfifthhypoAtwhattimedidthishappen}{Timestamp of the fifth hypo since last questionnaire}
#'     \item{AM_forfirsthypoIdontknowatwhattimedidthishappen}{Whether participant does not know when first hypo occurred}
#'     \item{AM_forsecondhypoIdontknowatwhattimedidthishappen}{Whether participant does not know when second hypo occurred}
#'     \item{AM_forthirdhypoIdontknowatwhattimedidthishappen}{Whether participant does not know when third hypo occurred}
#'     \item{AM_forfourthhypoIdontknowatwhattimedidthishappen}{Whether participant does not know when fourth hypo occurred}
#'     \item{AM_forfirsthypoHowdidyoudetectyourhypoorahypothatwasabouttohappen}{How did participant detect first hypo}
#'     \item{AM_forsecondhypoHowdidyoudetectyourhypoorahypothatwasabouttohappen}{How did participant detect first hypo}
#'     \item{AM_forthirdhypoHowdidyoudetectyourhypoorahypothatwasabouttohappen}{How did participant detect first hypo}
#'     \item{AM_forfourthhypoHowdidyoudetectyourhypoorahypothatwasabouttohappen}{How did participant detect first hypo}
#'     \item{AM_forfifthhypoHowdidyoudetectyourhypoorahypothatwasabouttohappen}{How did participant detect first hypo}
#'     \item{AM_forfirsthypoHowdidyoudetectyourhypoorahypothatwasabouttohappenotherpleasespecify}{Other hypo detection - please specify}
#'     \item{AM_forfirsthypoHowdidyoudetectyourhypoorahypothatwasabouttohappenotherpleasespecify}{Other hypo detection - please specify}
#'     \item{AM_forfirsthypoHowdidyoudetectyourhypoorahypothatwasabouttohappenotherpleasespecify}{Other hypo detection - please specify}
#'     \item{AM_forfirsthypoWhathappened}{What happened for first hypo}
#'     \item{AM_forsecondhypoWhathappened}{What happened for second hypo}
#'     \item{AM_forthirdhypoWhathappened}{What happened for third hypo}
#'     \item{AM_forfourthhypoWhathappened}{What happened for fourth hypo}
#'     \item{AM_forfifthypoWhathappened}{What happened for fifth hypo}
#' }
"raw_checkin"

#' Dataset containing raw promis questionnaire data of 2 participants
#' who used the Hypo-METRICS app on the uMotif platform
#'
#' @format A data frame with one row per questionnaire submitted
#'
#' @details Dataset containing responses to promis questionnaire on Likert scale
#' from 1 to 5.
#'
#' \describe{
#'     \item{userid}{Unique uMotif ID}
#'     \item{stage}{Study day when questionnaire was completed}
#'     \item{localTimestamp}{Timestamp of the submission of the questionnaire}
#'     \item{Sleep115}{I was satisfied with my sleep}
#'     \item{Sleep116}{My sleep was refreshing}
#'     \item{Sleep90}{I had trouble sleeping}
#'     \item{Sleep110}{I got enough sleep}
#'     \item{Sleep44}{I had difficulty falling asleep}
#'     \item{Sleep87}{I had trouble staying asleep}
#'     \item{Sleep108}{My sleep was restless}
#'     \item{Sleep109}{My sleep quality was...}
#' }
"raw_promis"

#' Dataset containing raw WPAI questionnaire data of 2 participants
#' who used the Hypo-METRICS app on the uMotif platform
#'
#' @format A data frame with one row per questionnaire submitted
#'
#' @details Dataset containing responses to weekly WPAI questionnaire
#'
#' \describe{
#'     \item{userid}{Unique uMotif ID}
#'     \item{stage}{Study day when questionnaire was completed}
#'     \item{localTimestamp}{Timestamp of the submission of the questionnaire}
#'     \item{areyoucurrentlyemployedworkingforpay}{No/Yes}
#'     \item{duringthepastsevendayshowmanyhoursdidyouactuallywork}{Number field}
#'     \item{duringthepastsevendayshowmuchdidhypoglycaemiaaffectyourabilitytodoyourregulardailyactivitiesotherthanworkatajob}{Number field}
#'     \item{duringthepastsevendayshowmuchdidhypoglycaemiaaffectyourproductivitywhileyouwereworking}{Number field}
#'     \item{duringthepastsevendayshowmanyhoursdidyoumissfromworkbecauseofanyotherreasonsuchasvacationholidaystimeofftoparticipateinthisstudy}{Scale 0-10}
#'     \item{duringthepastsevendayshowmanyhoursdidyoumissfromworkbecauseofproblemsassociatedwithyourhypoglycaemia}{Scale 0-10}
#' }
"raw_wpai"

#' Dataset containing raw EQ5D5L questionnaire data of 2 participants
#' who used the Hypo-METRICS app on the uMotif platform
#'
#' @format A data frame with one row per questionnaire submitted
#'
#' @details Dataset containing responses to daily EQ5D5L questionnaire  on scale from 1 to 5
#'
#' \describe{
#'     \item{userid}{Unique uMotif ID}
#'     \item{stage}{Study day when questionnaire was completed}
#'     \item{localTimestamp}{Timestamp of the submission of the questionnaire}
#'     \item{SC}{Self Care}
#'     \item{AD}{Anxiety/depression}
#'     \item{UA}{Usual activities}
#'     \item{MB}{Mobility}
#'     \item{PD}{Pain/discomfort}
#' }
"raw_eq5d5l"

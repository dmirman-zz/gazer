
#' Zero Time Index to Event Onset
#'
#' Centers the sample index or time stamp of your time series to a reference
#' marker, such that 0 always coincides with the onset of a particular event.
#'
#' @param time Numeric vector of sample indices or time stamps corresponding to
#' a single time series.
#'
#' @param marker Vector containing markers of trial events.
#'
#' @param event Label in `marker` that indicates the onset event to which
#' the time series should be zeroed. If this label is a character string, it
#' must be enclosed in quotation marks.
#'
#' @return Numeric vector that zeroes `time` to the trial onset. In the
#' event that no onset marker is found, a warning is issued and a vector of
#' `NA`s is returned.
#'
#' @export

onset_pupil <- function(time, marker, event){
  onset_time <- time[!is.na(marker) & marker == event]
  if(length(onset_time) == 0){
    return(rep(NA, length(time)))
  }
  if(length(onset_time) > 1)
    stop ("event marker occurs more than once per trial")
  time - onset_time
}

#'  Calculates max absoulte dilation speed as suggested in Kret & Sjak-Shie (in press)
#' calculates the median absoulte deviation (MAD) and sets the threshold to filter on.
#' @param pup column listing pupil sizes
#' @param time column in data file listing time
#' @return data with pupil speeds for each time point
#' @export
speed_pupil <- function(pup, time) {
  cur_dilation_speed<-diff(pup)/diff(time)
  backward_pupil<- c(NA, cur_dilation_speed)
  forward_pupil<-c(cur_dilation_speed,NA)
  backfwd_pupil<-cbind(backward_pupil, forward_pupil)
  max_backfwd_pupil<-apply(backfwd_pupil, 1,max, na.rm=TRUE)
  max_backfwd_pupil<-abs(max_backfwd_pupil)
  return(max_backfwd_pupil)
}






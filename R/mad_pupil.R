# Calculates max dilation speed

max_dilation_speed <- function(pup, time) {
  cur_dilation_speed<-diff(pup)/diff(time)
  backward_pupil<- c(NA, cur_dilation_speed)
  forward_pupil<-c(cur_dilation_speed,NA)
  backfwd_pupil<-cbind(backward_pupil, forward_pupil)
  max_backfwd_pupil<-apply(backfwd_pupil, 1,max, na.rm=TRUE)
  max_backfwd_pupil<-abs(max_backfwd_pupil)
  return(max_backfwd_pupil)
}






#' Turns pupil sizes with blinks NA and linearly interpolates values
#' @param datafile data frame.
#' @param extendblinks blinks already extended  in data frame 
#' @export
#' @import zoo
#' 
#' @return data frame containing interpolated data
#' 
interpolate_pupil<-function(datafile, extendblinks=FALSE) { 
  require(zoo)
  if (extendblinks==FALSE) {
message("Turning pupil size with blinks to NA")
  blinks_na <- datafile %>% dplyr::mutate(pup = ifelse(blink==1, NA, pupil)) #turns blinks into NA for interpolation
  message("Performing linear interpolation")
  pupil_interp <- blinks_na %>%
    dplyr::group_by(subject, trial) %>%
    dplyr::mutate(interp = na.approx(pup, rule=2)) %>% ungroup() 
    return(pupil_interp)
  }
  
  if (extendblinks==TRUE) {
message("Performing linear interpolation")
pupil_interp <- datafile %>%
  dplyr::group_by(subject, trial) %>%
  dplyr::mutate(interp = na.approx(pupil, rule=2)) %>% ungroup() 
return(pupil_interp)
  }
}

  

#' Turns pupil sizes with blinks NA and linearly interpolates values
#' @param binsize user-specified threshold for binning data.
#' @export
#' @import zoo
#' @return data frame containing interpolated data
#' 

interpolate_pupil<-function(datafile) { #must specify desired binszie
  require(zoo)
message("Turnining pupil size with blinks to NA")
  blinks_na <- datafile %>% dplyr::mutate(pup = ifelse(blink==1, NA, pupilmm)) #turns blinks into NA for interpolation
  message("Performing linear interpolation")
  pupil_interp <- blinks_na %>%
    dplyr::group_by(subject, trial) %>%
    dplyr::mutate(interp = na.approx(pup, rule=2)) %>% ungroup() 
    return(pupil_interp)
}

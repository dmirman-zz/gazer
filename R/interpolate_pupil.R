#' Turn blinks into NA and perform linear interpolation
#' @param binsize user-specified threshold for binning data.
#' @export
#' @import itsadug
#' @return data frame containing interpolated data
#' 

interpolate_pupil<-function(datafile, binsize=NA) { #must specify desired binszie
  require(zoo)
  require(itsadug)
message("Turnining all blinks to NA")
  blinks_na <- datafile %>% mutate(pup = ifelse(blink==1, NA, pupil)) #turns blinks into NA for interpolation
  message("Performing linear interpolation")
  pupil_interp <- blinks_na %>%
    dplyr::group_by(subject, trial, time) %>%
    mutate(interp = na.approx(pup, rule=2), #linear interpolation
           timebins = timeBins(time, binsize))  %>% ungroup()#places data into timebins =  50, 150 ms, 250, ms if 100ms bins
    return(pupil_interp)
}

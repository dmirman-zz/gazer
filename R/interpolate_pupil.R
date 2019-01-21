#' Turns pupil sizes with blinks NA and linearly interpolates values
#' @param datafile data frame.
#' @param extendblinks blinks already extended  in data frame 
#' @param type type of interpolation (linear or cubic)
#' @export
#' @import zoo
#' 
#' @return data frame containing interpolated data 
#' 
interpolate_pupil<-function(datafile, extendblinks=FALSE, type=NA) { #supports linear and cublic-spline interpolation
  require(zoo)
  if (extendblinks==FALSE & type=="linear") {
message("Turning pupil size with blinks to NA")
  blinks_na <- datafile %>% dplyr::mutate(pup = ifelse(blink==1, NA, pupil)) #turns blinks into NA for interpolation
  message("Performing linear interpolation")
  pupil_interp <- blinks_na %>%
    dplyr::group_by(subject, trial) %>%
    dplyr::mutate(interp = zoo::na.approx(pup, rule=2)) %>% ungroup() 
    return(pupil_interp)
  }
  
  if (extendblinks==TRUE & type=="linear") {
message("Performing linear interpolation")
pupil_interp <- datafile %>%
  dplyr::group_by(subject, trial) %>%
  dplyr::mutate(interp = zoo::na.approx(pupil, rule=2)) %>% ungroup() 
return(pupil_interp)

  }
  
  if (extendblinks==FALSE & type=="cubic") { 
    message("Turning pupil size with blinks to NA")
    blinks_na <- datafile %>% dplyr::mutate(pup = ifelse(blink==1, NA, pupil)) #turns blinks into NA for interpolation
    message("Performing cubic interpolation")
    pupil_interp <- blinks_na %>% dplyr::group_by(subject, trial) %>% 
                       dplyr::mutate(index = ifelse(is.na(pup), NA, dplyr::row_number()),
                       index = zoo::na.approx(index, na.rm = FALSE),
                       interp = zoo::na.spline(pupil, na.rm = FALSE, x = index))
    pupil_interp <-  pupil_interp %>% 
                     dplyr::select(-index)
    return(pupil_interp)
  }
  
  if (extendblinks==TRUE & type=="cubic") { 
    message("Performing cubic interpolation")
    pupil_interp <- blinks_na %>% dplyr::group_by(subject, trial) %>% 
      dplyr::mutate(index = ifelse(is.na(pup), NA, dplyr::row_number()),
                    index = zoo::na.approx(index, na.rm = FALSE),
                    interp = zoo::na.spline(pupil, na.rm = FALSE, x = index))
    pupil_interp <- pupil_interp %>% 
                    dplyr::select(-index)
    return(pupil_interp)
  }
    
    }

  

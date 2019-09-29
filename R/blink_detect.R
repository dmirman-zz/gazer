#' Take pupil file and detect blinks
#' merge blinks with sample report 

#' @param samp pupil data file
#' @return  merged data file with blinks detected
#' @export
#' 
blink_detect <- function (samp) { 
  
  library(saccades)
  
  blk <- saccades::detect.fixations(samp) # get blinks using the saccades algo from saccades package

  blk1 <- blk %>%
  dplyr::filter(event=="blink") %>%
  tidyr::gather(data=., key="startend", value="time", start:end) # gather all the blinks

  blk_merge <- merge(samp, blk1, by=c("trial", "time", "x", "y"), all=TRUE)
  
  return(blk_merge)
}
  
  
  
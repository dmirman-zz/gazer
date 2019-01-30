#' Merge pupil files from a folder
#'
#' Renames column variables and makes them lower case
#' Places time in ms
#'@import tidyverse
#'@import itsadug
#'@import data.table
#'
#' @param file_list path to .xls files
#' @export
#' @return data frame containing pupil data
merge_pupil_files <- function (file_list) {  
  #file list is path to .xls files
  library(data.table)

    
  dataset <- do.call("rbind", lapply(file_list, FUN=function(files){
    
    for (i in 1:length(files)){ 
      if(file.exists(files[i])){
        message( "now processing:", files[i])
      }
    }
    
    fread(files, header=TRUE, sep="\t", na.strings = ".", fill=TRUE)})) #fread makes reading in files quicke
  
  change_name <- select(dataset,subject=RECORDING_SESSION_LABEL, trial =  TRIAL_INDEX,    blink = AVERAGE_IN_BLINK, pupil = AVERAGE_PUPIL_SIZE, everything())
  
  names(change_name) <- tolower(names(change_name))
  
  change_name$time <- change_name$timestamp-change_name$ip_start_time
  
  return(as_tibble(change_name))
}

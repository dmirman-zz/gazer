#' Merge pupil files from a folder
#'
#' Renames column variables and makes them lower case
#' Places time in ms
#'
#' @param file_list path to .xls files
#' @export
#' @return data frame containing pupil data
merge_pupil_files <- function (file_list) {  #file list is path to .xls files
  dataset <- do.call("rbind", lapply(file_list, FUN=function(files){
    read.table(files, header=TRUE, sep="\t", na.strings = ".") } ))
  change_name <- select(dataset,
                        c(subject=RECORDING_SESSION_LABEL, trial = TRIAL_INDEX,
                         blink = AVERAGE_IN_BLINK, pupil = AVERAGE_PUPIL_SIZE, everything()))
  names(change_name) <- tolower(names(change_name))
  change_name$time <- change_name$timestamp-change_name$ip_start_time
  return(change_name)
}


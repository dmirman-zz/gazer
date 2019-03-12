#' Calculate percent missing data by subject and by trial
#' @param missingthresh user-specified threshold for missing data.
#' @export
#' @return data frame containing missing data information
#' @export
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
missing_pupil_count<- function(datafile, missingthresh=.3) {
  
  countsbysubject <- datafile %>%
    dplyr::group_by(subject) %>%
    dplyr::summarise(missing = sum(is.na(pupil)),
                     samples = sum(!is.na(pupil)),
                     total = length(pupil)) %>%
    dplyr::mutate(averageMissingSub = missing / total)
  
  countsbytrial <- datafile %>%
    dplyr::group_by(subject, trial) %>%
    dplyr::summarise(missing = sum(is.na(pupil)),
                     samples = sum(!is.na(pupil)),
                     total = length(pupil))%>%
    dplyr::mutate(averageMissingTrial = missing / total)
  
  greaterthan <- dplyr::filter(countsbytrial, averageMissingTrial > missingthresh)
  prop <- length(greaterthan$trial)/length(countsbytrial$trial)
  
  # % trials excluded
  message("% trials excluded:",  round(prop, digits=3))
  message("Participants taken out:" ,countsbysubject$subject[countsbysubject$averageMissingSub> missingthresh])
  
  # print # subjects excluded
  
  # add missing data per subject to the data frame
  combineSub <- merge(datafile, countsbysubject[, c("subject", "averageMissingSub")], by="subject", all=TRUE)
  # add missing data per trial to the data frame
  combinetrial <- merge(combineSub, countsbytrial[, c("subject", "trial", "averageMissingTrial")], by=c("subject", "trial"), all=TRUE)
  # keep only the data that exceeds the threshold for retaining the trials & subjects
  combinetrial_above_threshold <- dplyr::filter(combinetrial, (averageMissingSub < missingthresh) & (averageMissingTrial < missingthresh))
  
  return(combinetrial_above_threshold)
}



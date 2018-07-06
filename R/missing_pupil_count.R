#' Calculate percent missing data by subject and by trial
#'
#'
#' renames column variables and makes them lower case
#' places time in ms
#'
#' @param missingthresh user-specified threshold for missing data.
#' @export
#' @return data frame containing missing data information
missing_pupil_count <- function(datafile, missingthresh=.3) {
  
  countsbysubject <- datafile %>%
      group_by(subject) %>%
      summarise(missing = sum(is.na(pupil)), 
            samples = sum(!is.na(pupil)), 
            total = length(pupil)) %>%
      mutate(averageMissingSub = missing / total)
  
  countsbytrial <- datafile %>%
      group_by(subject, trial) %>%
      summarise(missing = sum(is.na(pupil)), 
            samples = sum(!is.na(pupil)), 
            total = length(pupil))%>%
      mutate(averageMissingTrial = missing / total)
  
  greaterthan <- subset(countsbytrial, countsbytrial$averageMissingTrial > missingthresh)
  prop <- length(greaterthan$trial)/length(countsbytrial$trial)
  
  # % trials excluded
  print(prop)
  print(countsbysubject$subject[countsbysubject$averageMissingSub> missingthresh])
  
  # print # subjects excluded
  
  # add missing data per subject to the data frame
  combineSub <- merge(datafile, countsbysubject[, c("subject", "averageMissingSub")], by="subject", all=TRUE)
  # add missing data per trial to the data frame
  combinetrial <- merge(combineSub, countsbytrial[, c("subject", "trial", "averageMissingTrial")], by=c("subject", "trial"), all=TRUE)
  # keep only the data that exceeds the threshold for retaining the trials & subjects
  combinetrial_above_threshold <- subset(combinetrial, (averageMissingSub < missingthresh) & (averageMissingTrial < missingthresh))
  
  return(combinetrial_above_threshold)
}


#' calculated percent missing data by subject and by trial
#'
#'
#' renames column variables and makes them lower case
#' places time in ms
#'@param missingthresh user-specified threshold for missing data.
#' calculate percent missing data by subject and by trial
#'
#'
#'
#'@param missingthresh user-specified threshold for missing data.
missing_pupil_count <- function(datafile, missingthresh=.5) {
  countsbysubject <- ddply(datafile, .(subject), summarise,
                           missing = sum(is.na(pupil) ), samples = sum(!is.na(pupil)), total = length(pupil))
  countsbysubject$averageMissingSub <- (countsbysubject$missing / countsbysubject$total)

  countsbytrial <- ddply(datafile, .(subject, trial), summarise,
                         missing = sum(is.na(pupil) ), samples = sum(!is.na(pupil)), total = length(pupil))
  countsbytrial$averageMissingTrial <- (countsbytrial$missing / countsbytrial$total)
  greaterthan=subset(countsbytrial, countsbytrial$averageMissingTrial > missingthresh)
  prop<-length(greaterthan$trial)/length(countsbytrial$trial)
  #%trials excluded
  print(prop)
  print(countsbysubject$subject[countsbysubject$averageMissingSub> missingthresh])
  #print # subjects excluded
  combineSub<-merge(datafile, countsbysubject[, c("subject", "averageMissingSub")], by="subject", all=TRUE)
  combinetrial<-merge(combineSub, countsbytrial[, c("subject", "trial", "averageMissingTrial")], by=c("subject", "trial"), all=TRUE)
  combinetrial_threshold<-subset(combinetrial, (averageMissingSub < missingthresh) & (averageMissingTrial < missingthresh))
  return(combinetrial_threshold)
}


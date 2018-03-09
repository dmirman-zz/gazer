#' calculated percent missing data by subject and by trial
#' 
#'
#' renames column variables and makes them lower case
#' places time in ms
#'@param missingthresh user-specified threshold for missing data. 





missing_pupil_count <- function(datafile, missingthresh=.5) { 
  countsbysubject <- ddply(datafile, .(subject), summarise, 
                           missing = sum(is.na(pupil) ), samples = sum(!is.na(pupil)), total = length(pupil))
  countsbysubject$averageMissingSub <- (countsbysubject$missing / countsbysubject$total)
  #print # subjects excluded
  countsbytrial <- ddply(datafile, .(subject, trial), summarise, 
                         missing = sum(is.na(pupil) ), samples = sum(!is.na(pupil)), total = length(pupil))
  countsbytrial$averageMissingTrial <- (countsbytrial$missing / countsbytrial$total)
  #trials excluded
  combineSub<-merge(datafile, countsbysubject[, c("subject", "averageMissingSub")], by="subject", all=TRUE)
  combinetrial<-merge(combineSub, countsbytrial[, c("subject", "trial", "averageMissingTrial")], by=c("subject", "trial"), all=TRUE)
  combinetrial<-subset(combinetrial, (averageMissingSub < missingthresh) & (averageMissingTrial < missingthresh))
  return(countsbysubject)
}
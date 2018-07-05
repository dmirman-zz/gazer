#' Convert fixation list into time bins
#'
#' Takes a data frame list of fixations and returns a time series
#' with the fixations expanded into time bins of specified duration.
#'
#' @param gaze a data frame of fixations, as returned by readFixationReport
#' @param binSize length of bins, in ms
#' @param keepCols set of columns to keep, fixation start and end times will be dropped
#' @param maxTime optional parameter to set maximum trial length
#' @return data frame containing gaze data arranged by bin
#' @export
#' @examples
binify_fixations <- function(gaze, binSize=20, keepCols=c("Subject", "TrialNumber", "Target", "T"), maxTime=NULL){
  #convert a list of fixations to bins
  #binSize determines the size of each bin in ms
  #keepCols determines which columns from the original data frame will show up in the output
  #	will no longer need fixation start and duration, nor fixation location coordinates
  #
  #maxTime can be used to cut down trial length
  #

  #need to know when fixations end
  if ("CURRENT_FIX_END" %in% names(gaze)) {
    gaze$FixEnd <- gaze$CURRENT_FIX_END
  } else {
    #compute end of fixation from start and duration
    gaze$FixEnd <- gaze$CURRENT_FIX_START + gaze$CURRENT_FIX_DURATION
  }
  #if maxTime is defined, do some trimming
  if (!is.null(maxTime)) {
    #drop all fixations that start after the maxTime
    gaze <- subset(gaze,CURRENT_FIX_START < maxTime)
    #trim fixation end times to be less than maxTime
    gaze$FixEnd[gaze$FixEnd>maxTime] <- maxTime
  }

  #make a fixation ID variable that is just the fixation number in the overall data frame
  gaze$FixationID <- 1:nrow(gaze)
  #expand fixations into time bins
  for(f in 1:max(gaze$FixationID)){ # for each fixation
    this_fix <- subset(gaze, FixationID == f)
    fix_bins <- ceiling(this_fix$CURRENT_FIX_START/binSize):ceiling(this_fix$FixEnd/binSize)
    if(f==1){ #first fixation: initialize gaze_bins
      gaze_bins <- data.frame(FixationID = 1, timeBin = fix_bins)
    } else { #other fixations: append to gaze_bins
      gaze_bins <- rbind(gaze_bins, data.frame(FixationID = f, timeBin = fix_bins))
    }
  }

  #there is a border case in which two redundant bins can be generated
  #clean them up by keeping the second one
  gaze_bins <- subset(gaze_bins, timeBin[2:length(timeBin)]!=timeBin[1:(length(timeBin)-1)])

  #combine data
  dataFull <- merge(gaze_bins, gaze[,c(keepCols, "FixationID")], by="FixationID")

  #add a variable with actual time instead of time bin
  dataFull$Time <- dataFull$timeBin*binSize

  return(dataFull)
}

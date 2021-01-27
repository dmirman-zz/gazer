# this function implements the noise bsaed blink detection in Ronen et al. 2018
# This adaptation to R was made with the supervision and encouragement of Dr William Paul Boyce.
# For more information about this adaptation and for more R solutions, don't hesitate to contact him: paul.boyce@ntu.edu.sg
# Dr. Jason Geller helped diagnose a coding error in the original script where matching was being done on index instead of time
#' Extends blinks if not already done in SR Data Viewer
#' @param pupil_data column with pupil values
#' @param  hz = sampling rate of eye tracker
#' @return blinks

noise_based_blink_detection <- function(pupil_data, hz){

  library(forecast) # you need this
  library(ggplot2) # you need this
  library(pracma)

  sampling_interval     <- round(1000/hz) #compute the sampling time interval in milliseconds.
  gap_interval          <- 100 # set the interval between two sets that appear consecutively for concatenation.

  blinks_data <- pupil_data==0
  blinks <- c(-1*which(diff(blinks_data) %in% 1), which(diff(blinks_data) %in% -1)+1)

  # Case 1: there are no blinks

  if (length(blinks)==0)
    return(blinks);

  # Sort the blinks by absolute value. in this way we are getting an array of blinks when the offset appears after the onset
  blinks <- blinks[order(abs(blinks))]

  # Edge cases
  # Case 2: the data starts with a blink. In this case, blink onset will be defined as the first missing value.
  if (length(blinks)>0 && blinks[1]>0 && pupil_data[1]==0)
    blinks = c(-1, blinks);

  # Case 3: the data ends with a blink. In this case, blink offset will be defined as the last missing sample
  if(length(blinks)>0 && tail(blinks, 1)<0 && tail(pupil_data, 1)==0)
    blinks = c(blinks, nrow(pupil_data))

  # Smoothing the data in order to increase the difference between the measurement noise and the eyelid signal.
  ms_4_smooting  <- 10                                      # using a gap of 10 ms for the smoothing
  samples2smooth <- ceiling(ms_4_smooting/sampling_interval) # amount of samples to smooth
  smooth_data    <- ma(pupil_data, samples2smooth)
  smooth_data[1, 1] <- pupil_data[1, 1]
  smooth_data[2, 1] <- pupil_data[2, 1]

  smooth_data[smooth_data==0] <- NaN;  # replace zeros with NaN values
  diff_smooth_data            = diff(smooth_data);

  # Finding the blinks' onset and offset
  blink                 <- 1;                            # initialize blink index for iteration
  blinks_data_positions <- matrix(0, length(blinks), 1)  # initialize the array of blinks
  prev_offset           <- -1                            # initialize the previous blink offset (in order to detect consecutive sets)

  while (blink < length(blinks)){
    onset_candidate <- blinks[blink]
    blink           <- blink + 1  # increase the value for the offset

    # set the offset candidate
    offset_candidate <- blinks[blink]
    blink            <- blink + 1  # increase the value for the next blink

    # find blink onset
    data_before <- diff_smooth_data[2:abs(onset_candidate)] # returns all the data before the candidate
    blink_onset <- tail(which(data_before>0), 1)            # returns the last 2 samples before the decline

    # Case 2 (the data starts with a blink. In this case, blink onset will be defined as the first missing value.)
    if (isempty(blink_onset==TRUE))
      ifelse(onset_candidate == blinks[1], blink_onset <- 0, blink_onset <- -abs(onset_candidate))

    # correct the onset if we are not in case 2
    if (onset_candidate>0 || pupil_data[onset_candidate+2, 1]>0)
      blink_onset      = blink_onset+2

    # find blink offset
    data_after   <- diff_smooth_data[abs(offset_candidate):length(diff_smooth_data)] # returns all data after the candidate
    blink_offset  <- abs(offset_candidate)+head(which(data_after<0), 1)                   # returns the last sample before the pupil increase

    # Case 3 (the data ends with a blink. In this case, blink offset will be defined as the last missing sample.)
    if (length(blink_offset)==0)
      blink_offset <- nrow(pupil_data)+1

    # Set the onset to be equal to the previous offset in case where several sets of missing values are presented consecutively
    if (sampling_interval*blink_onset > gap_interval && sampling_interval*blink_onset-sampling_interval*prev_offset<=gap_interval)
      blink_onset <- prev_offset

    prev_offset <- blink_offset-1
    # insert the onset into the result array
    blinks_data_positions[blink-2] <- -sampling_interval*blink_onset
    # insert the offset into the result array
    blinks_data_positions[blink-1] <- sampling_interval*(blink_offset-1)
  }

  duplicated_values     <- blinks_data_positions[duplicated(blinks_data_positions)]

  #blinks_data_positions <- blinks_data_positions[!blinks_data_positions %in% duplicated_values];

  res <- blinks_data_positions
  id = 1;
  while(id<length(res)-2)
  {
    if(res[id]>0 && res[id]==-res[id+1]){
      toremove <- matrix(TRUE, length(res), 1);
      toremove[id] <- FALSE
      toremove[id+1] <- FALSE
      res <- res[toremove]
    }else{
      id = id+1
    }
  }
  return(abs(res))
}

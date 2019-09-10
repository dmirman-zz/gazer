
#' Extends blinks if not already done in SR Data Viewer
#' @param values column with pupil values 
#' @param fillback  NA extend backward (in ms)
#' @param fillforward NA extend forward (in ms)
#' @param hz sampling rate of eye tracker
#' @return vector containing extended blinks
#' @export

extend_blinks <- function(values, fillback=5, fillforward=5, hz=NA) {
  
  samprate<-round(1000/hz)
  
  fillf <- fillforward/samprate
  
  fillb <- fillback / samprate
  
  
  # Function to take any run of NA values in a vector,
  # and expand them by assigning NA to values 
  # that lead and trail that run
  # NOTE: this could be re-written with RLE, but it works already
  
  output_length <- length(values)
  
  # Extract the values from the trial
  #values <- trial$GazeByImageAOI
  
  # Grab all the non-NA gaze frames. Store a second copy of it.
  tracked <- which(!is.na(values))
  #   tracked <- tracked
  
  # Record how many missing frames there are.
  missing <- sum(is.na(values))
  
  # For each non-NA gaze frame, subtract the index of the previous frame from it. 
  # The difference allows us to find out how many NA frames were skipped when we 
  # extracted all the non-NA frames.
  for (frame in length(tracked):1){
    if(1 < frame){
      tracked[frame] <- (tracked[frame] - tracked[frame - 1])
    }
  }
  
  ## Find the precise starts and ends of each gap
  
  # Starts are not accurate because they don't take into account other missing 
  # frames. Use the cumulative sum of missing frames to correct the start 
  # locations.
  gap.start <- which(1 < tracked)
  gap.size <- tracked[gap.start] - 1
  gap.sizes <- cumsum(gap.size)
  
  # Correction of gap locations
  for (gap in seq_along(gap.start)){
    if (gap == 1){
      next
    }  
    gap.start[gap] <- gap.start[gap] + gap.sizes[gap - 1]
  }
  
  # Final adjustments so gap.start and gap.end are indices are non-NA values.
  gap.end <- gap.start + gap.size + fillf
  gap.start <- gap.start - fillb
  
  # Pair off all start and ends, make ranges, and combine into a list of indices
  make_these_na <- unlist(mapply(":", gap.start, gap.end))
  
  # ensure no negative indices
  make_these_na <- make_these_na[make_these_na > 0]
  
  # Overwrite indices with NA
  values[make_these_na] <- NA
  
  return(values[1:output_length])
}
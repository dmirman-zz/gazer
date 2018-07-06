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
binify_fixations <- function(gaze,
                             binSize = 20,
                             keepCols = c(
                               "Subject", "TrialNumber", "Target", "T"),
                             maxTime = NULL) {

  # Need to know when fixations end
  if ("CURRENT_FIX_END" %in% names(gaze)) {
    gaze$FixEnd <- gaze$CURRENT_FIX_END
  } else {
    # compute end of fixation from start and duration
    gaze$FixEnd <- gaze$CURRENT_FIX_START + gaze$CURRENT_FIX_DURATION
  }

  # If maxTime is defined, do some trimming.
  if (!is.null(maxTime)) {
    # drop all fixations that start after the maxTime
    gaze <- subset(gaze, CURRENT_FIX_START < maxTime)
    # trim fixation end times to be less than maxTime
    gaze$FixEnd[gaze$FixEnd > maxTime] <- maxTime
  }

  # Make a fixation ID variable that is just the fixation number in the overall
  # data frame
  gaze$FixationID <- seq_len(nrow(gaze))

  # Split: Make a list of dataframes for each fixation
  gaze_list <- split(gaze, gaze$FixationID)

  # Apply: A function to expand a single fication
  expand_fixation <- function(df) {
    fix_bins <- seq(
      from = ceiling(df[["CURRENT_FIX_START"]] / binSize),
      to = ceiling(df[["FixEnd"]] / binSize))
    data.frame(
      FixationID = unique(df[["FixationID"]]),
      timeBin = fix_bins)
  }

  # Combine: Expand each fixation and reduce to a single dataframe
  gaze_bins <- dplyr::bind_rows(lapply(gaze_list, expand_fixation))

  # There is a border case in which two redundant bins can be generated.
  # Clean them up by keeping the second one.
  gaze_bins <- subset(
    gaze_bins,
    timeBin[2:length(timeBin)] != timeBin[1:(length(timeBin)-1)])

  # Check names in keepCcols
  missing_names <- keepCols[!(keepCols %in% names(gaze))]
  if (length(missing_names) > 0) {
    quoted <- sprintf("`%s`", missing_names)
    warning(
      paste0(
        quoted, collapse = ", "), " are not columns in the fixation dataframe")
  }

  # Combine binned data with information from original df
  keepCols <- keepCols[keepCols %in% names(gaze)]
  dataFull <- merge(
    gaze_bins, gaze[, c(keepCols, "FixationID")], by = "FixationID")

  # Add a variable with actual time instead of time bin
  dataFull$Time <- dataFull$timeBin * binSize

  dataFull
}

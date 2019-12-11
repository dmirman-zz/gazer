#' Downsample gaze data
#'
#' This function will combine gaze samples into time bins
#' @param dataframe dataframe
#' @param bin.length Length of time bins
#' @param timevar time column
#' @param aggvars vector of variable names to group by for aggregation
#' @param type pupil for pupil data gaze for gaze data
#' @export
downsample_gaze <- function(dataframe, bin.length = 50, timevar = "time", aggvars = c("subject", "condition", "target", "trial", "object", "timebins"), type="gaze"){
  if(type=="gaze") {
  downsample <- dataframe %>%
    mutate(timebins = round(!!sym(timevar)/bin.length)*bin.length)
  # if there are aggregation variables, then aggregate
  if(length(aggvars > 0)){
    downsample <- downsample %>%
      dplyr::group_by_(.dots = aggvars) %>%
      dplyr::summarize(acc = unique(acc), rt = unique(rt),
                       Fix = mean(Fix) > 0.5) %>%
      dplyr::ungroup()
  }
  return(downsample)
}
  if (type=="pupil") {
    downsample <- dataframe %>%
      mutate(timebins = round(!!sym(timevar)/bin.length)*bin.length)
    # if there are aggregation variables, then aggregate
    if(length(aggvars > 0)){
      downsample <- downsample %>%
        dplyr::group_by(.dots = aggvars) %>%
        dplyr::summarize(aggbaseline=mean(baselinecorrectedp)) %>%
        dplyr::ungroup()
    }
  }
  return(downsample)
}



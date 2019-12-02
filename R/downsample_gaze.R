#' Downsample gaze data
#'
#' This function will combine gaze samples into time bins
#' @param dataframe dataframe
#' @param bin.length Length of time bins
#' @param aggvars list of variable names to group by for aggregation
#' @export
downsample_gaze <- function(dataframe, bin.length = 50, timevar = "time", aggvars = list("subject", "condition", "target", "trial", "object", "timebin")){
  downsample <- dataframe %>%
    mutate(timebin = round(!!sym(timevar)/bin.length)*bin.length)
  # if there are aggregation variables, then aggregate
  if(length(aggvars > 0)){
    downsample <- downsample %>%
      dplyr::group_by_(.dots = aggvars) %>%
      dplyr::summarize(acc = unique(acc), rt = unique(rt),
                       Fix = mean(Fix) > 0.5) %>%
      ungroup()
  }
  return(downsample)
}


#' Downsample pupil data
#'
#' This function will reduce the sampling fequency
#' @param dataframe dataframe
#' @param bin.length Length of bins to average
#' @export
downsample_pupil <- function(dataframe, bin.length = NULL){
  downsample <- dataframe %>%
        dplyr::group_by(subject, trial) %>% 
                     dplyr::mutate(
                     timebin = trunc(time_zero/bin.length),
                     timebin = ifelse(time_zero<0, timebin - 1, timebin),
                     timebins = timebin*bin.length) %>% 
                      select(-timebin) %>%
                      ungroup()
  return(downsample)
}


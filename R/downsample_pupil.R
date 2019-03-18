#' Downsample pupil data
#'
#' This function will reduce the sampling fequency
#' @param df dataframe
#' @param bin.length Length of bins to average
#' @export
downsample_pupil <- function(df, bin.length = NULL){
  downsample <- df %>%
        dplyr::group_by(trial) %>% 
                     dplyr::mutate(
                     timebin = trunc(time_0/bin.length),
                     timebin = ifelse(time_0<0, timebin - 1, timebin),
                     timebins = timebin*bin.length) %>% 
                      select(-timebin) %>%
                      ungroup()
  return(downsample)
}


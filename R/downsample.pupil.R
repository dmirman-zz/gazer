#' Downsample pupil data
#'
#' This function will reduce the sampling fequency
#' @param df dataframe
#' @param bin.length Length of bins to average
#' @keywords downsample
#' @export
#' @examples
#' pupil.downsample(df, bin.length = 100)
downsample.pupil <- function(df, bin.length = NULL){
  downsample <- df %>%
        dplyr::group_by(trial) %>% 
                     dplyr::mutate(
                     timebin = trunc(time/bin.length),
                     timebin = ifelse(time<0, timebin - 1, timebin),
                     time = timebin*bin.length) %>% 
                      ungroup()
  return(downsample)
}


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
  x <- df %>%
        dplyr::group_by(trial) %>% 
                     dplyr::mutate(
                     timeBin = trunc(time/bin.length),
                     timeBin = ifelse(time<0, timebin - 1, TimeBin),
                     time = timebin*bin.length) %>% 
                      ungroup()
  return(x)
}


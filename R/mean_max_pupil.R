#' Calculates mean and max pupil size for each trial
#' @param datafile raw pupil data
#' @return data frame containing data with mean and max pupil size appended 

mean_max_pupil <- function(datafile) { 
  message("Calculating mean and max")
  meanmax_pupil <- datafile %>%
    dplyr::group_by(subject, trial) %>%
    dplyr::mutate(mean_pupil = mean(baselinecorrectedp),
                     max_pupil = max(baselinecorrectedp)) %>% ungroup()
  return(meanmax_pupil)
  }
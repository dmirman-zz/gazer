#' Performs linear (substraction) baseline correction relative to desired stim_onset and off_set
#' @param datafile raw pupil data
#' @param baseline_window user-specified threshold for baseline window.
#' @return data frame containing baseline corrected data from event of interest
#' @export
baseline_correction_pupil<-function(datafile,baseline_window=NA) { message("Calculating baseline")
  #recent paper (Mathot et al.,2018)suggested using median over mean for baseline correction
  message("Calculating median baseline from",":", baseline_window[1], "-", baseline_window[2])
  baseline <- datafile  %>%
    dplyr::filter(time > baseline_window[1],
                  time < baseline_window[2]) %>%
    dplyr::group_by(subject, trial) %>%
    dplyr::summarise(baseline = median(movingavgpup)) %>% ungroup()
  message("Merging baseline")
  merge_baseline <- merge(baseline, datafile) # merge median pupil size with raw dataset
  message("Performing baseline correction")
  corrected_baseline <- merge_baseline %>%  dplyr::mutate(baselinecorrectedp = movingavgpup - baseline) %>% dplyr::arrange(trial, time)
  
  return(corrected_baseline)

  }
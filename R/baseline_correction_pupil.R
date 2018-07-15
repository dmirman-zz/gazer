#' Performs linear (substraction) baseline correction relative to desired stim_onset and off_set
#' @param datafile raw pupil data
#' @param baseline_window user-specified threshold for baseline window.
#' @param binsize user-specified threshold for binning data.
#' @param stim_onset user-specified threshold for when stimulus of interest appears.
#' @param stim_offset  user-specified threshold for trial offset 
#' @return data frame containing baseline corrected data from event of interest
baseline_correction_pupil<-function(datafile,baseline_window=NA,stim_onset=NA, stim_offset=NA) { 
  #recent paper (Mahot, 2018)suggested using median over mean for baseline correction
    baseline <- datafile  %>%
    dplyr::filter(timebins > baseline_window[1],
                  timebins < baseline_window[2]) %>%
    dplyr::group_by(subject, trial) %>%
    dplyr::summarise(baseline = median(movingavgpup, na.rm = TRUE)) %>% ungroup()
  
  merge_baseline <- merge(baseline, datafile) # merge median pupil size with raw dataset
  
  corrected_baseline <- merge_baseline %>% dplyr::filter(timebins>=stim_onset & timbins<=stim_offset) %>%  dplyr::mutate(baselinecorrectedp = movingavgpup - baseline) %>% dplyr::arrange(trial, time) #clip trials relative to stim onset and offset
  #perform baseline subtraction
  
  return(corrected_baseline)

  }
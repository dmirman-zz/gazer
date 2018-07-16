#' Performs linear (substraction) baseline correction relative to desired stim_onset and off_set
#' @param datafile raw pupil data
#' @param baseline_window user-specified threshold for baseline window.
#' @param binsize user-specified threshold for binning data.
#' @param stim_onset user-specified threshold for when stimulus of interest appears.
#' @param stim_offset  user-specified threshold for trial offset 
#' @return data frame containing baseline corrected data from event of interest
baseline_correction_pupil<-function(datafile,baseline_window=NA,stim_onset=NA, stim_offset=NA) { message("Calculating baseline")
  #recent paper (Mahot, 2018)suggested using median over mean for baseline correction
    baseline <- datafile  %>%
    dplyr::filter(timebins > baseline_window[1],
                  timebins < baseline_window[2]) %>%
    dplyr::group_by(subject, trial) %>%
    dplyr::summarise(baseline = median(movingavgpup)) %>% ungroup()
  message("Merging baseline")
  merge_baseline <- merge(baseline, datafile) # merge median pupil size with raw dataset
  message("Clipping trials")
  message("Performing baseline correction")
  corrected_baseline <- merge_baseline %>% dplyr::filter(timebins>=stim_onset & timebins<=stim_offset) %>%  dplyr::mutate(baselinecorrectedp = movingavgpup - baseline) %>% dplyr::arrange(trial, timebins) #clip trials relative to stim onset and offset
  #perform baseline subtraction
  message("setting trial at zero")
  zero_onset <- corrected_baseline %>% dplyr::mutate(timebinonset=timebins-timebins[[1]])
  
  return(zero_onset)

  }
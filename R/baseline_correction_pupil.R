#' Performs linear (substraction) or divisive baseline correction relative to desired stim_onset and off_set
#' @param datafile raw pupil data
#' @param pupil_colnames name of your pupil colname you want baseline corrected
#' @param baseline_window user-specified threshold for baseline window.
#' @param baseline correction method. Default is sub but can also include divisive
#' @return data frame containing baseline corrected data from event of interest
#' @export
baseline_correction_pupil<-function(datafile, pupil_colnames=NULL, baseline_window=NA, baseline_method="sub") { message("Calculating baseline")
  #recent paper (Mathot et al.,2018)suggested using median over mean for baseline correction
  message("Calculating median baseline from",":", baseline_window[1], "-", baseline_window[2])
  baseline <- datafile  %>%
    dplyr::filter(time > baseline_window[1],
                  time < baseline_window[2]) %>%
    dplyr::group_by(subject, trial) %>%
    dplyr::rename(pupil_avg = pupil_colnames) %>% 
    dplyr::summarise(baseline = median(pupil_avg)) %>% 
    ungroup()
  
  message("Merging baseline")
  merge_baseline <- merge(baseline,datafile,all=TRUE) # merge median pupil size with raw dataset
  
  if (baseline_method=="sub") {
  message("Performing subtractive baseline correction")
  
    corrected_baseline <- merge_baseline %>% 
    dplyr::rename(pupil_avg = pupil_colnames) %>% 
    dplyr::mutate(baselinecorrectedp = pupil_avg - baseline) %>%
      dplyr::rename(movingavgpuup = pupil_avg) %>%
      dplyr::arrange(subject, trial, time)
  }
  if (baseline_method=="div") { 
    message("Performing divisive baseline correction")
    
    corrected_baseline <- merge_baseline %>% 
      dplyr::rename(pupil_avg = pupil_colnames) %>% 
      dplyr::mutate(baselinecorrectedp = (pupil_avg - baseline)/baseline) %>%
      dplyr::rename(movingavgpuup = pupil_avg) %>%
      dplyr::arrange(subject, trial, time)
    
    }
    
  return(corrected_baseline)

  }
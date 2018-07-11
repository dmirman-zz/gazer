#' Preprocessing clean-up of messy pupil data.
#'
#' Turn all blinks NA, linear interpolation of NA values
#' user-specified thresholds for baseline values, binsizes, and stimulus onset
#'
#' @param datafile raw pupil data
#' @param baseline_window user-specified threshold for baseline window.
#' @param binsize user-specified threshold for binning data.
#' @param stim_onset user-specified threshold for when stimulus of interest appears.
#' @param smoothing Low-pass filtering or moving average
#' @export
#' @return data frame containing pre-processed pupil data
preprocess_pupil <- function(datafile, baseline_window=NA, binsize=NA, stim_onset=NA, smoothing=NA){

  require(itsadug)
  require(zoo)

  blinks_na <- datafile %>% mutate(pup = ifelse(blink==1, NA, pupil))

  pupil_interp <- blinks_na %>%
    dplyr::group_by(subject, trial, time) %>%
    mutate(interp = na.approx(pup, rule=2),
           timebins = timeBins(time, binsize))  #places data into timebins =  50, 150 ms, 250, ms

  if (smoothing=="LPF") {
    pupil_data_lf<- pupil_interp %>%
      dplyr::group_by(subject,trial, timebins) %>%
      dplyr::mutate(pupil_smoothed_lf=low_pass_filter(ts = interp, samp_freq=250)) %>% ungroup() #need to run the low_pass_filter function first
    #baseline_window baselinemin #getting baseline pupil dimater. This takes 500 ms preceding the oneset of the similarity judgmenets.
    baseline <- pupil_data_lf %>%
      dplyr::filter(timebins > baseline_window[1],
                    timebins < baseline_window[2]) %>%
      dplyr::group_by(subject, trial) %>%
      dplyr::summarise(baseline = median(pupil_smoothed_lf, na.rm = TRUE)) %>% ungroup() #recent paper (Mahot, 2018)suggested using median over mean for baseline correction

    merge_baseline <- merge(baseline, pupil_data_lf) # merge median pupil size with raw dataset

    corrected_baseline <- merge_baseline %>%  dplyr::mutate(baselinecorrectedp = pupil_smoothed_lf - baseline) %>%
      dplyr::filter(timebins>=stim_onset) %>% dplyr::arrange(trial, time)# baseline subtraction

    # merge mean and max per trial with corrected baseline data
    meanmax_pupil <- corrected_baseline %>%
      dplyr::group_by(subject, trial) %>%
      dplyr::summarise(mean_pupil = mean(baselinecorrectedp, na.rm=TRUE),
                       max_pupil = max(baselinecorrectedp, na.rm=TRUE))

    #when does stimulus onset occur.

    # merge and tidy up
    corrected_pupil_baseline <- merge(corrected_baseline,
                                      meanmax_pupil[, c("subject", "trial", "mean_pupil", "max_pupil")],
                                      by=c("subject", "trial"))

    corrected_pupil_baseline <- corrected_pupil_baseline %>% arrange(subject, trial, time)

    return(corrected_pupil_baseline)
  }
   else {
  rolling_mean_pupil_average<-as.data.frame(pupil_interp) %>% #must be in a data.frame
    dplyr::select(subject, trial, time, interp) %>%
    dplyr::mutate(movingavgpup= movingAverage(interp,n=3)) #use a n point moving average to smooth the data #must run the movingaverage function first
  interp_pupil_avg<-merge(rolling_mean_pupil_average,pupil_interp)#merge moving average pupil size with raw data

#baseline_window baselinemin #getting baseline pupil dimater. This takes 500 ms preceding the oneset of the similarity judgmenets.
  baseline <- interp_pupil_avg %>%
    dplyr::filter(timebins > baseline_window[1],
                  timebins < baseline_window[2]) %>%
  dplyr::group_by(subject, trial) %>%
  dplyr::summarise(baseline = median(movingavgpup, na.rm = TRUE)) %>% ungroup()#recent paper (Mahot, 2018)suggested using median over mean for baseline correction

  merge_baseline <- merge(baseline, interp_pupil_new) # merge median pupil size with raw dataset

corrected_baseline <- merge_baseline %>%  dplyr::mutate(baselinecorrectedp = movingavgpup - baseline) %>%
  dplyr::filter(timebins>=stim_onset) %>% dplyr::arrange(trial, time)

# merge mean and max per trial with corrected baseline data
meanmax_pupil <- corrected_baseline %>%
  dplyr::group_by(subject, trial) %>%
  dplyr::summarise(mean_pupil = mean(baselinecorrectedp, na.rm=TRUE),
            max_pupil = max(baselinecorrectedp, na.rm=TRUE)) %>% ungroup()

#when does stimulus onset occur.

# merge and tidy up
corrected_pupil_baseline <- merge(corrected_baseline,
                                  meanmax_pupil[, c("subject", "trial", "mean_pupil", "max_pupil")],
                                  by=c("subject", "trial"))

corrected_pupil_baseline <- corrected_pupil_baseline %>% dplyr::arrange(subject, trial, time)

return(corrected_pupil_baseline)
   }
}


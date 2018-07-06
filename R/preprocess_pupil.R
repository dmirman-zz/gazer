#' Preprocessing clean-up of messy pupil data.
#'
#' Turn all blinks NA, linear interpolation of NA values
#' user-specified thresholds for baseline values, binsizes, and stimulus onset
#'
#' @param datafile raw pupil data
#' @param baseline_window user-specified threshold for baseline window.
#' @param binsize user-specified threshold for binning data.
#' @param stim_onset user-specified threshold for when stimulus of interest appears.
#' @export
#' @return data frame containing gaze diagnostics
preprocess_pupil <- function(datafile, baseline_window=NA, binsize=NA, stim_onset=NA){
  blinks_na <- datafile %>% mutate(pup = ifelse(blink==1, NA, pupil))

  pupil_interp <- blinks_na %>% 
        group_by(subject, trial, time) %>%
        mutate(interp = na.approx(pup, rule=2), 
               timebins = timeBins(time, binsize))  #places data into timebins =  50, 150 ms, 250, ms

  #baseline_window baselinemin #getting baseline pupil dimater. This takes 500 ms preceding the oneset of the similarity judgmenets.
  baseline <- pupil_interp %>%
       dplyr::filter(timebins > baseline_window[1],
                timebins < baseline_window[2])) %>%
       group_by(subject, trial) %>%
       summarise(baseline = mean(interp, na.rm = TRUE))
  
  # Change names of columns for merge
  colnames(baseline) <- c("Subject", "trial", "baseline")
  #baseline<-subset(baseline, baseline$baseline>LBbaseline) # throws away small pupil values before baseline
  # merge baseline mean with raw datase)
  baseline1 <- merge(baseline, pupil_interp) 

  corrected_baseline <- baseline1 %>%  mutate(baselinecorrectedp = interp - baseline) %>%
    filter(timebins>=stim_onset) %>% arrange(trial, time)

  # merge mean and max per trial with corrected baseline data
  meanmax_pupil <- corrected_baseline %>%
       group_by(subject, trial) %>%
       summarise(mean_pupil = mean(baselinecorrectedp, na.rm=TRUE),
            max_pupil = max(baselinecorrectedp)) 

  #when does stimulus onset occur.
  
  # merge and tidy up
  corrected_pupil_baseline <- merge(corrected_baseline,
                                    meanmax_pupil[, c("subject", "trial", "mean_pupil", "max_pupil")],
                                    by=c("subject", "trial"))

  corrected_pupil_baseline <- corrected_pupil_baseline %>% arrange(subject, trial, time) 

  return(corrected_pupil_baseline)
}

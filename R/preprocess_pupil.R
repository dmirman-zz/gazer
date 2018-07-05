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
  blinks_na <- mutate(datafile, pup=ifelse(blink==1, NA, pupil))

  pupil_interp <- blinks_na %>% group_by(subject, trial, time) %>%
    mutate(interp=na.approx(pup, rule=2), timebins=timeBins(time, binsize))  #places data into timebins =  50, 150 ms, 250, ms

  baseline <- aggregate(pupil_interp$interp~pupil_interp$subject + pupil_interp$trial,
                        data = pupil_interp, median,
                        subset = pupil_interp$timebins > baseline_window[1] &
                          pupil_interp$timebins < baseline_window[2]) #baseline_window baselinemin #getting baseline pupil dimater. This takes 500 ms preceding the oneset of the similarity judgmenets.
  colnames(baseline) <- c("Subject", "trial", "baseline")#change names of columns for merge
  #baseline<-subset(baseline, baseline$baseline>LBbaseline) # throws away small pupil values before baseline
  baseline1=merge(baseline, pupil_interp) #merge(baseline mean with raw datase)

  corrected_baseline <- baseline1 %>%  mutate(baselinecorrectedp=interp-baseline) %>%
    filter(timebins>=stim_onset) %>% arrange(trial, time)

  meanmax_pupil <- ddply(corrected_baseline, .(subject, trial), summarise,
                       mean_pupil=mean(baselinecorrectedp, na.rm=TRUE),
                       max_pupil=max(baselinecorrectedp)) #merge mean and max per trial with corrected baseline data
  #when does stimulus onset occur.

  corrected_pupil_baseline <- merge(corrected_baseline,
                                    meanmax_pupil[, c("subject", "trial", "mean_pupil", "max_pupil")],
                                    by=c("subject", "trial"))

  corrected_pupil_baseline=corrected_pupil_baseline %>% arrange(subject, trial, time) #merge and tidy up

  return(corrected_pupil_baseline)
}

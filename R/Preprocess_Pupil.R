preprocess_pupil=function(datafile, baseline_window=NA, binsize=NA, stim_onset=NA){

  #Turns all Blinks Missing (NA)  #convert arbitrary pupil size to mm. This was done by using a fake pupil (5 mm in size). The ET recorded pupil size during a short experiment. Pupil size was averaged across the trials (5570.29). This information was entered into equation to get mm
  blinks_na<-mutate(datafile,
                    pup=ifelse(blink==1, NA, pupil))

  pupil_interp<- blinks_na%>% group_by(subject, trial, time) %>% mutate(interp=na.approx(pup, rule=2), timebins=timeBins(time, binsize))  #places data into timebins =  50, 150 ms, 250, ms

  baseline=aggregate(pupil_interp$interp~pupil_interp$subject + pupil_interp$trial, data=pupil_interp, median,
                     subset=pupil_interp$timebins > baseline_window[1] & pupil_interp$timebins < baseline_window[2])#baseline_window baselinemin #getting baseline pupil dimater. This takes 500 ms preceding the oneset of the similarity judgmenets.
  colnames(baseline)=c("Subject", "trial", "baseline")#change names of columns for merge
  #baseline<-subset(baseline, baseline$baseline>LBbaseline) # throws away small pupil values before baseline
  baseline1=merge(baseline, pupil_interp) #merge(baseline mean with raw datase)

  corrected_baseline= baseline1 %>%  mutate(baselinecorrectedp=interp-baseline) %>% filter(timebins>=stim_onset) %>% arrange(trial, time)

  meanmax_pupil_size<- corrected_baseline %>% group_by(subject, trial) %>% mutate(baselinemean=mean(baselinecorrectedp, na.rm=TRUE), baselinemax=max(baselinecorrectedp, na.rm = TRUE)) #get the mean and max pupil size

  corrected_pupil_baseline<-merge(corercted_baseline, meanmax_pupil_size) #merge mean and max per trial with corrected baseline data

  #when does stimulus onset occur.
  #write.csv(corrected_baseline, file="corrected_baseline_pupil.csv")
  return(corrected_pupil_baseline)
}

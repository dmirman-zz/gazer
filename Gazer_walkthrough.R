library(devtools)
remotes::install_github("dmirman/gazer")
library(gazer)
library(tidyverse)
library(zoo)
library(knitr)
library(data.table)
library(tidyverse)
# load in data
gaze_path <- system.file("extdata", "vwp_data_raw_edf.xls", package = "gazer")
gaze_path <- fread(gaze_path) # reads in large datasets quickly
gaze <- as_tibble(gaze_path) # save as tibble
head(gaze)
summary(gaze)


gaze_track <- get_trackloss(gaze, screen_size=c(1024, 768),missingthresh=.2)


gaze$TargetLocation <- as.numeric(substr(gaze$correctport, 6, 6))
gaze$CompLocation <- as.numeric(substr(gaze$comport, 6, 6))


gaze_aoi <- gazer::assign_aoi(gaze, screen_size=c(1024, 768), aoi_size=c(400, 300), aoi_loc=NULL, X="x", Y="y")
summary(gaze_aoi)

gaze_aoi$Targ <- gaze_aoi$AOI == gaze_aoi$TargetLocation
gaze_aoi$Comp <- gaze_aoi$AOI == gaze_aoi$CompLocation
gaze_aoi$Unrelated <- 
  ((gaze_aoi$AOI != as.numeric(gaze_aoi$TargetLocation)) &
     (gaze_aoi$AOI != as.numeric(gaze_aoi$CompLocation)) &
     (gaze_aoi$AOI != 0) & !is.na(gaze_aoi$AOI))

gaze_obj <- gather(gaze_aoi, 
                   key = "object", value = "fix", 
                   Targ, Comp, Unrelated, factor_key = TRUE)
# recode NA as not-fixating
gaze_obj$Fix <- replace(gaze_obj$fix, is.na(gaze_obj$fix), FALSE) 
summary(gaze_obj)

gaze_subj <- gaze_obj %>% 
  filter(acc == 1, condition != "practice", time < 3500) %>% 
  # calculate number of valid trials for each subject-condition
  group_by(subject, condition, object) %>% 
  mutate(nTrials = length(unique(target))) %>% ungroup() %>%
  # calculate number of fixations 
  group_by(subject, condition, object, time) %>%
  summarize(sumfix = sum(fix, na.rm=TRUE), # number of fixations
            ntrials = unique(nTrials), # number of trials
            meanfix = sum(fix, na.rm=TRUE)/unique(nTrials)) # fixation proportion
# there were two unrelated objects, so divide those proportions by 2
# there were two unrelated objects, so divide those proportions by 2
gaze_subj$meanfix[gaze_subj$object == "Unrelated"] <- 
  gaze_subj$meanfix[gaze_subj$object == "Unrelated"] / 2
summary(gaze_subj)

ggplot(gaze_subj, aes(time, meanfix, color = object)) + 
  facet_wrap(~ condition) +
  theme_gray() +
  labs(x = "Time (ms)",y = "Proportion of Fixations", colour = NULL) +
  stat_summary(fun.y = mean, geom = "line") +
  geom_vline(xintercept = 1300) +
  annotate("text", x=1300, y=0.9, label="Word onset", hjust=0) + 
  
  theme(axis.title.y=element_text(size = 12, face="bold"), axis.title.x = element_text(size=12,   face="bold"), axis.text.x=element_text(size = 12, face="bold"),axis.text.y=element_text(size=12, face="bold"), strip.text = element_text(size=14)) 

#### Pupil functions

pupil_path <- system.file("extdata", "sample_pupil_edf.xls", package = "gazer")
pupil_files1<-fread(pupil_path)
pupil_files1 <- as_tibble(pupil_files1)
summary(pupil_files1)



behave_data<-behave_pupil(pupil_files1, omiterrors = FALSE, behave_colnames = c("subject","script","alt", "trial", "item","acc","rt", "block"))
behave_data

itemacc<-behave_data %>% 
  dplyr::group_by(item) %>% 
  dplyr::summarise(meanitemacc = mean(acc[block>0 & alt=="word"])) #overall item accuracy

subacc<-behave_data %>% 
  dplyr::group_by(subject) %>% 
  dplyr::summarise (meansubacc = mean(acc[block>0 & alt=="word"]))#subject accuracy

dataraw1<-merge(pupil_files1, itemacc)#merge into main ds
dataraw2<-merge(dataraw1, subacc)#merge into main ds


pupil_files1<-dataraw2 %>%
  dplyr::filter(block>0  & acc==1 & alt=="word" & meanitemacc>.60 & meansubacc>.74) %>% 
  arrange(subject,trial, time)

#Extend Blinks
pup_extend<- pupil_files1 %>% 
  group_by(subject, trial) %>% 
  mutate(extendpupil=extend_blinks(pupil, fillback=100, fillforward=100, hz=250))
# Smooth and Interpolate
smooth_interp <- smooth_interpolate_pupil(pup_extend, pupil="pupil", extendpupil="extendpupil", extendblinks=TRUE, method.first="smooth", maxgap=Inf, type="linear", hz=250, n=5) {

  interp_graph <- pup_interp  %>%
    dplyr::filter(subject=="10b.edf", trial=="15")
  
  bold <- element_text(face = "bold", color = "black", size = 14) #axis bold
#Graph interpolation  
  pup_g<- ggplot(interp_graph, aes(x= time, y= pupil)) + geom_point()+ geom_line(colour="black") + 
    geom_line(aes(x=time, y=interp), colour="darkgreen") + xlab("Time (ms)") + ylab("Pupil Size (arbitrary units)") + theme_bw() + theme(axis.title.y=element_text(size = 16, face="bold"), axis.title.x = element_text(size=16, face="bold"), axis.text.x=element_text(size = 12, face="bold"), axis.text.y=element_text(size=12, face="bold"))
  print(pup_g)
  
#Baseline
baseline_pupil<-baseline_correction_pupil(smooth_interp, pupil_colnames='interp', baseline_window=c(500,1000)) 
#removing missing data
pup_missing<-count_missing_pupil(baseline_pupil, missingthresh = .2)
# remove outliers
pup_outliers<-pup_missing %>% 
  dplyr::filter (interp  >= 2500, interp <= 5100) # based on visual inspection
#MAD
max_pup<-pup_outliers  %>% 
  dplyr::group_by(subject, trial) %>% 
  dplyr::mutate(speed=speed_pupil(interp,time))

mad_pup<-max_pup %>% 
  dplyr::group_by(subject, trial) %>% 
  dplyr::mutate(MAD=calc_mad(speed))

mad_removal<-mad_pup %>% 
  dplyr::filter(speed < MAD)

#onset to stimulus

baseline_pupil_onset<-mad_removal %>% 
  dplyr::group_by(subject, trial) %>%  
  dplyr::mutate(time_zero=onset_pupil(time, message, event=c("target"))) %>%
  ungroup() %>% 
  dplyr::filter(time_zero >= 0 & time_zero <= 2500) %>%
  select(subject, trial, time,baselinecorrectedp, script, time_zero,message,baselinecorrectedp)

timebins1<- downsample_pupil(baseline_pupil_onset, 100)
data(cursive_agg_data)

# plot data
cursive_plot <- ggplot(cursive_agg_data)+  geom_line(aes(timebins, aggbaseline, linetype=script, color=script), size=3) +
  theme_bw() +
  scale_colour_manual(values=c("orange", "dark green"), name="Script") + 
  scale_linetype_manual(values = c("solid", "dotted"), name="Script")+ 
  #stat_summary(fun.y = "mean",geom = "line",size = 1,aes(colour = NULL)) +
  labs(x = "Time (ms)",y = "Pupil Dilation (change from baseline (a.u.))", colour = NULL) +
  #geom_hline(yintercept = 0,linetype = "dashed") +
  #geom_ribbon(data = WSCI, aes(ymin = aggbaseline-ci, ymax = aggbaseline+ci, linetype=script, colour=script),  alpha = 0.3) +
  theme(axis.title.y=element_text(size = 14, face="bold"), axis.title.x = element_text(size=14,   face="bold"), axis.text.x=element_text(size = 12, face="bold"),axis.text.y=element_text(size=12, face="bold")) +
  ggtitle('Pupillary Timecourse by Script Type')

cursive_plot


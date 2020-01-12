library(devtools)
remotes::install_github("dmirman/gazer")
library(gazer)
library(tidyverse)
library(zoo)
library(knitr)
library(data.table)
library(tidyverse)
library(here)

here('R')

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

gaze_obj <-gaze_aoi %>%
  dplyr::gather(key ="object", value ="fix",Targ, Comp, Unrelated, factor_key =TRUE) %>%
  dplyr::mutate(Fix =replace_na(fix, FALSE)) # recode NA as not-

bin_gaze <- downsample_gaze(gaze_obj, bin.length = 50, timevar = "time", aggvars = c("subject", "condition", "target", "trial", "object", "timebins"))

head(bin_gaze)


ggaze_subj <-bin_gaze %>%
  filter(acc ==1, condition != "practice", timebins <3500) %>%
  # calculate number of valid trials for each subject-condition
  group_by(subject, condition, object, timebins) %>%
  summarize(meanfix =mean(Fix, na.rm=TRUE)) #
#there were two unrelated objects, so divide those proportions by 2
gaze_subj$meanfix[gaze_subj$object == "Unrelated"] <-
gaze_subj$meanfix[gaze_subj$object == "Unrelated"] / 2
summary(gaze_subj)

ggplot(gaze_subj, aes(timebins, meanfix, color = object)) +
  facet_wrap(~ condition) +
  theme_gray() +
  labs(x = "Time (ms)",y = "Proportion of Fixations", colour = NULL) +
  stat_summary(fun.y = mean, geom = "line") +
  geom_vline(xintercept = 1300) +
  annotate("text", x=1300, y=0.9, label="Word onset", hjust=0) +

  theme(axis.title.y=element_text(size = 12, face="bold"), axis.title.x = element_text(size=12,   face="bold"), axis.text.x=element_text(size = 12, face="bold"),axis.text.y=element_text(size=12, face="bold"), strip.text = element_text(size=14))

#### Pupil functions

pupil_path <- system.file("extdata", "pupil_sample_files_edf.xls", package = "gazer")
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

dataraw1<-dplyr::full_join(pupil_files1, itemacc)#merge into main ds
dataraw2<-dplyr::full_join(dataraw1, subacc)#merge into main ds


pupil_files1<-dataraw2 %>%
  dplyr::filter(block>0  & acc==1 & alt=="word" & meanitemacc>.60 & meansubacc>.74) %>%
  arrange(subject,trial, time)

#Extend Blinks
pup_extend<- pupil_files1 %>%
  group_by(subject, trial) %>%
  mutate(extendpupil=extend_blinks(pupil, fillback=100, fillforward=100, hz=250))

# Smooth and Interpolate
smooth_interp <- smooth_interpolate_pupil(pup_extend, pupil="pupil", extendpupil="extendpupil", extendblinks=TRUE, step.first="smooth", maxgap=Inf, type="linear", hz=250, n=5)

interp_graph <- pup_interp  %>%
  dplyr::filter(subject=="10b.edf", trial=="15")

bold <- element_text(face = "bold", color = "black", size = 14) #axis bold
#Graph interpolation
pup_g<- ggplot(interp_graph, aes(x= time, y= pupil)) + geom_point()+ geom_line(colour="black") +
  geom_line(aes(x=time, y=interp), colour="darkgreen") + xlab("Time (ms)") + ylab("Pupil Size (arbitrary units)") + theme_bw() + theme(axis.title.y=element_text(size = 16, face="bold"), axis.title.x = element_text(size=16, face="bold"), axis.text.x=element_text(size = 12, face="bold"), axis.text.y=element_text(size=12, face="bold"))
print(pup_g)

#Baseline
baseline_pupil<-baseline_correction_pupil(smooth_interp, pupil_colnames='interp', baseline_window=c(500,1000))

#use messages to baseline correct
baseline_pupil<-baseline_correction_pupil_msg(smooth_interp, pupil_colname='pup_interp', baseline_dur=100, event="target", baseline_method = "sub")

head(baseline_pupil)

#removing missing data
pup_missing<-count_missing_pupil(baseline_pupil, missingthresh = .2)
# remove outliers
pup_outliers<-pup_missing %>%
  dplyr::filter (pup_interp  >= 2500, pup_interp <= 5100) # based on visual inspection
#MAD removal
max_removal<-pup_outliers  %>%
  dplyr::group_by(subject, trial) %>%
  dplyr::mutate(speed=speed_pupil(pup_interp,time)) %>%
  dplyr::mutate(MAD=calc_mad(speed)) %>%
  dplyr::filter(speed < MAD)

#onset to stimulus

baseline_pupil_onset<-max_removal %>%
  dplyr::group_by(subject, trial) %>%
  dplyr::mutate(time_zero=onset_pupil(time, message, event=c("target"))) %>%
  ungroup() %>%
  dplyr::filter(time_zero >= -100 & time_zero <= 2500) %>%
  select(subject, trial, time,baselinecorrectedp, script, time_zero,message,baselinecorrectedp)

#downsample
timebins1<- downsample_gaze(baseline_pupil_onset, bin.length=100, timevar = "time_zero", aggvars = c("subject", "script", "timebins"), type="pupil")

timebins1

# plot data


cursive_plot <-ggplot(timebins1)+
  aes(timebins, aggbaseline, linetype=script, color=script) +
  stat_summary(fun.y = "mean", geom = "line", size = 1) +
  theme_bw() +
  labs(x ="Time (ms)",y ="Pupil Dilation (change from baseline (a.u.))") +
  geom_hline(yintercept=0.0)

cursive_plot

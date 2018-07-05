library(gazer)
library(dplyr)
library(tidyr)
library(ggplot2)

# read in data
gaze <- readFixationReport("data/FixData_v1_N15.xls", plot_fix_scatter = F)
# check calibration diagnostics
cg <- get_gaze_diagnostics(gaze)
# get scatterplot -- note that calibration fig is going flash by
gaze <- readFixationReport("data/FixData_v1_N15.xls")

summary(gaze)

# convert TargetLoc and CompPort to numbers
gaze$TargetLocation <- as.numeric(substr(gaze$TargetLoc,6,6))
gaze$CompLocation <- as.numeric(substr(gaze$CompPort,6,6))

# assign AOI: numeric code for location
gaze_aoi <- assignAOI(gaze)
summary(gaze_aoi)

# determine which object was being fixated by matching AOI codes with target and competitor locations
gaze_aoi$Targ <- gaze_aoi$AOI == gaze_aoi$TargetLocation
gaze_aoi$Comp <- gaze_aoi$AOI == gaze_aoi$CompLocation
gaze_aoi$Unrelated <- ((gaze_aoi$AOI != as.numeric(gaze_aoi$TargetLocation)) &
                         (gaze_aoi$AOI != as.numeric(gaze_aoi$CompLocation)) &
                         (gaze_aoi$AOI != 0) & !is.na(gaze_aoi$AOI))

# convert from fixation list to time bins, only keep the columns needed for analysis
gaze_bins <- binify_fixations(gaze_aoi, keepCols=c("Subject", "Target", "Condition", "ACC", "RT", "Targ", "Comp", "Unrelated"))
summary(gaze_bins)

# gather fixation locations into a single "object" column
gaze_obj <- gather(gaze_bins, key = "Object", value = "Fix", c("Targ", "Comp", "Unrelated"), factor_key = TRUE)
gaze_obj$Fix <- replace(gaze_obj$Fix, is.na(gaze_obj$Fix), FALSE) # recode NA as not-fixating
summary(gaze_obj)

# aggeragate fixation proportions by subject, condition, object and time point
gaze_subj <- gaze_obj %>% filter(ACC==1 & Time < 3500) %>% #only use correct trials
  dplyr::group_by(Subject, Condition, Time, Object) %>%
  summarize(meanFix = mean(Fix))
# there were two unrelated objects, so divide those proportions by 2
gaze_subj$meanFix[gaze_subj$Object=="Unrelated"] <- gaze_subj$meanFix[gaze_subj$Object=="Unrelated"] / 2

summary(gaze_subj)

# make a plot
ggplot(gaze_subj, aes(Time, meanFix, color=Object)) + facet_wrap(~ Condition) +
  stat_summary(fun.y=mean, geom="line")

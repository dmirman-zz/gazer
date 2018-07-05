## ---- message=FALSE, warning=FALSE---------------------------------------
library(gazer)
library(dplyr)
library(tidyr)
library(ggplot2)

## ------------------------------------------------------------------------
# note that the path is likely to be different on your local machine
gaze <- readFixationReport("../data/FixData_v1_N15.xls", plot_fix_scatter = F)
summary(gaze)

## ---- fig.height=5, fig.width=7------------------------------------------
cg <- get_gaze_diagnostics(gaze)

## ----eval=FALSE----------------------------------------------------------
#  gaze <- readFixationReport("../data/FixData_v1_N15.xls")

## ------------------------------------------------------------------------
gaze$TargetLocation <- as.numeric(substr(gaze$TargetLoc, 6, 6))
gaze$CompLocation <- as.numeric(substr(gaze$CompPort, 6, 6))

## ------------------------------------------------------------------------
gaze_aoi <- assignAOI(gaze)
summary(gaze_aoi)

## ------------------------------------------------------------------------
gaze_aoi$Targ <- gaze_aoi$AOI == gaze_aoi$TargetLocation
gaze_aoi$Comp <- gaze_aoi$AOI == gaze_aoi$CompLocation
gaze_aoi$Unrelated <- ((gaze_aoi$AOI != as.numeric(gaze_aoi$TargetLocation)) &
                         (gaze_aoi$AOI != as.numeric(gaze_aoi$CompLocation)) &
                         (gaze_aoi$AOI != 0) & !is.na(gaze_aoi$AOI))

## ------------------------------------------------------------------------
gaze_bins <- binify_fixations(gaze_aoi, keepCols=c("Subject", "Target", "Condition", "ACC", "RT", "Targ", "Comp", "Unrelated"))
summary(gaze_bins)

## ------------------------------------------------------------------------
gaze_obj <- gather(gaze_bins, key = "Object", value = "Fix", c("Targ", "Comp", "Unrelated"), factor_key = TRUE)
gaze_obj$Fix <- replace(gaze_obj$Fix, is.na(gaze_obj$Fix), FALSE) # recode NA as not-fixating
summary(gaze_obj)

## ------------------------------------------------------------------------
gaze_subj <- gaze_obj %>% filter(ACC==1 & Time < 3500 & Condition != "practice") %>% 
  dplyr::group_by(Subject, Condition, Time, Object) %>%
  summarize(meanFix = mean(Fix))
# there were two unrelated objects, so divide those proportions by 2
gaze_subj$meanFix[gaze_subj$Object=="Unrelated"] <- gaze_subj$meanFix[gaze_subj$Object=="Unrelated"] / 2

summary(gaze_subj)

## ---- fig.height=5, fig.width=8------------------------------------------
ggplot(gaze_subj, aes(Time, meanFix, color=Object)) + 
  facet_wrap(~ Condition) +
  stat_summary(fun.y=mean, geom="line")


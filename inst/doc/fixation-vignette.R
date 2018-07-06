## ---- message=FALSE, warning=FALSE---------------------------------------
library(gazer)
library(dplyr)
library(tidyr)
library(ggplot2)

## ------------------------------------------------------------------------
# Use a file installed with the package 
gaze_path <- system.file("extdata", "FixData_v1_N15.xls", package = "gazer")
gaze <- readFixationReport(gaze_path, plot_fix_scatter = FALSE)
summary(gaze)

## ---- fig.height=5, fig.width=7------------------------------------------
cg <- get_gaze_diagnostics(gaze)

## ----eval=FALSE----------------------------------------------------------
#  gaze <- readFixationReport(gaze_path)

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
gaze_bins <- binify_fixations(
  gaze = gaze_aoi, 
  keepCols = c(
    "Subject", "Target", "Condition", "ACC", 
    "RT", "Targ", "Comp", "Unrelated"))
summary(gaze_bins)

## ------------------------------------------------------------------------
gaze_obj <- gather(gaze_bins, 
                   key = "Object", value = "Fix", 
                   Targ, Comp, Unrelated, factor_key = TRUE)

# recode NA as not-fixating
gaze_obj$Fix <- replace(gaze_obj$Fix, is.na(gaze_obj$Fix), FALSE) 
summary(gaze_obj)

## ------------------------------------------------------------------------
gaze_subj <- gaze_obj %>% 
  filter(ACC == 1, Condition != "practice", Time < 3500) %>% 
  # calculate number of valid trials for each subject-condition
  group_by(Subject, Condition, Object) %>% 
  mutate(nTrials = length(unique(Target))) %>% ungroup() %>%
  # calculate number of fixations 
  group_by(Subject, Condition, Object, Time) %>%
  summarize(sumFix = sum(Fix), nTrials = unique(nTrials), 
            meanFix = sum(Fix)/unique(nTrials))

# there were two unrelated objects, so divide those proportions by 2
gaze_subj$meanFix[gaze_subj$Object == "Unrelated"] <- 
  gaze_subj$meanFix[gaze_subj$Object == "Unrelated"] / 2

summary(gaze_subj)

## ---- fig.height=5, fig.width=8------------------------------------------
ggplot(gaze_subj, aes(Time, meanFix, color = Object)) + 
  facet_wrap(~ Condition) +
  stat_summary(fun.y = mean, geom = "line") +
  geom_vline(xintercept = 1300) +
  annotate("text", x=1300, y=0.9, label="Word onset", hjust=0)


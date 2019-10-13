#' Calculate trackloss and remove trials and subjects with excessive trackloss
#'
#' @param gaze fixation list from fixation report
#' @param screen_size size of the screen in pixels. Defaults to c(1024, 768)
#' @param missingthresh threshold to throw out trials and subjs
#' @export
#' @return data frame containing gaze df with trackloss by subjs and trial. Trials and Subjs with excessive trackloss removed
#' 
#' 
get_trackloss <- function(gaze, screen_size=c(1024, 768),missingthresh=.2){
  #measure calibration goodness by computing:
  #	proportion of off-screen fixations
  
  g2 <- data.frame(gaze, screen_width=screen_size[1], screen_height=screen_size[2])
  
  #how many out of bounds samples by sub 
  oob_prop_subj <- g2 %>% dplyr::group_by(subject) %>%
    dplyr::summarise(oob_prop_sub = sum(time[(x > screen_width | x < 0 |
                                                     y > screen_height | y < 0)]) /
                sum(time))
  
#how many out of bounds samples by items 
  oob_prop_trial <- g2 %>% dplyr::group_by(trial) %>%
    dplyr::summarise(oob_prop_tri = sum(time[(x > screen_width | x < 0 |
                                            y > screen_height | y < 0)]) /
                       sum(time))
  
  
  greaterthan <- dplyr::filter(oob_prop_trial, oob_prop_trial$oob_prop_tri > missingthresh)
  prop <- length(greaterthan$trial)/length(oob_prop_trial$trial)
  
  # % trials excluded
  message("% trials excluded:",  round(prop, digits=3))
  message("Participants taken out:" ,oob_prop_subj$subject[oob_prop_subj$oob_prop_sub > missingthresh])
  
  oob_trial=merge(g2, oob_prop_trial, by=c("trial"))
  oob_subject=merge(oob_trial, oob_prop_subj ,by=c("subject"))
  
  combinetrial_above_threshold <- dplyr::filter(oob_subject, (oob_prop_sub < missingthresh), (oob_prop_tri < missingthresh)) %>%
    dplyr::select(-screen_width, -screen_height)
  
  
  return(combinetrial_above_threshold)
}

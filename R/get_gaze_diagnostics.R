#' Calculate gaze track diagnostics
#'
#' Use fixation data to calculate two diagnostics of gaze track quality: non-fixation time and out-of-bounds proportion.
#'
#' @param gaze fixation list from fixation report
#' @param screen_size size of the screen in pixels. Defaults to c(1024, 768)
#' @export
#' @return data frame containing gaze diagnostics
get_gaze_diagnostics <- function(gaze, screen_size=c(1024, 768)){
  #measure calibration goodness by computing:
  #	proportion non-fixation time
  #	proportion of off-screen fixations
  temp <- gaze %>% dplyr::group_by(Subject, Target) %>%
    dplyr::summarise(fixTime = sum(CURRENT_FIX_DURATION),
              totalTime = max(CURRENT_FIX_END))

  nonFixTime <- temp %>% dplyr::group_by(Subject) %>%
    dplyr::summarise(nonFixTime = mean(1 - (fixTime/totalTime)))

  g2 <- data.frame(gaze, screen_width=screen_size[1], screen_height=screen_size[2])
  oob_prop <- g2 %>% dplyr::group_by(Subject) %>%
    dplyr::summarise(oob_prop = sum(CURRENT_FIX_DURATION[(CURRENT_FIX_X > screen_width | CURRENT_FIX_X < 0 |
                                                     CURRENT_FIX_Y > screen_height | CURRENT_FIX_Y < 0)]) /
                sum(CURRENT_FIX_DURATION))
  calibGoodness <- merge(nonFixTime, oob_prop)
  #make a scatterplot of calibration diagnostics
  calib_plot <- ggplot(calibGoodness, aes(nonFixTime, oob_prop, label=Subject)) +
    geom_label() + labs(x="Non-Fixation Time", y="Out of Bounds Proportion", title="Gaze Diagnostics") + theme_bw() + 
    theme(axis.title.y=element_text(size = 12, face="bold"), axis.title.x = element_text(size=12, face="bold"), axis.text.x=element_text(size = 12, face="bold"), axis.text.y=element_text(size=12, face="bold"), legend.position = "bottom")
  print(calib_plot)
  #return calibration diagnostics
  return(calibGoodness)
}

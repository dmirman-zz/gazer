#' Read EyeDataViewer fixation report
#' 
#' Reads in a fixation report created by EyeLink's EyeDataViewer application
#' 
#' @param filename string containing full filename of fixation report
#' @param screen_size size of the screen in pixels. Used to compute track quality diagnostics. Defaults to c(1024, 768)
#' @param name_changes set of variable name pairs for improving variable names. Should be formatted as named character vector with new names as values and old names as names; see plyr::rename()
#' @param plot_fix_scatter logical indicating whether a scatterplot of fixations should be created
#' @return gaze data frame containing fixation report
readFixationReport <- function(filename, screen_size=c(1024, 768), 
                               name_changes=c(RECORDING_SESSION_LABEL="Subject",
                                              StimSlide.ACC="ACC", StimSlide.RT="RT",
                                              CorrectPort="TargetLoc"),
                               plot_fix_scatter=TRUE) {  
  gaze <- read.table(filename, header=TRUE,sep="\t", na.strings=".")
  #simplify some other column names
  gaze <- rename(gaze, name_changes)
  gaze$Subject <- as.factor(gaze$Subject)
  
  #convert CorrectPort into TargetLocation by taking just the integer at the end of the line
  gaze$TargetLocation <- as.factor(substr(gaze$TargetLoc, 6, 6))
  
  if(plot_fix_scatter){
    #measure calibration goodness by computing: 
    #	proportion non-fixation time
    #	proportion of off-screen fixations
    temp <- ddply(gaze, .(Subject, Target), summarize, 
                  fixTime = sum(CURRENT_FIX_DURATION),
                  totalTime = max(CURRENT_FIX_END))
    nonFixTime <- ddply(temp, .(Subject), summarize, 
                           nonFixTime = mean(1 - (fixTime/totalTime)))
    g2 <- data.frame(gaze, screen_width=screen_size[1], screen_height=screen_size[2])
    oob_prop <- ddply(g2, .(Subject), summarize, 
          oob_prop = sum(CURRENT_FIX_DURATION[(CURRENT_FIX_X > screen_width |
                                                 CURRENT_FIX_X < 0 | 
                                                 CURRENT_FIX_Y > screen_height |
                                                 CURRENT_FIX_Y < 0)]) /
                      sum(CURRENT_FIX_DURATION))
    calibGoodness <- merge(nonFixTime, oob_prop)
    #make a scatterplot of fixations
    g3 <- merge(g2, calibGoodness)
    #make a strip text label containing subject ID and calibration diagnostics
    g3$sText <- with(g3, paste(Subject, ": NonFix=", signif(nonFixTime, 2), 
                               ", OOB=", signif(oob_prop, 2), sep=""))
    #make the scatterplots
    sp <- ggplot(g3, aes(CURRENT_FIX_X, CURRENT_FIX_Y, 
                         size=sqrt(CURRENT_FIX_DURATION))) + 
      facet_wrap(~ sText, scales="free") + geom_point(alpha=0.5) + 
      annotate("rect", xmin=0, ymin=0, 
               xmax=screenSize[1], ymax=screenSize[2], color="red")    
    #show them
    print(sp)
  }
  
  return(gaze)
}

#' Take EDF files and return data that is in format usable for gazeR
#' uses saccades package to get blinks
#' merges the samples data and message data
#' puts time in ms
#' adds subject variable column
#' cleans up the column names
#' creates pupil column that is ambigious as to whether you sampled from left eye, right eye, or both (takes the average)
#'@import tidyverse
#'@import data.table
#'@import edfR
#'@import saccades
#'
#
#' @param file_list directory to edf files
#' @param output.dir directory to save new cleaned files
#' @export

parse_pupil_edf <- function (file_list, output.dir) {
  
  library(edfR)#use edfR to read in the edf files
  library(saccades)
  
    subs <- length(file_list)
    
    for (sub in 1:subs) { 
      
     #samps_all <- edf.trials(file_list[sub], samples=TRUE)
     samps_all <- edf.batch(file_list[sub], samples = TRUE)
     
     msg<-samps_all[[1]][["messages"]]
     
     msg<- msg %>%
       dplyr::rename(trial="eyetrial", time="sttime")

     samp <- samps_all[[1]][["samples"]]
     
     samp <- samp %>%
       rowwise() %>%
       dplyr::mutate(pup=mean(c(paL,paR),na.rm=TRUE), x=mean(c(gxL,gxR),na.rm=TRUE), y=mean(c(gyL, gyR),na.rm=TRUE)) %>%
       dplyr::rename(trial="eyetrial")
     
     samp1<-select(samp, time, ID, trial, x, y,pup)
     
     blk <- saccades::detect.fixations(samp1) # get blinks using the saccades alg
     
     blk <- blk %>%
       filter(event=="blink") %>%
       gather(startend, time, start:end) # gather all the blinks
     
     blk_merge<- merge(samp1, blk, by=c("trial", "time", "x", "y"), all=TRUE)
     
     blinks <- blk_merge %>% 
       group_by(grp = cumsum(!is.na(startend))) %>% 
       mutate(Label = replace(startend, first(startend) == 'start', 'start')) %>% #extends the start message forward until end message
       ungroup() %>% 
       # label blinks as 1 
       dplyr::select(ID, trial, time, x, y, pup, Label, -grp)
    
       #blinks$blink[is.na(blinks$blink)] <- 0 
     
     
     dat_samp_msg <- merge(blinks, msg, by=c("ID", "time", "trial"), all=TRUE)
     
     dat_samp_msg1 <- dat_samp_msg %>% 
       dplyr::mutate(blink=ifelse(!is.na(Label), 1, 0), pupil=ifelse(blink==1, NA, pup))%>%
       #plyr::filter(eyetrial!="NA")%>% #non-trial valu
       rowwise() %>% 
       #dplyr::mutate(pupil=mean(c(paL,paR),na.rm=TRUE), gazex=mean(c(gxL,gxR),na.rm=TRUE), gazey=mean(c(gyL, gyR),na.rm=TRUE)) %>% # get if recorded from left return left if right right if both average averagepupil
       dplyr::rename(subject="ID") %>% 
       dplyr::select(subject, time, trial, pupil, x, y, trial, blink, message, -Label) %>%
       ungroup() %>%
       dplyr::group_by(trial) %>%
       dplyr::mutate(time=time-time[1])
       
      dat_samp_msg1$message <- gsub("[^a-zA-Z]", "", dat_samp_msg$message)
     
     #edf file has diff number before each message which makes it diff to align events
     dat_samp_msg1 <- dat_samp_msg1 %>%
       group_by(trial) %>%
       distinct(time, .keep_all = TRUE)
     
      setwd(output.dir) 
      subOutData <- paste(file_list[sub], "_raw1.csv", sep="") # save file 
      write.table(dat_samp_msg1, file = subOutData, append = FALSE, sep = ",",
      row.names = FALSE, col.names = TRUE)
    }
}

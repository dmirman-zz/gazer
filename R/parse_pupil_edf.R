#' Take EDF files and return data that is in format usable for gazeR
#' merges the samples data and message data
#' puts time in ms
#' adds subject variable column
#' cleans up the column names
#' creates pupil column that is ambigious to whether you sampled from left eye, right eye, or both (takes the average)
#'@import tidyverse
#'@import data.table
#'@import edfR
#'
#
#' @param file_list directory to edf files
#' @param output.dir directory to save new cleaned files
#' @export

parse_pupil_edf <- function (file_list, output.dir) {
  
  library(edfR)#use edfR to read in the edf files
  
    subs <- length(file_list)
    
    for (sub in 1:subs) { 
      
     #samps_all <- edf.trials(file_list[sub], samples=TRUE)
     samps_all <- edf.batch(file_list[sub], samples = TRUE)
     msg<-samps_all[[1]][["messages"]]
     #msg<-edf.messages.c(file_list[subs])
     msg<-dplyr::rename(msg, time="sttime")
     samp <- samps_all[[1]][["samples"]]
     dat_samp_msg <- merge(samp, msg, by=c("ID", "time", "eyetrial"), all=TRUE)
     
     dat_samp_msg1 <- dat_samp_msg %>% 
       #plyr::filter(eyetrial!="NA")%>% #non-trial valu
       rowwise() %>% 
       dplyr::mutate(pupil=mean(c(paL,paR),na.rm=TRUE), gazex=mean(c(gxL,gxR),na.rm=TRUE), gazey=mean(c(gyL, gyR),na.rm=TRUE)) %>% # get if recorded from left return left if right right if both average averagepupil
       dplyr::rename(trial="eyetrial", subject="ID") %>% 
       dplyr::select(subject, time, trial, pupil, gazex, gazey,trial, message) %>%
       ungroup() %>%
       dplyr::group_by(trial) %>%
       dplyr::mutate(time=time-time[1])
      
      dat_samp_msg1$message <- gsub("[^a-zA-Z]", "", dat_samp_msg$message) #edf file has diff number before each message which makes it diff to align events
     
      setwd(output.dir) 
      subOutData <- paste(file_list[sub], "_raw1.csv", sep="") # save file 
      write.table(dat_samp_msg1, file = subOutData, append = FALSE, sep = ",",
      row.names = FALSE, col.names = TRUE)
    }
}

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
  
     msg<-edf.messages(file_list[sub])
     msg$subj <- paste(file_list[sub])
     samp<-edf.samples(file_list[sub], trials = TRUE)
     samp$subj <- paste(file_list[sub])

     dat_samp_msg <- merge(samp, msg, all=TRUE)
     
     dat_samp_msg1 <- dat_samp_msg %>% 
       dplyr::filter(eyetrial!="NA")%>% #non-trial values
       dplyr::mutate(time=time-time[1], subject=str_replace(subj, pattern="/Users/gellr/Desktop/TL_EDF/", replacement = "")) %>% #time in ms 
       rowwise() %>% 
       dplyr::mutate(pupil=mean(c(paL,paR),na.rm=TRUE), gazex=mean(c(gxL,gxR),na.rm=TRUE), gazey=mean(c(gyL, gyR),na.rm=TRUE)) %>% # get if recorded from left return left if right right if both average averagepupil
       dplyr::rename(trial="eyetrial") %>% 
       dplyr::select(subject, time, trial, pupil, gazex, gazey, trial, msg)

      setwd(output.dir) 
      subOutData <- paste(file_list[sub], "_raw.csv", sep="") # save file 
      write.table(dat_samp_msg1, file = subOutData, append = FALSE, sep = ",",
      row.names = FALSE, col.names = TRUE)
    }
}


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
#' @param type include whether you want to parse edf pupil data or vwp (fixations)
#' @export

parse_edf <- function (file_list, output.dir, type="", hz=NA) {
  
  library(edfR)#use edfR to read in the edf files
  
  if (type=="samp") {
    subs <- length(file_list)
    
    for (sub in 1:subs) { 
      
      subject = basename(file_list[sub])
     #samps_all <- edf.trials(file_list[sub], samples=TRUE)
     samps_all <- edf.batch(file_list[sub], samples = TRUE, do.plot=FALSE)
     
     msg<-samps_all[[1]][["messages"]]
     
     msg<- msg %>%
       dplyr::rename(trial="eyetrial", time="sttime")
     
     msg$subject<-subject

     samp <- samps_all[[1]][["samples"]]
     
     samp <- samp %>%
       rowwise() %>%
       dplyr::mutate(pup=mean(c(paL,paR),na.rm=TRUE), x=round(mean(c(gxL,gxR),na.rm=TRUE)), y=round(mean(c(gyL, gyR),na.rm=TRUE))) %>%
       dplyr::rename(trial="eyetrial")
     
     samp$subject<-subject
     samp$row_index<-as.numeric(row.names(samp))
     
     samp1<-select(samp, time, subject, trial, x, y,pup, row_index)
     
     df_blinks <- as.data.frame(samp1) %>%
       # pass into a list
       dplyr::summarise(row_index= list(based_noise_blinks_detection(as.matrix(samp1$pup), sampling_rate_in_hz=250))) %>%
       unnest()
     
     df_blinks$Label <- rep(c("Onset", "Offset"), length.out=nrow(df_blinks))
     
     df_blinks_merge<-merge(samp1, df_blinks, by="row_index", all.x=TRUE)
     
     blinks <- df_blinks_merge %>% 
       group_by(grp = cumsum(!is.na(Label))) %>% 
       mutate(Label = replace(Label, first(Label) == 'Onset', 'Onset')) %>% #extends the start message forward until end message
       ungroup() %>% 
       # label blinks as 1 
       dplyr::select(subject, trial, time, x, y, pup, Label, -grp)
    
     #blk <- saccades::detect.fixations(samp1) # get blinks using the saccades alg
     
     #blk <- blk %>%
     #  filter(event=="blink") %>%
     #  gather(startend, time, start:end) # gather all the blinks
     
     #blk_merge<- merge(samp1, blinks, by=c("trial", "time", "x", "y"), all=TRUE)
     
     #blinks <- blk_merge %>% 
     #  group_by(grp = cumsum(!is.na(startend))) %>% 
     #  mutate(Label = replace(startend, first(startend) == 'start', 'start')) %>% #extends the start message forward until end message
     #  ungroup() %>% 
       # label blinks as 1 
     #  dplyr::select(subject, trial, time, x, y, pup, Label, -grp)
    
       #blinks$blink[is.na(blinks$blink)] <- 0 
     
     
     dat_samp_msg <- merge(blinks, msg, by=c("subject", "time", "trial"), all=TRUE)
     
     dat_samp_msg1 <- dat_samp_msg %>% 
       dplyr::mutate(blink=ifelse(!is.na(Label), 1, 0), pupil=ifelse(blink==1 | pup==0, NA, pup))%>%
       #plyr::filter(eyetrial!="NA")%>% #non-trial valu
       rowwise() %>% 
       #dplyr::mutate(pupil=mean(c(paL,paR),na.rm=TRUE), gazex=mean(c(gxL,gxR),na.rm=TRUE), gazey=mean(c(gyL, gyR),na.rm=TRUE)) %>% # get if recorded from left return left if right right if both average averagepupil
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
 
    if (type=="fix") { 
      
      subs <- length(file_list)
      
      for (sub in 1:subs) { 
        
        subject = basename(file_list[sub])
        #samps_all <- edf.trials(file_list[sub], samples=TRUE)
        samps_all <- edf.batch(file_list[sub], samples = TRUE, do.plot=FALSE)
        
        samp <- samps_all[[1]][["samples"]]
        
        df_samp = samp %>% group_by(eyetrial) %>% mutate(timestart=time[1]) # start of time
        
        df1 <-dplyr::select(df_samp, eyetrial, timestart) %>%
          distinct(eyetrial, .keep_all = TRUE) # get df of start times per trial
        
        samp_fix <- samp[[1]][["fixations"]]
        
        samp_fix_merge <- merge(samp_fix, df1, by="eyetrial", by.x=TRUE)
        
        st_end_fix <- samp_fix_merge %>% 
          dplyr::group_by(eyetrial) %>% 
          dplyr::mutate(CURRENT_FIX_START=timestart-sttime, CURRENT_FIX_END=timestart-entime) %>%
          dplyr::rename(CURRENT_FIX_Y="gavx", CURRENT_FIX_X="gavy") %>% 
          ungroup()%>%
          dplyr::muatte(subject=subject, CURRENT_FIX_DURATION= CURRENT_FIX_END-CURRENT_FIX_START) %>%
          dplyr::rename(trial="eyetrial")
        
                                                            
        setwd(output.dir) 
        subOutData <- paste(file_list[sub], "_raw_vwp.csv", sep="") # save file 
        write.table(st_end_fix, file = subOutData, append = FALSE, sep = ",",
                    row.names = FALSE, col.names = TRUE)
        
      }
    
    }
}
    
  
   
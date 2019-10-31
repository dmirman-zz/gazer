#' Take EDF files and return data that is in format usable for gazeR
#' for pupil merges the samples data
#' puts time in ms
#' adds subject variable column
#' cleans up the column names
#' creates pupil column that is ambigious as to whether you sampled from left eye, right eye, or both (takes the average)
#' For fixation data, obtains sample report, puts time in ms
#'@import tidyverse
#'@import data.table
#'@import edfR
#' @param file_list directory to edf files
#' @param output.dir directory to save new cleaned files
#' @param type include whether you want to parse edf pupil data (pupil) or vwp (fixations)
#' @export

parse_edf <- function (file_list, output.dir, type="pupil") {
  
  library(edfR)#use edfR to read in the edf files
  library(saccades)
  library(data.table)
  
  if (type=="pupil") {
    subs <- length(file_list)
    
    for (sub in 1:subs) { 
      
      subject = basename(file_list[sub])
      #samps_all <- edf.trials(file_list[sub], samples=TRUE)
      samps_all <- edf.batch(file_list[sub], samples = TRUE, do.plot=FALSE)
      
      msg<-samps_all[[1]][["messages"]]
      
      msg<- msg %>%
       dplyr::rename(trial="eyetrial", time="sttime") %>%
       distinct(time, .keep_all = TRUE)
      
      # msg$subject<-subject
      
      samp <- samps_all[[1]][["samples"]]
      
      samp <- samp %>%
        rowwise() %>%
        dplyr::mutate(pup=mean(c(paL,paR),na.rm=TRUE), x=mean(c(gxL,gxR),na.rm=TRUE), y=mean(c(gyL, gyR),na.rm=TRUE)) %>%
        dplyr::rename(trial="eyetrial") %>%
        dplyr::select(-blink, -fixation, -saccade) %>%
        dplyr::ungroup()
      
      #samp$subject<-subject
      #samp$row_index<-as.numeric(row.names(samp))
      # use data.table to merge messages to neartest time point in orginal df. in the edf files messages appear during non-sampled times making a lot of NAs. 
      #samp1<-select(samp, time, subject, trial, x, y,pup, row_index)
      
      #df_blinks <- as.data.frame(samp1) %>%
      # pass into a list
      # dplyr::summarise(row_index= list(based_noise_blinks_detection(as.matrix(samp1$pup), sampling_rate_in_hz=250))) %>%
      # unnest()
      
      #df_blinks$Label <- rep(c("Onset", "Offset"), length.out=nrow(df_blinks))
      
      #df_blinks_merge<-merge(samp1, df_blinks, by="row_index", all.x=TRUE)
      
      # blinks <- df_blinks_merge %>% 
      #  group_by(grp = cumsum(!is.na(Label))) %>% 
      #  mutate(Label = replace(Label, first(Label) == 'Onset', 'Onset')) %>% #extends the start message forward until end message
      #  ungroup() %>% 
      # label blinks as 1 
      #  dplyr::select(subject, trial, time, x, y, pup, Label, -grp)
      
      blinks_merge <- blink_detect(samp)
      
      blinks <- blinks_merge %>% 
        dplyr::group_by(grp = cumsum(!is.na(startend))) %>% 
        dplyr::mutate(Label = replace(startend, first(startend) == 'start', 'start')) %>% #extends the start message forward until end message
        dplyr::ungroup() %>% 
        # label blinks as 1 
        dplyr::select(trial, time, x, y, pup, Label, -grp)
      
      #blinks$blink[is.na(blinks$blink)] <- 0 
      #samp <- as.data.table(blinks) #turn into df 
      
     # msg <- as.data.table(msg) #turn into df
      
      #setkey(samp1, ID, trial, time)# merge on values
      
      #setkey(msg, ID, trial, time)# merge on values
      
      #dat_samp_msg<- msg[blinks, roll='nearest'] # merge the two dfs to nearest timepoint
  
      dat_samp_msg <- merge(blinks, msg, by=c("time", "trial"), all=TRUE)
      
      dat_samp_msg <- dat_samp_msg %>%
        dplyr::mutate(pup=zoo::na.locf(pup, fromLast=TRUE)) # sample messages are sometimes recorded outside sampling rate so we need to fill in those NAs with available data
      
      dat_samp_msg1 <- dat_samp_msg %>% 
        dplyr::mutate(blink=ifelse(!is.na(Label), 1, 0), pupil=ifelse(blink==1 | pup==0, NA, pup))%>%
        dplyr::filter(trial!="NA") %>% # get rid non-trial values 
        dplyr::mutate(message=str_replace_all(message, pattern="[^a-zA-Z]", replacement = "")) %>%
        # rowwise() %>% 
        #dplyr::mutate(pupil=mean(c(paL,paR),na.rm=TRUE), gazex=mean(c(gxL,gxR),na.rm=TRUE), gazey=mean(c(gyL, gyR),na.rm=TRUE)) %>% # get if recorded from left return left if right right if both average averagepupil
        # ungroup() %>%
        dplyr::group_by(trial) %>%
        dplyr::mutate(time=time-time[1], subject=subject)%>%
        dplyr::ungroup()%>%
        dplyr::select(subject, time, trial, pupil, x, y, trial, blink, message, -Label)
        
      
      #edf file has diff number before each message which makes it diff to align events
      #dat_samp_msg1 <- dat_samp_msg1 %>%
      #  group_by(trial) %>%
      #  distinct(time, .keep_all = TRUE)
      
      setwd(output.dir) 
      subOutData <- paste(file_list[sub], "_raw1.csv", sep="") # save file 
      write.table(dat_samp_msg1, file = subOutData, append = FALSE, sep = ",",
                  row.names = FALSE, col.names = TRUE)
    }
  }
  
  if (type=="vwp") { 
    
    subs <- length(file_list)
    
    for (sub in 1:subs) { 
      
      subject = basename(file_list[sub])
      #samps_all <- edf.trials(file_list[sub], samples=TRUE)
      samps_all <- edf.batch(file_list[sub], samples = TRUE, do.plot=FALSE)
      
      samp <- samps_all[[1]][["samples"]]
      
      
      df_samp = samp %>% group_by(eyetrial) %>% 
        dplyr::mutate(time=time-time[1]) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(pupil=mean(c(paL,paR),na.rm=TRUE), x=mean(c(gxL,gxR),na.rm=TRUE), y=mean(c(gyL, gyR),na.rm=TRUE)) %>% 
        dplyr::select(-blink, -fixation, -saccade, -gxL, -gyL, -gxR, -gyR, -paR, -paL) %>%
        dplyr::rename(trial="eyetrial")
      
      # start of time
      
      #df1 <-dplyr::select(df_samp, eyetrial, timestart) %>%
      #distinct(eyetrial, .keep_all = TRUE) # get df of start times per trial
      
      #samp_fix <- samps_all[[1]][["fixations"]]
      
      #samp_fix_merge <- merge(samp_fix, df1, by="eyetrial", all=TRUE)
      
      #st_end_fix <- samp_fix_merge %>% 
      #  dplyr::group_by(eyetrial) %>% 
      #  dplyr::mutate(CURRENT_FIX_START=sttime-timestart, CURRENT_FIX_END=entime-timestart) %>%
      #  dplyr::rename(CURRENT_FIX_Y="gavx", CURRENT_FIX_X="gavy") %>% 
      #  ungroup()%>%
      #  dplyr::mutate(subject=subject, CURRENT_FIX_DURATION=abs(CURRENT_FIX_END-CURRENT_FIX_START)) %>%
      #  dplyr::rename(trial="eyetrial")%>%
      #  dplyr::select(-ID, -sttime, -entime)
      
      
      setwd(output.dir) 
      subOutData <- paste(file_list[sub], "_raw_vwp.csv", sep="") # save file 
      write.table(df_samp, file = subOutData, append = FALSE, sep = ",",
                  row.names = FALSE, col.names = TRUE)
      
    }
    
  }
}


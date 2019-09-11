#' Take EDF files and return message events to extract behavioral data
#' creates pupil column that is ambigious to whether you sampled from left eye, right eye, or both (takes the average)
#'@import tidyverse
#'@import data.table
#'@import edfR
#'
#
# Collect trial variables for your experiment. 

#' @param file_list directory to edf files
#' @param output.dir directory to save new cleaned files
#' @export
#' 
#' 
#' 
parse_edf_messages <- function (file_list, output.dir) {
  
  library(edfR)#use edfR to read in the edf files
  
  subs <- length(file_list)
  
  for (sub in 1:subs) { 
    
    event_messages = edf.messages.c(file_list[sub]) # get trial ID messages
    subject= basename(file_list[sub]) # get id from file
    event_messages$subject <- subject # add id so it is easier to extract events per subject
    
    setwd(output.dir) 
    subOutData <- paste(file_list[sub], "_messages.csv", sep="") # save file 
    write.table(event_messages, file = subOutData, append = FALSE, sep = ",",
                row.names = FALSE, col.names = TRUE)
  }
}


trial <- f$msg %>%
  subset(str_detect(string=., (pattern="TRIALID"))) %>%
    str_replace(pattern="TRIALID", replacement = "") %>%
    str_replace_all(pattern=" ", repl="") # get rid of white space

script <- f$msg %>%
  subset(str_detect(string=., (pattern="script"))) %>%
  str_replace(pattern="!V TRIAL_VAR script", replacement = "") %>%
  str_replace_all(pattern=" ", repl="")


item <- f$msg %>%
  subset(str_detect(string=., (pattern="item"))) %>%
  str_replace(pattern="!V TRIAL_VAR item", replacement = "") %>%
  str_replace_all(pattern=" ", repl="")

rt <- f$msg %>%
  subset(str_detect(string=., (pattern="TRIAL_VAR RT"))) %>%
  str_replace(pattern="!V TRIAL_VAR RT", replacement = "") %>%
  str_replace_all(pattern=" ", repl="")

acc <- f$msg %>%
  subset(str_detect(string=., (pattern="ACCURACY"))) %>%
  str_replace(pattern="!V TRIAL_VAR ACCURACY", replacement = "") %>%
  str_replace_all(pattern=" ", repl="")

alt <- f$msg %>%
  subset(str_detect(string=., (pattern="alteration"))) %>%
  str_replace(pattern="!V TRIAL_VAR alteration", replacement = "") %>%
  str_replace_all(pattern=" ", repl="")

block <- f$msg %>%
  subset(str_detect(string=., (pattern="block"))) %>%
  str_replace(pattern="!V TRIAL_VAR block", replacement = "") %>%
  str_replace_all(pattern=" ", repl="")


behav <- data.frame(trial=as.numeric(trial), acc=as.numeric(acc), block=as.numeric(block), rt=as.numeric(rt), item=as.factor(item), script=as.factor(tolower(script)), alt=as.factor(alt))

behav <- behav %>% 
  mutate(trial=trial + 1) # edf does not start trial at 1 

#merge behav into the big df


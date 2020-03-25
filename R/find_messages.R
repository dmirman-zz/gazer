#' Take EDF files and return a tiblle with important behavioral events
#'@param file_list list of edf files
#'@param varnames a vector of variable names from experiment c("TRIALID", "ACCURACY")
#'@param patterns a vector of patters to look for ("TRIALID", "!V TRIAL_VAR script")
#'@import tidyverse
#'@import data.table
#'@import edfR
#
#
# Collect trial variables for your experiment.

find_messages_edf <- function(file_list,varnames,patterns, output_dir)
{
  subs <- length(file_list)

  for (sub in 1:subs) {
    subject = basename(file_list[sub]) # get id from file
    messages_all <- edf.batch(file_list[sub], samples = TRUE, do.plot=FALSE)

    msg<-samps_all[[1]][["samples"]]

    messagelist = list()

    for(i in 1:length(varnames)){

      find_msg <- msg$message %>%
        subset(str_detect(string=., pattern=varnames[i])) %>% # find specific pattern
        str_replace(pattern=patterns[i], replacement = "") %>% # replace pattern with white space
        str_replace_all(pattern=" ", repl="") # get rid of white space
        messagelist[[i]]<-find_msg
    }


    message_data=dplyr::bind_cols(messagelist) %>%
    set_names(varnames) %>%
    dplyr::mutate(subject=subject) %>%
    dplyr::rename(trial = "TRIALID")%>%
    dplyr::mutate(trial=ifelse(trial==0, trial+1, trial))


    setwd(output_dir)
    subOutData <- paste(file_list[sub], "behave_data.csv", sep="") # save file

    write.table(message_data, file = subOutData, append = FALSE, sep = ",",
              row.names = FALSE, col.names = TRUE)
  }
}





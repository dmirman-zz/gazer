
behave_pupil=function(datafile, omiterrors=TRUE, behave_colnames=NULL){
  #culls the important behavioral data from the sample report
  #returns RTs without errors for RT analysis or dataset with all error data for error analysis
  #datachange<-subset(datafile,select=datafile[,behave_colnames])#change depending on column variable names
  datafull <- unique(datafile[, behave_colnames]) #drop duplicated rows (multiple rows per subject) and only use columns of interest
  # remove errors if the user has asked for it
  #renames variables in datafile
  if(omiterrors == TRUE){
    omitnadata <- subset(datafull, datafull$acc == 1)
  } else {
    omitnadata <- datafull
  }
  return(omitnadata)
}
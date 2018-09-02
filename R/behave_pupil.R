#'culls  important behavioral data from the sample report
#'@param datafile raw pupil data
#'@param omiterrors if ture returns RTs without errors for RT analysis or dataset with all error data for error analysis
#'@param behave_colnames specify variables of interest 
#'@return data frame with or withour errors 
#'@export
behave_pupil=function(datafile, omiterrors=TRUE, behave_colnames=NULL){
  datafull <- unique(datafile[, behave_colnames]) 
  if(omiterrors == TRUE){
    omitnadata <- subset(datafull, datafull$acc == 1)
  } else {
    omitnadata <- datafull
  }
  return(omitnadata)
}
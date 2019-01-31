#'calculates the median absoulte deviation (MAD) and sets the threshold to filter on.
#' @param max_dilation pupil file returned from speed_pupil
#' @param n constant used 
#' @return threhold value to filter on
#' @export
calc_mad<-function(max_dilation,n=16){
  med_d=median(max_dilation)
  mad=median(abs(max_dilation-med_d))
  thres= med_d+(n*mad)
  return(thres)
}

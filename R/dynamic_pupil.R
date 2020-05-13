#' calc dynamic pupil range (Ayasse et al., 2017; Piquado et al., 2010))
#' must have min pupil and max pupil size merged into df to work
#'
#' @param pupil_col pupil col
#' @param dmin mean pupil size during the ‘bright’ condition (i.e., minimum pupil size)
#' @param dmax mean pupil size during the ‘dark’ condition (i.e., max pupil size)
#' @return return dynamic pupil size scaled to P
#' @export


dynamic_pupil <- function(pupil_col, dmin=NA, dmax=NA) {

  dynamic_pupil <- (pupil_col-dmin) / (dmax-dmin)*100

  return(dynamic_pupil)
}

#' Crawford-Howell (1998) t-test for case-control comparisons.
#'
#' Computes Crawford-Howell t-test comparing a single case to a set of controls.
#'
#' @param case single case score
#' @param control vector of control scores
#' @return Returns a data frame with three values: t, df, and p.
#' @export
#' @examples
#' Crawford_Howell(649, c(243, 175, 231, 164, 236, 131, 302, 303, 363, 287, 205, 297, 144, 789, 210, 245))
Crawford_Howell <- function(case, control){
  tval <- (case - mean(control)) / (sd(control)*sqrt((length(control)+1) / length(control)))
  degfree <- length(control)-1
  pval <- 2*(1-pt(abs(tval), df=degfree)) #two-tailed p-value
  result <- data.frame(t = tval, df = degfree, p=pval)
  return(result)
}
#In the future: add modeling of covariates (Crawford et al., 2011)? Might require Bayesian stats, which would be too much for this context.

#' Get random effects for individual effect size quantification
#'
#' Takes a model and name of a random effect. Returns a data frame containing those random effect estimates arranged and ready for quantifying individual effect sizes.
#'
#' @param model (g)lmerMod object
#' @param ranef_name string name of random effect (right of pipe in (g)lmer call)
#' @return Returns a data frame containing the random effect estimates
#' @export
#' @examples
#' re <- get_ranef(m.FT, "Subject:Object:Condition") #using FunctTheme example model
get_ranef <- function(model=NULL, ranef_name=NULL){
  if(is.null(model) | is.null(ranef_name)){
    stop("Both model and ranef_name need to be specified.")
  }
  #get random effect
  re <- ranef(model)[[ranef_name]]
  #parse rownames
  cn <- unlist(strsplit(ranef_name, ":")) #new variable names based on components of random effect name
  rn <- colsplit(rownames(re), ":", cn) #get rownames, split them at ":", give them those variable names
  #combine parsed rownames and random effect values
  result <- cbind(rn, re)
  #if necessary, remove annoying parens from variable name
  if(any(names(result)=="(Intercept)")){
    result <- rename(result, c("(Intercept)" = "Intercept"))
  }

  return(result)
}

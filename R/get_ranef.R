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
  # get the named random effects and add info from rownames
  re <- ranef(model)[[ranef_name]] %>%
    mutate(rn = rownames(.)) %>%
    separate(rn, unlist(strsplit(ranef_name, ":")), sep = ":", remove=TRUE)

  #if necessary, remove annoying parens from variable name
  if(any(names(re)=="(Intercept)")){
    names(re) <- replace(names(re), names(re)=="(Intercept)", "Intercept")
  }

  return(re)
}

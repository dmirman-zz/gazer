#' Get p-values for a multilevel regression model
#' 
#' Takes a model fit with lmer and (optionally) an estimation method, returns a 
#' data frame containing fixed effect coefficient estimates, SE, t-values, and 
#' p-values. Can use normal approximation, Kenward-Roger approximation for 
#' degrees of freedom, or both. If Kenward-Roger approximation is used, result 
#' also contains approximated degrees of freedom.
#' 
#' @param model a merMod model object fit by lmer
#' @param method p-value estimation method, defaults to "normal", can also be
#'   "KR" or "all"
#' @return data frame containing fixed effect estimates, SE, t-values, (degrees 
#'   of freedom if appropriate) and p-values.
#' @importFrom pbkrtest get_Lb_ddf
#' @export
#' @examples
#' model <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#' get_pvalues(model)
#' # Not working with lme4 1.8?
#' # get_pvalues(m.full, "KR")
get_pvalues <- function(model, method = "normal") {
  #extract fixed effect coefficients from model
  coefs <- data.frame(coef(summary(model)))
  switch(method,
         # normal approximation
         normal = {coefs$p.normal <- get_pvalues_normal(coefs$t.value)},
         # K-R
         KR = {coefs$df.KR <- get_Lb_ddf(model, fixef(model))
               coefs$p.KR <- get_pvalues_kr(coefs$t.value, coefs$df.KR)},
         # all
         all = {coefs$p.normal <- get_pvalues_normal(coefs$t.value)
                coefs$df.KR <- get_Lb_ddf(model, fixef(model))
                coefs$p.KR <- get_pvalues_kr(coefs$t.value, coefs$df.KR)},
         {warning("Invalid p-value estimation method. Use 'normal', 'KR'", 
                  "or 'all'. Defaulting to normal approximation.")
          coefs$p.normal <- get_pvalues_normal(coefs$t.value)})
  coefs
}

get_pvalues_normal <- function(ts) {
  2 * (1 - pnorm(abs(ts)))
}

get_pvalues_kr <- function(ts, dfs) {
  2 * (1 - pt(abs(ts), dfs))
}

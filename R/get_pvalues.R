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
#' require(lme4)
#' model <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#' get_pvalues(model)
#' # Not working with lme4 1.8?
#' # get_pvalues(m.full, "KR")
get_pvalues <- function(model, method = "normal") {
  # Extract fixed effect coefficients from model
  coefs <- data.frame(coef(summary(model)))

  # Check/warn if bad method name used
  supported <- c("normal", "KR", "all")
  if (!is.element(method, supported)) {
    warning("Invalid p-value estimation method. Use 'normal', 'KR'",
            "or 'all'. Defaulting to normal approximation.")
    method <- "normal"
  }

  # Apply normal approx if asked for
  if (method %in% c("normal", "all")) {
    coefs$p.normal <- get_pvalues_normal(coefs$t.value)
    coefs$p.normal.star <- p_star(coefs$p.normal)
  }

  # Apply KR if asked for
  if (method %in% c("KR", "all")) {
    coefs$df.KR <- get_Lb_ddf(model, fixef(model))
    coefs$p.KR <- get_pvalues_kr(coefs$t.value, coefs$df.KR)
  }

  coefs
}

get_pvalues_normal <- function(ts) {
  2 * (1 - pnorm(abs(ts)))
}

get_pvalues_kr <- function(ts, dfs) {
  2 * (1 - pt(abs(ts), dfs))
}

p_star <- function(pval){
  star <- ifelse(pval < 0.001, "***",
                 ifelse(pval < 0.01, "**",
                        ifelse(pval < 0.05, "*",
                               ifelse(pval < 0.1, ".", ""))))
  return(star)
}

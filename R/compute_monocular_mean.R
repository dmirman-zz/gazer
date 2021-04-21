# A "monocular mean" averages both eyes together. If data is available in just
# one eye, use the available value as the mean, unless we need_both is TRUE.
#' @param x1 pupil left
#' @param x2 pupil right
#' @return vector with monocular mean values
compute_monocular_mean <- function(x1, x2) {
  xm <- rowMeans(cbind(x1, x2), na.rm = TRUE)
  # NaN => NA
  ifelse(is.nan(xm), NA, xm)
}

#' Low-Pass Filter
#'
#' \code{low_pass_filter} implements a low-pass third-order Butterworth filter.
#'
#' This function serves as a wrapper around \code{\link[signal]{butter}} and
#' \code{\link[signal]{filter}} that combines filter construction and
#' implementation and allows the user to specify the filter in terms of the
#' original sampling frequency in Hz and the desired filter in Hz, without
#' having to specify the filter in terms of a fraction of the Nyquist filter.
#'
#' This function also simplifies the output of the call to
#' \code{\link[signal]{filter}} by converting it into a numerical vector,
#' which can then be used to replace the original time series in a data frame.
#'
#' @section Note:
#' The low-pass filter assumes that the signal should start from 0, and it will
#' always return a time series that starts from 0.
#'
#' @seealso \code{\link{artifacts}}, \code{\link{normalize}}
#'
#' @param ts A time series, passed as a vector of chronologically ordered
#' observations separated by equal intervals of time.
#'
#' @param samp_freq Sampling frequency in Hz.
#'
#' @param filter_freq Frequency of the low pass filter. The default value of
#' 4 Hz is recommended for pupil dilation.
#'
#' @return A low-pass-filtered copy of the time series.
#'
#' @export
#'

low_pass_filter<-function(ts, samp_freq, filter_freq=4){
  LPF<-signal::butter(3, 2 * filter_freq / samp_freq, "low")
  return(as.vector(signal::filter(LPF, ts)))
}

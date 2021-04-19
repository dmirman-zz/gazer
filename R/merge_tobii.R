#' Merges files taken from a X2-30 Tobii tracker created from a PsychoPy experiment using the Python package
#' Titta which interacts with the Tobii SDK.
#' Removes calibration and validation messages from main file
#' Puts time in milliseconds
#' Averages left and right pupil data
#' Creates a trial col --note you need to make sure your recording has trial messages
#' Changes colnames so it is ready to be used in gazeR
#'
#' @param file_list a vector of file names
#' @param part_colname name of your participant id
#' @param behave_colnames name of your behave colnames
#' @param type right now work with X230
#' @return DF
#' @export
#'
merge_tobii <- function (file_list, part_colname="PID", behave_colnames="cond", type="X230") {
  #file list is path to .xls files
  #vroom is faster
  library(data.table)

  dataset <- do.call("rbind", lapply(file_list, FUN=function(files){

    for (i in 1:length(files)){
      if(file.exists(files[i])){
        message( "now processing:", files[i])
      }
    }

    fread(files, header=TRUE, sep="\t", na.strings = ".", fill=TRUE)}))

  dataset<- dataset %>%
    dplyr::rename("subject" = part_colname) # rename sub col

  # for some reason titta uses mixed case for calibration and validation messages.
  dataset <- dataset %>%
    dplyr::group_by(subject) %>%
    dplyr::mutate(msg=tolower(msg))%>%
    # take out the calibration and validation data
    dplyr::filter(!str_detect(msg, 'calibration'), !str_detect(msg, 'validation')) %>%
    # select a few of the important columns
    dplyr::select(subject,system_time_stamp, device_time_stamp, msg, all_of(behave_colnames), right_pupil_diameter,left_pupil_diameter,
                  right_pupil_validity, left_pupil_validity) %>%

    dplyr::mutate(msgtrial=ifelse(str_detect(msg, "startfix"), str_replace_all(msg,"startfix_", ""), NA)) %>%
    ungroup() %>%

    dplyr::mutate(trial = zoo::na.locf(msgtrial)) %>%

    dplyr::group_by(trial)%>%

    dplyr::mutate(time= (device_time_stamp - device_time_stamp[1]) /1e+6 * 1000) %>%

    dplyr::ungroup() %>%

    dplyr::rowwise() %>%
    # need to create a monocular average pupil size
    dplyr::mutate(pupil=compute_monocular_mean(right_pupil_diameter, left_pupil_diameter, need_both = FALSE))

  get_msg <- dataset %>%

    dplyr::group_by(trial, msg) %>%

    top_n(n=1, wt=desc(device_time_stamp)) %>%

    select(subject, device_time_stamp, msg, trial) %>%

    rename("new_msg" = "msg")

  tobii_samp_msg <- dplyr::full_join(dataset, get_msg)

  tobii_samp_msg <- tobii_samp_msg %>%
    dplyr::rename("message" = "new_msg")

  return(tobii_samp_msg)

}

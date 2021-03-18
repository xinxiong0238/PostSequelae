#' Patient-level plot
#'
#' Create patient-level disease count plot over time
#'
#' @param PatientObersvations_pro Dataframe; \code{PatientObersvations_pro} output from \code{\link{main}}.
#' @param patient_num_id Integer; patient id.
#' @return A ggplot object.
#' @export
#' @importFrom magrittr %>%
patientlevelplot <- function(PatientObersvations_pro,
                              patient_num_id) {
  patient_num_id <- as.numeric(patient_num_id)
  patient.plot <-
    PatientObersvations_pro %>%
    dplyr::filter(patient_num %in% patient_num_id) %>%
    dplyr::select(Description, patient_num, timewindw, Description) %>%
    dplyr::group_by(patient_num, timewindw) %>%
    dplyr::summarise(code_count = dplyr::n_distinct(Description)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(PatientObersvations_pro %>%
               dplyr::filter(patient_num %in% patient_num_id) %>%
               dplyr::group_by(patient_num, timewindw, Description) %>%
               dplyr::summarise(code_count = dplyr::n_distinct(Description)),
             mapping = ggplot2::aes(x = timewindw, fill = Description)
    ) +
    ggplot2::aes(x = timewindw, y = code_count, group = 1) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::scale_fill_viridis_d()
  return(patient.plot)
}

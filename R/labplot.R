#' Density plot
#'
#' Create density plot for lab data
#'
#' @param PatientObersvations_pro Dataframe; \code{PatientObersvations_pro} output from \code{\link{main}}.
#' @param loinc_mapping Dataframe; connecting loinc codes to detailed description.
#' @return A ggplot object.
#' @export
#' @importFrom magrittr %>%
labplot <- function(PatientObersvations_pro, loinc_mapping,
                    windows.size, windows.min, windows.max){
  LabCodesUnique<-unique(PatientObersvations_pro[PatientObersvations_pro$concept_type=="LAB-LOINC",c("concept_code")])
  LabsGraphs<-list()
  PatientObersvations_pro$timewindw = NULL
  PatientObersvations_pro = add.twindows(PatientObersvations_pro,
                                         windows.size, windows.min, windows.max)
  for (i in 1:length(LabCodesUnique)){
    LOINCvalues<-PatientObersvations_pro[PatientObersvations_pro$concept_type=="LAB-LOINC" &
                                       PatientObersvations_pro$concept_code == LabCodesUnique[i] ,] %>%
      dplyr::group_by(patient_num, timewindw) %>%
      dplyr::summarise(MeanVal=mean(value))

    LabsGraphs[[i]] <- ggplot2::ggplot(LOINCvalues,
                                       ggplot2::aes(x = MeanVal, y = timewindw, fill = stat(x))) +
      ggridges::geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
      ggplot2::scale_fill_viridis_c(name = "MeanVal", option = "C") +
      ggplot2::theme(text = ggplot2::element_text(size=15)) +
      ggplot2::labs(title = paste("Mean",loinc_mapping[loinc_mapping$LOINC==LabCodesUnique[i], c("Name")],"values"))
  }
  names(LabsGraphs) = LabCodesUnique
  LabsGraphs[[i]]
  return(LabsGraphs)
}


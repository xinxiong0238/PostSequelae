#' @importFrom magrittr %>%
rollup <- function(PatientObservationsEnctrs,
                   PheCodes, loinc_mapping, digits = NULL) {
  PatientObservationsEnctrs$concept_code <-
    stringr::str_trim(PatientObservationsEnctrs$concept_code,
             side = c("both")
    )

  ## mapping LOINC
  PO_loinc <- PatientObservationsEnctrs[
    stringr::str_detect(PatientObservationsEnctrs$concept_type, "LOINC"),]
  PO_loinc <-
    merge(PO_loinc,
          loinc_mapping[,c("Name","LOINC")],
          by.x = c("concept_code"), by.y = ("LOINC"),
          all.x = TRUE
    )
  PO_loinc = PO_loinc[order(PO_loinc$patient_num),]
  PO_loinc = PO_loinc[!duplicated(PO_loinc),]
  PO_loinc = stats::na.omit(PO_loinc)

  ## mapping ICD
  PO_pheicd <- PatientObservationsEnctrs[
    stringr::str_detect(PatientObservationsEnctrs$concept_type, "ICD"),]
  PO_pheicd <- PO_pheicd[
    !stringr::str_detect(PO_pheicd$concept_code, "\\."),]

  if(is.null(digits)){
    digits = max(stringr::str_length(PO_pheicd$concept_code))
  }
  PheCodes$code_digits = stringr::str_trunc(PheCodes$code,
                                   digits,side = "right",ellipsis = "")
  PheCodes_digits = PheCodes[, c("code_digits", "Description", "Phecode")]
  PheCodes_digits = PheCodes_digits[!duplicated(PheCodes_digits),]

  # == changed this to ICD10 concept type from the i2b2 files
  PO_pheicd <-
    merge(PO_pheicd,
          PheCodes_digits,
          by.x = c("concept_code"), by.y = ("code_digits"),
          all.x = TRUE
    )
  PO_pheicd = PO_pheicd[order(PO_pheicd$patient_num),]
  PO_pheicd = PO_pheicd[!duplicated(PO_pheicd),]
  # bb = unique(PO_pheicd$concept_code[is.na(PO_pheicd$Description)])
  # b.len = length(PO_pheicd$concept_code[is.na(PO_pheicd$Description)])
  PO_pheicd$Phecode <- as.character(PO_pheicd$Phecode)
  PO_pheicd$PhecodeLength <- nchar(PO_pheicd$Phecode)
  # length(PO_pheicd[is.na(PO_pheicd$Phecode),1])

  PO_pheicd <- stats::na.omit(PO_pheicd)
  PO_loinc$Phecode = PO_loinc$PhecodeLength = ""
  colnames(PO_loinc) = colnames(PO_pheicd)
  PO_rollup = rbind(PO_pheicd, PO_loinc)

  # PO_rollup <- PO_rollup %>% mutate("Pre-Covid?" = sample(c("Yes", "No"),
  #                                                         dim(PO_pheicd)[1],
  #                                                         replace = TRUE
  # ))
  return(PO_rollup)
}

add.twindows <- function(PO_rollup, windows.size, windows.min, windows.max) {
  breaks <- seq(windows.min, windows.max, by = windows.size)
  breaks.lr <- c(-Inf, breaks, Inf)
  labels <- paste0(
    breaks[1:(length(breaks) - 1)], "-",
    breaks[2:length(breaks)]
  )
  labels <- c(paste0("<", windows.min), labels, paste0(">", windows.max))
  PO_rollup$timewindw <- cut(PO_rollup$days_since_admission,
                             breaks = breaks.lr, right = FALSE,
                             labels = labels
  )
  return(PO_rollup)
}

#' @importFrom magrittr %>%
data.pre <- function(PO_rollup, PatientSummary,
                     windows.size, windows.min, windows.max){
  PatientSummary$days_till_discharge = as.numeric(as.Date(PatientSummary$last_discharge_date) -
                                                    as.Date(PatientSummary$admission_date))
  PO_rollup_tw = add.twindows(PO_rollup, windows.size, windows.min, windows.max)
  PO_rollup_tw = PO_rollup_tw[order(PO_rollup_tw$patient_num,
                                    PO_rollup_tw$Description,
                                    PO_rollup_tw$days_since_admission),]
  PO_rollup_tw_start = PO_rollup_tw %>%
    dplyr::group_by(patient_num,Description) %>%
    dplyr::summarise(first_Diag = min(days_since_admission))
  PO_rollup_tw = merge(PO_rollup_tw, PO_rollup_tw_start,
                       all.x = TRUE)
  PO_rollup_tw <- merge(PO_rollup_tw,
                        PatientSummary[,c("patient_num",
                                          "days_till_discharge")],
                        all.x = TRUE, by = "patient_num")

  PO_rollup_tw$severe <- ifelse(PO_rollup_tw$patient_num %in%
                                  unique(PatientSummary[PatientSummary$severe==1,
                                                        c("patient_num")]), 1, 0)


  return(PO_rollup_tw)
}

#' @importFrom magrittr %>%
filter.case <- function(PO_rollup_tw) {
  # case #1 (on, prior, or after admission)
  b_case1 <- PO_rollup_tw

  # case #2 (after admission)
  b_case2 <- PO_rollup_tw %>% dplyr::filter(first_Diag > 0)

  # case #3 (diagnosis which are new after discharge)
  b_case3 <- PO_rollup_tw[PO_rollup_tw$first_Diag >
                            PO_rollup_tw$days_till_discharge,]

  # case #4 (first diagnosis which are new after discharge)
  b_case4 <-  PO_rollup_tw[(PO_rollup_tw$first_Diag > PO_rollup_tw$days_till_discharge) &
                             (PO_rollup_tw$days_since_admission == PO_rollup_tw$first_Diag),]

  return(list(
    `case1` = b_case1,
    `case2` = b_case2,
    `case3` = b_case3,
    `case4` = b_case4
  ))
}

#' @importFrom magrittr %>%
aggre.TimeDes.count.ICD <- function(PatientObs) {
  CountDiagnosisTW <- PatientObs[PatientObs$PhecodeLength >= 3, ] %>%
    dplyr::filter(stringr::str_detect(concept_type, "ICD")) %>%
    dplyr::group_by(Description, timewindw) %>%
    dplyr::summarise(Freq = dplyr::n_distinct(patient_num)) %>%
    dplyr::arrange(Description)

  CountDiagnosisNPts <- PatientObs %>%
    dplyr::filter(stringr::str_detect(concept_type, "ICD")) %>%
    dplyr::group_by(timewindw) %>%
    dplyr::summarise(Pts = dplyr::n_distinct(patient_num))

  CountDiagnosisDescr <- PatientObs %>%
    dplyr::filter(stringr::str_detect(concept_type, "ICD")) %>%
    dplyr::group_by(Description) %>%
    dplyr::summarise(Pts = dplyr::n_distinct(patient_num)) %>%
    dplyr::arrange(Pts,Description)

  CountDiagnosis <- merge(CountDiagnosisTW, CountDiagnosisNPts, by = c("timewindw"), all.x = TRUE)
  CountDiagnosis$perc <- (CountDiagnosis$Freq / CountDiagnosis$Pts) * 100

  return(CountDiagnosis)
}

#' @importFrom magrittr %>%
aggre.TimeDes.count.LAB <- function(PatientObs) {
  CountDiagnosisTW <- PatientObs %>%
    dplyr::filter(stringr::str_detect(concept_type, "LAB")) %>%
    dplyr::group_by(Description, timewindw) %>%
    dplyr::summarise(Freq = dplyr::n_distinct(patient_num)) %>%
    dplyr::arrange(Description)

  CountDiagnosisNPts <- PatientObs %>%
    dplyr::filter(stringr::str_detect(concept_type, "LAB")) %>%
    dplyr::group_by(timewindw) %>%
    dplyr::summarise(Pts = dplyr::n_distinct(patient_num))

  CountDiagnosisDescr <- PatientObs %>%
    dplyr::filter(stringr::str_detect(concept_type, "LAB")) %>%
    dplyr::group_by(Description) %>%
    dplyr::summarise(Pts = dplyr::n_distinct(patient_num)) %>%
    dplyr::arrange(Pts,Description)

  CountDiagnosis <- merge(CountDiagnosisTW, CountDiagnosisNPts, by = c("timewindw"), all.x = TRUE)
  CountDiagnosis$perc <- (CountDiagnosis$Freq / CountDiagnosis$Pts) * 100
    return(CountDiagnosis)
}



#' Creat bubble plots and density plots
#'
#' Generate data frames that count the number of patients diagnosed with different diseases
#'   under different cases within time windows. Create bubble plots and density plots for rollup ICD data and Lab data.
#'
#' @param PatientSummary Dataframe; provided in the 4CE 2.1 data.
#' @param PatientObservations Dataframe; provided in the 4CE 2.1 data.
#' @param PheCodes Dataframe; a mapping file to roll up ICD codes to the Phecode level.
#' @param loinc_mapping Dataframe; connecting loinc codes to detailed description.
#' @param digits Integer; the digit of ICD code. If default = NULL, set to
#'   be the largest digit of numbers in the \code{concept_code} column in \code{PatientObservations}.
#' @param windows.size.bubble Integer; the size of each window in the bubble plot, default=30.
#' @param windows.size.density Integer; the size of each window in the density plot, default=30.
#' @param windows.min Integer; the minimum time point in the bubble plot, default = 0.
#' @param windows.max Integer; the maximum time point in the bubble plot, default = 120.
#' @param topn Integer; number of the most frequently diagnosed diseases to display in the bubble plot (default=NULL).
#'
#'
#' @return A list with the following components:
#'
#' \tabular{ll}{
#'  \code{data} \tab Processed \code{PatientObservations} with rollup information and input data for bubbleplot. \cr
#'  \code{bubble} \tab Bubble plots for ICD and LAB count data with 4 cases.  \cr
#'  \code{density} \tab Density plots for continuous LAB data. \cr
#'  }
#'
#' @export
main <- function(PatientSummary, PatientObservations,
                 PheCodes, loinc_mapping, digits = NULL,
                 windows.size.bubble = 30, windows.size.density = 30,
                 windows.min = 0, windows.max = 120,
                 topn = NULL) {

  PatientObservationsEnctrs <- PatientObservations

  ## Roll up icd codes to phe codes
  PO_rollup <- rollup(
    PatientObservationsEnctrs,
    PheCodes, loinc_mapping, digits = NULL
  )

  ## Prepare data for bubble plot
  PO_rollup_tw <- data.pre(PO_rollup, PatientSummary,
                           windows.size.bubble, windows.min, windows.max)

  ## Generate 4 dataframes related to the 4 bubble plots
  POPheCodes_case <- filter.case(PO_rollup_tw)

  ## Aggregate patient info to timewindows-disease level info
  CountDiagnosis_case1_icd <-
    aggre.TimeDes.count.ICD(POPheCodes_case$case1)
  CountDiagnosis_case2_icd <-
    aggre.TimeDes.count.ICD(POPheCodes_case$case2)
  CountDiagnosis_case3_icd <-
    aggre.TimeDes.count.ICD(POPheCodes_case$case3)
  CountDiagnosis_case4_icd <-
    aggre.TimeDes.count.ICD(POPheCodes_case$case4)

  CountDiagnosis_case1_lab <-
    aggre.TimeDes.count.LAB(POPheCodes_case$case1)
  CountDiagnosis_case2_lab <-
    aggre.TimeDes.count.LAB(POPheCodes_case$case2)
  CountDiagnosis_case3_lab <-
    aggre.TimeDes.count.LAB(POPheCodes_case$case3)
  CountDiagnosis_case4_lab <-
    aggre.TimeDes.count.LAB(POPheCodes_case$case4)


  result = list(
    `PatientObersvations_pro` = PO_rollup_tw,
    `Bubble_data` = list(`ICD` =
                           list(
                             `case1` = CountDiagnosis_case1_icd,
                             `case2` = CountDiagnosis_case2_icd,
                             `case3` = CountDiagnosis_case3_icd,
                             `case4` = CountDiagnosis_case4_icd
                           ),
                         `LAB` =
                           list(
                             `case1` = CountDiagnosis_case1_lab,
                             `case2` = CountDiagnosis_case2_lab,
                             `case3` = CountDiagnosis_case3_lab,
                             `case4` = CountDiagnosis_case4_lab
                           )
    )
  )
  Bubble = list(`LAB` = list(), `ICD` = list())
  for (i in 1:4) {
    Bubble$LAB[[i]] = bubbleplot(result$Bubble_data$LAB[[i]], topn)
    Bubble$ICD[[i]] = bubbleplot(result$Bubble_data$ICD[[i]], topn)
  }
  LAB.density = labplot(PO_rollup_tw, loinc_mapping, windows.size.density,
                        windows.min, windows.max)
  return(list(`data` = result,
              `bubble` = Bubble,
              `density` = LAB.density))
}


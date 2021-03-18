#' Bubble plot
#'
#' Create a bubble plot for count data
#'
#' @param CountDiagnosis Dataframe; one case in \code{Bubble_data} output from \code{\link{main}}.
#' @param topn Integer; if \code{topn} != NULL, the y-axis of the bubble plot only contains the most frequent \code{topn} diseases.
#' @return A ggplot object.
#' @export
#' @importFrom magrittr %>%
bubbleplot <- function(CountDiagnosis, topn) {
  CountDiagnosis.agg = CountDiagnosis %>%
    dplyr::group_by(Description) %>%
    dplyr::summarise(diag.time = sum(Freq))
  CountDiagnosis.agg = CountDiagnosis.agg[order(CountDiagnosis.agg$diag.time,
                                                decreasing = TRUE),]

  if (is.null(topn)) {
    topn = nrow(CountDiagnosis.agg)
  }else{
    topn = min(nrow(CountDiagnosis.agg),topn)
  }
  CountDiagnosis.agg = CountDiagnosis.agg[1:topn,]
  CountDiagnosis = merge(CountDiagnosis.agg,
                         CountDiagnosis, all.x = TRUE, by = "Description")
  CountDiagnosis = CountDiagnosis[order(CountDiagnosis$diag.time,decreasing = TRUE),]
  CountDiagnosis$Description = as.character(CountDiagnosis$Description)

  Bubble <-
    ggplot2::ggplot(
      CountDiagnosis,
      ggplot2::aes(x = timewindw, y = stats::reorder(Description,diag.time), size = perc, color = Freq)
    ) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::scale_size(range = c(.1, 15), name = "% Patients") +
    ggplot2::scale_colour_gradient(
      low = "#4895ef", high = "#990000",
      n.breaks = 5, name = "# Patients"
    ) +
    ggplot2::theme(text = ggplot2::element_text(size = 12)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.75)) +
    ggplot2::xlab("Day since admission") +
    ggplot2::ylab("Codes")

  return(Bubble)
}

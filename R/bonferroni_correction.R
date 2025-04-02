#' @title Apply Bonferroni correction to p-values
#'
#' @param data the data frame containing the test results.
#'
#' @return the data frame with adjusted p-values.
#' @export
#'
#' @examples
bonferroni_correction <- function(data) {
  num_comparisons <- nrow(data)
  data <- data |>
    dplyr::mutate(p.adjusted = p.value * num_comparisons) |>
    dplyr::mutate(p.adjusted = ifelse(p.adjusted > 1, 1, p.adjusted))

  return(data)
}

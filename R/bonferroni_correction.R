#' @title Apply Bonferroni correction to p-values
#'
#' @param data the data frame containing the test results.
#'
#' @return the data frame with adjusted p-values.
#' @export
#'
bonferroni_correction <- function(data) {
  num_comparisons <- nrow(data)
  data <- data |>
    dplyr::mutate(adj.p.value = p.value * num_comparisons) |>
    dplyr::mutate(adj.p.value = ifelse(adj.p.value > 1, 1, adj.p.value))

  return(data)
}

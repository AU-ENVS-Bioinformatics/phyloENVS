#' @title Perform ANOVA and Tukey's HSD test
#'
#' @param data the data for a single taxon.
#' @param design the test design (e.g., "Location").
#'
#' @return a list containing ANOVA and Tukey's HSD results.
#' @export
#'
#' @examples
perform_anova_tukey <- function(data, design) {
  formula <- as.formula(paste("Abundance ~", design))
  anova_results <- stats::aov(formula, data = data)
  tukey_results <- stats::TukeyHSD(anova_results)
  list(anova = broom::tidy(anova_results), tukey = broom::tidy(tukey_results))
}

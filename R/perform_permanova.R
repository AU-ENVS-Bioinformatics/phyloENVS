#' @title Perform PERMANOVA analysis on phyloseq object
#' @description This function performs PERMANOVA (Permutational Multivariate Analysis of Variance) on a phyloseq object using multiple design formulas, and saves the results as an Excel file.
#'
#' @param physeq A `phyloseq` object containing microbiome data.
#' @param designs A character vector specifying the model formulas (e.g., c("Concentration", "Temperature", "Concentration + Temperature")).
#' @param stats_path A string specifying the path where the output Excel file should be saved.
#' @param convert_to_rel convert counts to relative abundances before analysis. Default is TRUE.
#'
#' @return A data frame containing PERMANOVA results for all specified designs, with an additional column `"Model"` indicating the tested formula.
#' @export
#'
#' @examples
perform_permanova <- function(physeq, designs, stats_path, convert_to_rel = TRUE) {

  # Normalize.
  if (convert_to_rel == TRUE){
    physeq_rel <- physeq |>
      phyloseq::transform_sample_counts(function(x) x/sum(x))
  } else {
    physeq_rel <- physeq
  }

  # Extract OTU abundances and metadata
  otu  <- microbiome::abundances(physeq_rel)
  meta <- microbiome::meta(physeq_rel)

  # Create an empty list to store results
  results_list <- list()

  # Run PERMANOVA for each design
  for (design in designs) {
    formula <- as.formula(paste("t(otu) ~", design))
    results <- vegan::adonis2(formula, data = meta, permutations = 999, method = "bray") |>
      as.data.frame() |>
      dplyr::mutate(Model = design)
    results_list[[design]] <- results
  }

  # Combine all results into one data frame
  results_combined <- do.call(rbind, results_list)

  # Create the output directory if not exists
  if (!dir.exists(stats_path)) {
    dir.create(stats_path, recursive = TRUE)
  }

  # Define output file path
  output_file <- file.path(stats_path, "permanova_results.xlsx")

  # Write results to an Excel file (one sheet)
  openxlsx::write.xlsx(results_combined, file = output_file, rowNames = TRUE)

  return(results_combined)
}

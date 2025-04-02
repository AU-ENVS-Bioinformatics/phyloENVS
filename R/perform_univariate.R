#' @title Perform statistical tests on phyloseq data
#'
#' @param physeq a phyloseq object containing microbiome data.
#' @param stats_path a string specifying the directory to save the results.
#' @param level_glom name of the level to agglomerate counts. Default is "Phylum".
#' @param test_designs a vector of test designs (e.g., c("Concentration", "Temperature")).
#' @param lower_limit the minimum abundance threshold for including taxa. Default is 0.04 (4%).
#'
#' @return results are saved to the specified directory.
#' @export
#'
#' @examples
perform_univariate <- function(physeq, stats_path, level_glom = "Phylum", designs, lower_limit = 0.04) {

  # Convert to symbol.
  level_glom_sym <- rlang::sym(level_glom)

  # Preprocess phyloseq data.
  physeq_df <- physeq |>
    phyloseq::tax_glom(taxrank = level_glom) |>
    phyloseq::transform_sample_counts(function(x) x/sum(x)) |>
    phyloseq::psmelt()

  # Get abundant taxa.
  abundant_taxa <- physeq_df |>
    dplyr::group_by(!!level_glom_sym) |>
    dplyr::summarize(MaxAbundance = max(Abundance)) |>
    dplyr::filter(MaxAbundance > lower_limit) |>
    dplyr::pull(!!level_glom_sym)

  physeq_df <- physeq_df |>
    dplyr::filter(!!level_glom_sym %in% abundant_taxa)

  # Create the output directory if not exists
  if (!dir.exists(stats_path)) {
    dir.create(stats_path, recursive = TRUE)
  }

  # List to store results for each taxon.
  results_list <- list()

  for (taxon in abundant_taxa) {
    taxon_data <- physeq_df |>
      dplyr::filter(!!level_glom_sym == taxon)

    # Store results for each test design
    test_results_list <- list()

    # Run statistical tests
    for (design in designs) {
      anova_tukey_results <- perform_anova_tukey(taxon_data, design)
      pairwise_results    <- pairwise.t.test(taxon_data$Abundance, taxon_data[[design]], p.adjust.method = "none")
      pairwise_results_df <- broom::tidy(pairwise_results)
      pairwise_results_df <- bonferroni_correction(pairwise_results_df)

      test_results_list[[design]] <- list(anova = anova_tukey_results$anova,
                                          tukey = anova_tukey_results$tukey,
                                          pairwise = pairwise_results_df)
    }

    results_list[[taxon]] <- test_results_list
  }

  # Save results to Excel
  output_file <- file.path(stats_path, "univariate_results.xlsx")
  wb <- openxlsx::createWorkbook()

  # Write results to workbook
  for (taxon in abundant_taxa) {
    openxlsx::addWorksheet(wb, taxon)
    row_offset <- 1

    for (design in designs) {
      taxon_results <- results_list[[taxon]][[design]]

      openxlsx::writeData(wb, taxon, "ANOVA Results", startRow = row_offset, startCol = 1, colNames = FALSE)
      row_offset <- row_offset + 1
      openxlsx::writeData(wb, taxon, taxon_results$anova, startRow = row_offset, startCol = 1)
      row_offset <- nrow(taxon_results$anova) + row_offset + 2

      openxlsx::writeData(wb, taxon, taxon_results$tukey, startRow = row_offset, startCol = 1)
      row_offset <- nrow(taxon_results$tukey) + row_offset + 2

      openxlsx::writeData(wb, taxon, taxon_results$pairwise, startRow = row_offset, startCol = 1)
      row_offset <- nrow(taxon_results$pairwise) + row_offset + 2
    }

    # Highlight significant results
    #highlight_sig_results(wb, taxon, designs)
  }

  openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
  message("Results saved to: ", output_file)
}

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

  headers <- c("ANOVA results", "Tukey's HSD test", "T-test (with Bonferroni correction)")

  # Save results to Excel
  output_file <- file.path(stats_path, "univariate_results.xlsx")
  wb <- openxlsx::createWorkbook()

  style1 <- openxlsx::createStyle(fgFill = "#003d73",
                                  fontSize = 14,
                                  fontColour = "white",
                                  textDecoration = "bold",
                                  border = "TopBottomLeftRight",
                                  borderStyle = "thick",
                                  borderColour = "#003d73")

  style2 <- openxlsx::createStyle(fontSize = 11,
                                  fontColour = "#003d73",
                                  textDecoration = "bold")

  style3 <- openxlsx::createStyle(fgFill = "#93a8c2",
                                  fontSize = 11,
                                  fontColour = "#003d73",
                                  textDecoration = "bold",
                                  border = "TopBottomLeftRight",
                                  borderColour = "#003d73")

  style4 <- openxlsx::createStyle(fgFill = "#ffa293",
                                  fontSize = 11,
                                  textDecoration = "bold")

  # Write results to workbook
  for (design in designs) {
    openxlsx::addWorksheet(wb, design)
    row_offset <- 1

    for (taxon in abundant_taxa) {
      taxon_results <- results_list[[taxon]][[design]]
      col_offset <- 1
      span <- sum(unlist(lapply(taxon_results, ncol))) + 2

      openxlsx::writeData(wb, design, taxon, startRow = row_offset, startCol = col_offset, colNames = FALSE)
      openxlsx::mergeCells(wb, design, cols = col_offset:span, rows = row_offset)
      openxlsx::addStyle(wb, design, style1, cols = col_offset:span, rows = row_offset, gridExpand = TRUE, stack = TRUE)

      for (i in 1:length(taxon_results)){
        openxlsx::writeData(wb, design, headers[i], startRow = row_offset + 1, startCol = col_offset, colNames = FALSE)
        openxlsx::addStyle(wb, design, style2, rows = row_offset + 1, cols = col_offset, gridExpand = TRUE, stack = TRUE)
        openxlsx::writeData(wb, design, taxon_results[[i]], startRow = row_offset + 2, startCol = col_offset, borders = "all")
        openxlsx::addStyle(wb, design, style3, rows = row_offset + 2, cols = col_offset:(col_offset + ncol(taxon_results[[i]]) -1), gridExpand = TRUE, stack = TRUE)

        sig_results <- taxon_results[[i]][, colnames(taxon_results[[i]]) %in% c("p.value", "adj.p.value")] < 0.05
        print(sig_results)
        sig_results <- sig_results[, apply(sig_results, 2, function(x) any(x == TRUE, na.rm = TRUE))]
        print(dim(sig_results))

        if (nrow(sig_results) > 0){
          sig_rows <- which(apply(sig_results, 1, function(x) x == TRUE))
          sig_cols <- which(colnames(taxon_results[[i]]) %in% colnames(sig_results))
          openxlsx::addStyle(wb, design, style4, rows = sig_rows + row_offset + 2, cols = sig_cols + col_offset - 1, gridExpand = TRUE, stack = TRUE)
        }

        col_offset <- col_offset + ncol(taxon_results[[i]]) + 1
      }

      row_offset <- row_offset + max(unlist(lapply(taxon_results, nrow))) + 4

    }

    # Highlight significant results
    #highlight_sig_results(wb, taxon, designs)

  }

  openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
  message("Results saved to: ", output_file)
}


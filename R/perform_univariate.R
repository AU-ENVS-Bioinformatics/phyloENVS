#' @title Perform univariate statistical testing across microbial taxa
#' @description Perform ANOVA, Tukey HSD, and pairwise t-tests with Bonferroni correction across microbial taxa from a phyloseq object, based on one or more experimental design factors. The results are saved in excel.
#'
#' @details
#'  Statistical tests
#'
#' The following statistical tests are applied for each taxon across the specified design variables:
#'
#' * ANOVA: Tests whether there is a statistically significant difference in mean abundance across all groups. Useful as a global indicator.
#'
#' * Tukey's HSD: A test performed after ANOVA. Identifies which specific group pairs differ significantly, with correction for multiple comparisons (family-wise error rate).
#'
#' * Pairwise t-tests with Bonferroni correction: An alternative pairwise approach that adjusts p-values using the Bonferroni method. More flexible than Tukey's HSD and works with unequal variances or group sizes.
#'
#' Significant results are highlighted in the Excel output.
#
#' @param physeq a phyloseq object.
#' @param stats_path a string specifying the path where the output Excel file should be saved.
#' @param file_name a string specifying the name of the output excel file. If not specified, the file is named univariate_results.xlsx.
#' @param level_glom name of the taxonomic level to agglomerate counts. Default is "Phylum".
#' @param designs a vector of test designs (e.g., c("Concentration", "Temperature")).
#' @param signi_limit the significance level. Default is 0.05 (5 \%).
#' @param lower_limit the minimum abundance threshold for including taxa. Default is 0.05 (5 \%).
#' @param higher_limit the maximum abundance threshold for including taxa. Default is 1 (100 \%).
#'
#' @return results are saved to the specified directory.
#' @export
#'
#' @examples
#' # Data phyloseq object:
#' data(qaanaaq_rRNA)
#' phylo <- qaanaaq_rRNA
#'
#' \dontrun{
#' perform_univariate(phylo,
#'                    stats_path = "results/",
#'                    designs = c("Direction", "Wetness"))
#' }
perform_univariate <- function(physeq,
                               stats_path,
                               file_name = "univariate_results.xlsx",
                               level_glom = "Phylum",
                               designs,
                               signi_limit = 0.05,
                               lower_limit = 0.05,
                               higher_limit = 1) {

  # ------------#
  # Check inputs
  # ------------#

  if (class(physeq)[1] != "phyloseq") {
    stop("`physeq` must be a phyloseq object")
  }

  if (!is.character(stats_path)){
    stop("`stats_path` must be character")
  }

  if (!is.character(file_name)){
    stop("`file_name` must be character")
  }

  if (!is.character(level_glom)){
    stop("`level_glom` must be character")
  }

  if (!all(designs %in%  colnames(phyloseq::sample_data(physeq)))) {
    stop(paste("Designs are not found in sample data"))
  }

  if (!is.numeric(signi_limit)){
    stop("`num` must be numeric")
  }

  if (!is.numeric(lower_limit)){
    stop("`num` must be numeric")
  }

  if (!is.numeric(higher_limit)){
    stop("`num` must be numeric")
  }

  if (signi_limit > 1 | signi_limit < 0) {
    stop("`signi_limit` must be between 0 and 1")
  }

  if (lower_limit > 1 | lower_limit < 0) {
    stop("`lower_limit` must be between 0 and 1")
  }

  if (higher_limit > 1 | higher_limit < 0) {
    stop("`higher_limit` must be between 0 and 1")
  }

  # ------------#

  # Convert to symbol
  level_glom_sym <- rlang::sym(level_glom)

  # Preprocess phyloseq data
  physeq_df <- physeq |>
    phyloseq::tax_glom(taxrank = level_glom) |>
    phyloseq::transform_sample_counts(function(x) x/sum(x)) |>
    phyloseq::psmelt()

  # Get the relevant taxa
  abundant_taxa <- physeq_df |>
    dplyr::group_by(!!level_glom_sym) |>
    dplyr::summarize(MaxAbundance = max(Abundance)) |>
    dplyr::filter(MaxAbundance > lower_limit,
                  MaxAbundance < higher_limit) |>
    dplyr::pull(!!level_glom_sym)

  physeq_df <- physeq_df |>
    dplyr::filter(!!level_glom_sym %in% abundant_taxa)

  # Create the output directory if not exists
  if (!dir.exists(stats_path)) {
    dir.create(stats_path, recursive = TRUE)
  }

  # List to store results for each taxon
  results_list <- list()

  for (taxon in abundant_taxa) {
    taxon_data <- physeq_df |>
      dplyr::filter(!!level_glom_sym == taxon)

    # Store results for each test design
    test_results_list <- list()

    # Run statistical tests
    for (design in designs) {

      formula <- as.formula(paste("Abundance ~", design))

      anova_results <- stats::aov(formula, data = taxon_data)

      tukey_results <- stats::TukeyHSD(anova_results) |>
        broom::tidy()

      anova_results <-  anova_results |>
        broom::tidy()

      pairwise_results <- pairwise.t.test(taxon_data$Abundance, taxon_data[[design]], p.adjust.method = "none") |>
        broom::tidy() |>
        bonferroni_correction()

      test_results_list[[design]] <- list(anova = anova_results,
                                          tukey = tukey_results,
                                          pairwise = pairwise_results)
    }

    results_list[[taxon]] <- test_results_list
  }

  headers <- c("ANOVA results", "Tukey's HSD test", "T-test (with Bonferroni correction)")

  # Prepare Excel workbook.
  wb <- openxlsx::createWorkbook()

  # Styles.
  style_header1 <- openxlsx::createStyle(fgFill = "#003d73",
                                         fontSize = 14,
                                         fontColour = "white",
                                         textDecoration = "bold",
                                         border = "TopBottomLeftRight",
                                         borderStyle = "thick",
                                         borderColour = "#003d73")

  style_header2 <- openxlsx::createStyle(fontSize = 11,
                                         fontColour = "#003d73",
                                         textDecoration = "bold")

  style_header3 <- openxlsx::createStyle(fgFill = "#b9c6d7",
                                         fontSize = 11,
                                         fontColour = "#003d73",
                                         textDecoration = "bold",
                                         border = "TopBottomLeftRight",
                                         borderColour = "#003d73")

  style_signi <- openxlsx::createStyle(fgFill = "#ffa293",
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
      openxlsx::addStyle(wb, design, style_header1, cols = col_offset:span, rows = row_offset, gridExpand = TRUE, stack = TRUE)

      for (i in 1:length(taxon_results)){
        openxlsx::writeData(wb, design, headers[i], startRow = row_offset + 1, startCol = col_offset, colNames = FALSE)
        openxlsx::addStyle(wb, design, style_header2, rows = row_offset + 1, cols = col_offset, gridExpand = TRUE, stack = TRUE)
        openxlsx::writeData(wb, design, taxon_results[[i]], startRow = row_offset + 2, startCol = col_offset, borders = "all")
        openxlsx::addStyle(wb, design, style_header3, rows = row_offset + 2, cols = col_offset:(col_offset + ncol(taxon_results[[i]]) -1), gridExpand = TRUE, stack = TRUE)

        # Highlight significant results
        pval_cols <- which(colnames(taxon_results[[i]]) %in% c("p.value", "adj.p.value"))

        if (length(pval_cols) > 0) {
          for (col_index in pval_cols) {
            for (row_index in 1:nrow(taxon_results[[i]])) {
              value <- taxon_results[[i]][row_index, col_index]
              if (!is.na(value) && value < signi_limit) {
                openxlsx::addStyle(wb, design, style_signi, rows = row_offset + 2 + row_index, cols = col_offset + col_index - 1, gridExpand = TRUE, stack = TRUE)
              }
            }
          }
        }
        col_offset <- col_offset + ncol(taxon_results[[i]]) + 1
      }
      row_offset <- row_offset + max(unlist(lapply(taxon_results, nrow))) + 4
    }
  }

  # Save results
  output_file <- file.path(stats_path, fine_name)
  openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
  message("Results saved to: ", output_file)

}

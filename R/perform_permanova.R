#' @title Perform PERMANOVA analysis on phyloseq object
#' @description This function performs PERMANOVA (Permutational Multivariate Analysis of Variance) on a phyloseq object using multiple design formulas, and saves the results as an Excel file.
#'
#' @param physeq a phyloseq object.
#' @param stats_path a string specifying the path where the output Excel file should be saved.
#' @param designs a character vector specifying the model formulas (e.g., c("Concentration", "Temperature", "Concentration + Temperature")).
#' @param signi_limit the significance level. Default is 0.05 (5 \%).
#' @param convert_to_rel convert counts to relative abundances before analysis. Default is TRUE.
#' @param dist_method dissimilarity index used in vegdist (see vegan package). Default is "bray".
#' @param n_permute number of permutations. Default is 999.
#'
#' @return results are saved to the specified directory.
#' @export
#'
#' @examples
#' #' # Data phyloseq object:
#' data(qaanaaq_rRNA)
#' phylo <- qaanaaq_rRNA
#'
#' \dontrun{
#' perform_permanova(phylo,
#'                   stats_path = "results/",
#'                   designs = c("Direction", "Wetness"))
#' }
perform_permanova <- function(physeq,
                              stats_path,
                              designs,
                              signi_limit = 0.05,
                              convert_to_rel = TRUE,
                              dist_method = "bray",
                              n_permute = 999){

  # ------------#
  # Check inputs
  # ------------#

  if (class(physeq)[1] != "phyloseq") {
    stop("`physeq` must be a phyloseq object")
  }

  if (!is.character(stats_path)){
    stop("`stats_path` must be character")
  }

  if (!all(designs %in%  colnames(phyloseq::sample_data(physeq)))) {
    stop(paste("Designs are not found in sample data"))
  }

  if (signi_limit > 1 | signi_limit < 0) {
    stop("`signi_limit` must be between 0 and 1")
  }

  if (!is.logical(convert_to_rel)) {
    stop("`convert_to_rel` must be logical")
  }

  if (!is.character(dist_method)){
    stop("`dist_method` must be character")
  }

  if (!is.numeric(n_permute)){
    stop("`n_permute` must be numeric")
  }

  # ------------#


  # Normalize.
  if (convert_to_rel == TRUE){
    physeq_rel <- physeq |>
      phyloseq::transform_sample_counts(function(x) x/sum(x))
  } else {
    physeq_rel <- physeq
  }

  # Extract OTU abundances and metadata.
  otu  <- microbiome::abundances(physeq_rel)
  meta <- microbiome::meta(physeq_rel)

  # Create an empty list to store results.
  results_list <- list()

  # Run PERMANOVA for each design.
  for (design in designs) {
    formula <- as.formula(paste("t(otu) ~", design))
    results <- vegan::adonis2(formula,
                              data = meta,
                              permutations = n_permute,
                              method = dist_method) |>
      as.data.frame() |>
      dplyr::mutate(Model = design)
    results_list[[design]] <- results
  }

  # Combine all results into one data frame.
  results_combined <- do.call(rbind, results_list)

  # Create the output directory if not exists
  if (!dir.exists(stats_path)) {
    dir.create(stats_path, recursive = TRUE)
  }

  # Prepare Excel workbook.
  wb <- openxlsx::createWorkbook()
  sheet <- "PERMANOVA"
  openxlsx::addWorksheet(wb, sheet)

  # Styles.
  style_header1 <- openxlsx::createStyle(fontSize = 11,
                                         fontColour = "#003d73",
                                         textDecoration = "bold")

  style_header2 <- openxlsx::createStyle(fgFill = "#b9c6d7",
                                         fontSize = 11,
                                         fontColour = "#003d73",
                                         textDecoration = "bold",
                                         border = "TopBottomLeftRight",
                                         borderColour = "#003d73")

  style_signi <- openxlsx::createStyle(fgFill = "#ffa293",
                                       fontSize = 11,
                                       textDecoration = "bold")

  # Write grouped results by design.
  row_offset <- 1

  for (design in names(results_list)) {
    design_results <- results_list[[design]]

    openxlsx::writeData(wb, sheet, design, startRow = row_offset, colNames = FALSE)
    openxlsx::mergeCells(wb, sheet, cols = 1:ncol(design_results), rows = row_offset)
    openxlsx::addStyle(wb, sheet, style_header1, rows = row_offset, cols = 1, gridExpand = TRUE)

    openxlsx::writeData(wb, sheet, design_results, startRow = row_offset + 1, rowNames = FALSE, borders = "all")
    openxlsx::addStyle(wb, sheet, style_header2, rows = row_offset + 1, cols = 1:ncol(design_results), gridExpand = TRUE)

    if ("Pr(>F)" %in% colnames(design_results)) {
      sig_rows <- which(design_results[["Pr(>F)"]] < signi_limit)
      pval_col_index <- which(colnames(design_results) == "Pr(>F)")

      if (length(sig_rows) > 0) {
        openxlsx::addStyle(wb, sheet, style_signi, rows = row_offset + 1 + sig_rows, cols = pval_col_index, gridExpand = TRUE)
      }
    }
    row_offset <- row_offset + nrow(design_results) + 1
  }

  # Save results.
  output_file <- file.path(stats_path, "permanova_results.xlsx")
  openxlsx::saveWorkbook(wb, file = output_file, overwrite = TRUE)
  message("Distance: ", dist_method)
  message("Permutations: ", n_permute)
  message("Results saved to: ", output_file)
}


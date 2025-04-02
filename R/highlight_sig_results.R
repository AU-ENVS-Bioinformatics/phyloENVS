#' @title Highlight significant results in excel
#'
#' @param wb excel workbook object.
#' @param taxon the taxon (e.g., phylum) to highlight results for.
#' @param designs a vector of test designs.
#'
#' @return modifies the excel workbook directly.
#' @export
#'
#' @examples
highlight_sig_results <- function(wb, taxon, designs) {

  for (design in designs) {

    # Get the result for this test design
    sheet_data <- openxlsx::readWorkbook(wb, taxon, startRow = 1, colNames = TRUE)

    # Loop through the rows and highlight significant p-values
    for (row in 1:nrow(sheet_data)) {
      if (sheet_data[row, "p.adjusted"] < 0.05) {
        openxlsx::addStyle(wb,
                           taxon,
                           style = openxlsx::createStyle(fontColour = "#FFFFFF", bgFill = "#FF0000"),
                           rows = row,
                           cols = 1:ncol(sheet_data),
                           gridExpand = TRUE)
      }
    }
  }
}

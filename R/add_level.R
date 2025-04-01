#' @title Add taxonomic level
#' @description Add user-specified taxonomic level, e.g., domain.
#'
#' @param physeq a phyloseq object.
#' @param level the level to target/bin into a new overall level.
#' @param level_name name of the new overall level.
#' @param look_up_table a look up table to categories the target level. Example: kingdom_to_domain <- c("Bacteria" = "Prokaryotes", "SAR" = "Eukaryotes")
#'
#' @return an updated phyloseq object with the user-specified taxonomic level.
#' @export
#'
#' @examples
add_level <- function(physeq,
                      level,
                      level_name,
                      look_up_table){

  # Convert character to symbol.
  level_sym <- rlang::sym(level)
  level_name_sym <- rlang::sym(level_name)

  # Add the overall new level.
  new_tax_table <- as.data.frame(phyloseq::tax_table(physeq)) |>
    tibble::as_tibble() |>
    dplyr::mutate(!!level_name_sym := dplyr::recode(!!level_sym,
                                                !!!look_up_table,
                                                .default = "Unassigned")) |>
    dplyr::relocate(!!level_name_sym, .before = !!level_sym) |>
    as.matrix() |>
    phyloseq::tax_table()

  # Renew the tax table.
  phyloseq::tax_table(physeq) <- new_tax_table

  return(physeq)
}

#' @title Add taxonomic level
#' @description Add user-specified taxonomic level, e.g., "Domain".
#'
#' @param physeq a phyloseq object.
#' @param level the level to target/bin into a new overall level, e.g., "Kingdom".
#' @param level_name name of the new overall level, e.g., "Domain".
#' @param look_up_table a look up table to categories the target level. Example: kingdom_to_domain <- c("Bacteria" = "Prokaryotes", "SAR" = "Eukaryotes")
#'
#' @return an updated phyloseq object with the user-specified taxonomic level placed prior to the target/binned level.
#' @export
#'
#' @examples
#' # Data phyloseq object:
#' data(qaanaaq_rRNA)
#' phylo <- qaanaaq_rRNA
#'
#' # Look up table:
#' kingdom_to_domain <- c("Amorphea" = "Eukaryotes",
#'                        "Archaeplastida" = "Eukaryotes",
#'                        "Bacteria" = "Prokaryotes",
#'                        "Hacrobia" = "Eukaryotes",
#'                        "SAR" = "Eukaryotes")
#'
#' # Add the new overall level.
#' phylo <- add_level(physeq = phylo,
#'                    level = "Kingdom",
#'                    level_name = "Domain",
#'                    look_up_table = kingdom_to_domain)
#'
add_level <- function(physeq,
                      level,
                      level_name,
                      look_up_table){

  # ------------#
  # Check inputs
  # ------------#

  if (class(physeq)[1] != "phyloseq") {
    stop("`physeq` must be a phyloseq object")
  }

  if (!is.character(level)) {
    stop("`level` must be character")
  }

  if (!is.character(level_name)) {
    stop("`level_name` must be character")
  }

  if (!(level %in% colnames(phyloseq::tax_table(physeq)))) {
    stop(paste(level, "level is not found in taxonomy table"))
  }

  # ------------#

  # Convert character to symbol.
  level_sym <- rlang::sym(level)
  level_name_sym <- rlang::sym(level_name)

  # Add the overall new level.
  new_tax_table <- phyloseq::tax_table(physeq) |>
    as.data.frame() |>
    dplyr::mutate(!!level_name_sym := dplyr::recode(!!level_sym,
                                                    !!!look_up_table,
                                                    .default = "Unassigned")) |>
    dplyr::relocate(!!level_name_sym,
                    .before = !!level_sym) |>
    as.matrix() |>
    phyloseq::tax_table()

  # Renew the tax table.
  phyloseq::tax_table(physeq) <- new_tax_table

  return(physeq)
}

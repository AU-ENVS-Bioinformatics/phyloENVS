#' @title PhyloENVS theme with gridlines
#' @description Fundamental PhyloENVS theme with gridlines
#'
#' @param both use both vertical and horizontal gridlines. Otherwise, only horizontal gridlines are used. Default is FALSE.
#'
#' @return A custom theme (with gridlines) to be applied across package plots.
#' @export
#'
#' @examples
#' # Data phyloseq object:
#' data(qaanaaq_rRNA)
#' phylo <- qaanaaq_rRNA
#'
#' vis_nmds(physeq = phylo,
#'          group_color = "Location",
#'          group_shape = "Transect") +
#'          theme_ENVS_with_gridlines()
#'
theme_ENVS_with_gridlines <- function(both = FALSE) {
  if (both) {
    theme_ENVS() +
      ggplot2::theme(panel.grid.major = ggplot2::element_line(color = "#E3E3E3",
                                                              linewidth = 0.3),
                     panel.grid.minor = ggplot2::element_line(color = "#E3E3E3",
                                                              linewidth = 0.2))
  } else {
    theme_ENVS() +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color = "#E3E3E3",
                                                                linewidth = 0.3),
                     panel.grid.minor.y = ggplot2::element_line(color = "#E3E3E3",
                                                                linewidth = 0.2))
  }
}

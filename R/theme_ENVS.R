#' @title PhyloENVS theme
#' @description Fundamental PhyloENVS theme
#'
#' @return NMDS plot created with ggplot.
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
#'          theme_ENVS()
#'
theme_ENVS <- function() {
  ggplot2::theme_classic() +
    ggplot2::theme(axis.title.x = ggplot2::element_text(face = "bold",
                                                        vjust = -1),
                   axis.title.y = ggplot2::element_text(face = "bold",
                                                        vjust = 3),
                   legend.title = ggplot2::element_text(face = "bold"),
                   legend.key = ggplot2::element_rect(fill = "white"),
                   panel.background = ggplot2::element_rect(fill = "#F7F7F7",
                                                            color = NA),
                   panel.border = ggplot2::element_rect(color = "black",
                                                        linewidth = 1,
                                                        fill = NA),
                   strip.background = ggplot2::element_rect(color = "black",
                                                            fill = "white",
                                                            linewidth = 1,
                                                            linetype = "solid"),
                   strip.text = ggplot2::element_text(color = "black",
                                                      face = "bold",
                                                      margin = ggplot2::margin(t = 5, b = 5)),
                   panel.spacing = ggplot2::unit(0.8, "lines"),
                   plot.margin = ggplot2::margin(20, 20, 20, 20))
}

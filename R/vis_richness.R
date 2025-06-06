#' @title Visualize richness plot
#' @description Plot alpha diversity with Shannon and Inverse Simpson measures.
#'
#' @param physeq a phyloseq object
#' @param group_x the x axis sample variable to plot.
#' @param group_color the parameter in metadata to color by.
#' @param measures character vector of diversity indices names. Default is c("Simpson", "InvSimpson"). Supported: c("Observed", "Chao1", "ACE", "Shannon", "Simpson", "InvSimpson", "Fisher").
#'
#' @return richness plots created with ggplot.
#' @export
#'
#' @examples
#' # Data phyloseq object:
#' data(qaanaaq_rRNA)
#' phylo <- qaanaaq_rRNA
#'
#' vis_richness(physeq = phylo,
#'              group_x = "Location",
#'              group_color = "Transect")
#'
vis_richness <- function(physeq,
                         group_x,
                         group_color,
                         measures = c("Simpson", "InvSimpson")){

  # ------------#
  # Check inputs
  # ------------#

  if (class(physeq)[1] != "phyloseq") {
    stop("`physeq` must be a phyloseq object")
  }

  if (!is.character(group_x)){
    stop("`group_x` must be character")
  }

  if (!is.character(group_color)){
    stop("`group_color` must be character")
  }

  if (!all(measures %in% c("Observed", "Chao1", "ACE", "Shannon", "Simpson", "InvSimpson", "Fisher"))){
    stop("Invalid diversity indices. Supported: 'Observed', 'Chao1', 'ACE', 'Shannon', 'Simpson', 'InvSimpson', 'Fisher'")
  }

  # ------------#

  # Convert group_color (character) to symbol.
  group_color_sym <- rlang::sym(group_color)

  # Get sample data.
  sample_df <- as.data.frame(phyloseq::sample_data(physeq))

  # Get the color number.
  group_color_num <- length(unique(dplyr::pull(sample_df,
                                               !!group_color_sym)))

  # Create plot.
  plot <- phyloseq::plot_richness(physeq = physeq,
                                  x = group_x,
                                  color = group_color,
                                  measures = measures)

  plot <- plot +
    ggplot2::geom_boxplot(data = plot$data,
                          mapping = ggplot2::aes(color = NULL),
                          fill = "white",
                          alpha = 1,
                          show.legend = FALSE) +
    ggplot2::geom_point(size = 2,
                        alpha = 0.8) +
    ggplot2::theme_classic() +
    ggplot2::scale_fill_manual(values = fetch_color(group_color_num)) +
    ggplot2::scale_color_manual(values = fetch_color(group_color_num)) +
    ggplot2::theme(axis.title.x = ggplot2::element_text(face = "bold",
                                                        vjust = -1),
                   axis.title.y = ggplot2::element_text(face = "bold",
                                                        vjust = 3),
                   panel.background = ggplot2::element_rect(fill = "#F7F7F7",
                                                            color = NA),
                   panel.border = ggplot2::element_rect(color = "black",
                                                        size = 1,
                                                        fill = NA),
                   legend.title = ggplot2::element_text(face = "bold"),
                   strip.background = ggplot2::element_rect(color = "black",
                                                                     fill = "black",
                                                                     size = 1.5,
                                                                     linetype = "solid"),
                   strip.text.x = ggplot2::element_text(color = "white",
                                                        face = "bold"))
  return(plot)
}

#' @title Visualize NMDS
#' @description Plot the data in new ordination after dimensionality reductionnmds using Non-metric MultiDimensional Scaling (NMDS) based on the Bray-Curtis distance measure.
#'
#' @param physeq a phyloseq object.
#' @param convert_to_rel convert counts to relative abundances. Default is TRUE.
#' @param group_color the parameter in metadata to color by.
#' @param group_shape the parameter in metadata to shape by.
#' @param encircle encircle the points belonging to same group (as specified by group_color). Default is FALSE.
#' @param fill_circle fill the encircled area. Default is FALSE.
#' @param scale_circle value to scale the encircle area. Default is 0.1.
#' @param scale_plot value to scale the plot area. Default is 0.1.
#'
#' @return NMDS plot created with ggplot.
#' @export
#'
#' @examples
#' # Data phyloseq object:
#' data(qaanaaq_rRNA)
#' phylo <- qaanaaq_rRNA
#'
#' vis_nmds <- function(physeq = phylo,
#'                      group_color = "Location",
#'                      group_shape = "Transect",
#'                      scale_circle = 0.5,
#'                      scale_plot = 0.5)
#'
vis_nmds <- function(physeq,
                     convert_to_rel = TRUE,
                     group_color,
                     group_shape,
                     encircle = FALSE,
                     fill_circle = FALSE,
                     scale_circle = 0.1,
                     scale_plot = 0.1){

  # ------------#
  # Check inputs
  # ------------#

  if (class(physeq)[1] != "phyloseq") {
    stop("`physeq` must be a phyloseq object")
  }

  if (!is.character(group_color)){
    stop("`group_color` must be character")
  }

  if (!is.character(group_shape)){
    stop("`group_shape` must be character")
  }

  if (!is.numeric(scale_circle)){
    stop("`scale_circle` must be numeric")
  }

  if (!is.numeric(scale_plot)){
    stop("`scale_plot` must be numeric")
  }

  if (!is.logical(convert_to_rel)) {
    stop("`convert_to_rel` must be logical")
  }

  if (!is.logical(encircle)) {
    stop("`encircle` must be logical")
  }

  if (!is.logical(fill_circle)) {
    stop("`fill_circle` must be logical")
  }

  if (!(group_color %in%  colnames(sample_data(physeq)))) {
    stop(paste(group_color, "is not found in sample data"))
  }

  if (!(group_shape %in%  colnames(sample_data(physeq)))) {
    stop(paste(group_shape, "is not found in sample data"))
  }

  # ------------#

  # Convert group_color and group_shape (characters) to symbols.
  group_color_sym <- rlang::sym(group_color)
  group_shape_sym <- rlang::sym(group_shape)

  # Normalize.
  if (convert_to_rel == TRUE){
    physeq_rel <- physeq |>
      phyloseq::transform_sample_counts(function(x) x/sum(x)*100)
  } else {
    physeq_rel <- physeq
  }

  # Redirect output to null to suppress it (Mac version)
  sink("/dev/null")  # Redirects output to null (use "/dev/null" on macOS)
  on.exit(sink())  # Ensure that it gets reset after function execution

  # Ordinate.
  physeq_nmds <- invisible(phyloseq::ordinate(physeq_rel,
                                              method = "NMDS",
                                              distance = "bray"))

  # Get values.
  nmds_df <- data.frame(phyloseq::sample_data(physeq_rel),
                        "NMDS1" = physeq_nmds$points[, 1],
                        "NMDS2" = physeq_nmds$points[, 2])

  # Get the color and shape number.
  group_color_num <- length(unique(dplyr::pull(nmds_df, !!group_color_sym)))
  group_shape_num <- length(unique(dplyr::pull(nmds_df, !!group_shape_sym)))

  plot <- ggplot2::ggplot(data = nmds_df,
                          mapping = ggplot2::aes(x = NMDS1,
                                                 y = NMDS2,
                                                 color = !!group_color_sym,
                                                 shape = !!group_shape_sym))

  if (encircle == TRUE){
    x_range <- c(min(nmds_df$NMDS1)*(1+scale_plot), max(nmds_df$NMDS1)*(1+scale_plot))
    y_range <- c(min(nmds_df$NMDS2)*(1+scale_plot), max(nmds_df$NMDS2)*(1+scale_plot))
    plot <- plot +
      ggplot2::coord_cartesian(xlim = x_range,
                               ylim = y_range,
                               expand = TRUE)
    if (fill_circle == TRUE){
      plot <- plot +
        ggalt::geom_encircle(mapping = ggplot2::aes(group = !!group_color_sym,
                                                      fill = !!group_color_sym),
                             size = 2,
                             alpha = 0.2,
                             show.legend = FALSE,
                             expand = scale_circle) +
        ggplot2::expand_limits(x = x_range, y = y_range)
    } else {
      plot <- plot +
        ggalt::geom_encircle(mapping = ggplot2::aes(group = !!group_color_sym),
                             size = 2,
                             alpha = 0.5,
                             show.legend = FALSE,
                             expand = scale_circle) +
        ggplot2::expand_limits(x = x_range, y = y_range)
    }
  }

  plot <- plot + ggplot2::geom_point(mapping = ggplot2::aes(fill = !!group_color_sym),
                                     size = 4) +
    ggplot2::theme_classic() +
    ggplot2::xlab("NMDS1") +
    ggplot2::ylab("NMDS2") +
    ggplot2::theme(axis.title.x = ggplot2::element_text(face = "bold",
                                                        vjust = -1),
                   axis.title.y = ggplot2::element_text(face = "bold",
                                                        vjust = 3),
                   panel.background = ggplot2::element_rect(fill = "#F7F7F7", color = NA),
                   legend.title = ggplot2::element_text(face = "bold")) +
    ggplot2::scale_fill_manual(values = fetch_color(group_color_num)) +
    ggplot2::scale_color_manual(values = fetch_color(group_color_num)) +
    ggplot2::scale_shape_manual(values = fetch_shape(group_shape_num)) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 15)))

  return(plot)
}

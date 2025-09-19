#' @title Visualize NMDS
#' @description Plot the data in new ordination after dimensionality reduction using Non-metric MultiDimensional Scaling (NMDS) based on the Bray-Curtis distance measure.
#'
#' @param physeq a phyloseq object.
#' @param group_color the parameter in metadata to color by.
#' @param group_shape the parameter in metadata to shape by.
#' @param group_circle the parameter in metadata to encircle (if encircle = TRUE). If not specified, the encircled groups will be similar to colored groups.
#' @param env_factors vector with enviromental factors in the metadata to fit onto the ordination. See vegan::envfit() for details. Black arrows show significant fit.
#' @param env_labels vector with labels for the enviromental factors in env_factors. If not applied, the env_factors will be used.
#' @param convert_to_rel convert counts to relative abundances. Default is TRUE.
#' @param encircle encircle the points belonging to same group (as specified by group_color). Default is FALSE.
#' @param fill_circle fill the encircled area. Default is FALSE.
#' @param smooth_circle value describing the degree of smoothing of the polygon. Larger values result in sharper encirclement. Default is 0.
#' @param scale_circle value to scale the encircle area. Default is 0.
#' @param circle_edge_size size of the circle edge. Default is 0.5.
#' @param scale_plot value to scale the plot area. Default is 0.
#' @param set_alpha value to control the degree of transparency of the data points. Default is 0.8.
#' @param arrow_text_offset parameter controlling the distance from text to arrow. Default is 0.01.
#' @param arrow_text_size size of the enviromental factors text. Default is 2.
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
#'          group_shape = "Transect",
#'          encircle = TRUE,
#'          scale_circle = 0.2,
#'          scale_plot = 0.4)
#'
vis_nmds <- function(physeq,
                     group_color,
                     group_shape = group_color,
                     group_circle = group_color,
                     env_factors = NULL,
                     env_labels = NULL,
                     convert_to_rel = TRUE,
                     encircle = FALSE,
                     fill_circle = FALSE,
                     smooth_circle = 0,
                     scale_circle = 0,
                     circle_edge_size = 0.5,
                     scale_plot = 0,
                     set_alpha = 0.8,
                     arrow_text_offset = 0.01,
                     arrow_text_size = 2){

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

  if (!is.null(env_factors) && !is.character(env_factors)){
    stop("`env_factors` must be character")
  }

  if (!is.character(group_circle)){
    stop("`group_circle` must be character")
  }

  if (!is.numeric(smooth_circle)){
    stop("`smooth_circle` must be numeric")
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

  if (!(group_color %in%  colnames(phyloseq::sample_data(physeq)))) {
    stop(paste(group_color, "is not found in sample data"))
  }

  if (!(group_shape %in%  colnames(phyloseq::sample_data(physeq)))) {
    stop(paste(group_shape, "is not found in sample data"))
  }

  # ------------#

  # Convert group_color and group_shape (characters) to symbols
  group_color_sym  <- rlang::sym(group_color)
  group_shape_sym  <- rlang::sym(group_shape)
  group_circle_sym <- rlang::sym(group_circle)

  # Normalize
  if (convert_to_rel == TRUE){
    physeq_rel <- physeq |>
      phyloseq::transform_sample_counts(function(x) x/sum(x)*100)
  } else {
    physeq_rel <- physeq
  }

  otu_mat <- as(phyloseq::otu_table(physeq_rel), "matrix")

  # Samples as rows
  if(phyloseq::taxa_are_rows(physeq_rel)) {
    otu_mat <- t(otu_mat)
  }

  # Compute Bray-Curtis distance
  otu_dist <- vegan::vegdist(otu_mat, method = "bray")

  # Run NMDS
  nmds <- vegan::metaMDS(otu_dist,
                         k = 2,
                         trymax = 20,
                         autotransform = FALSE,
                         trace = FALSE)

  # Extract NMDS scores
  nmds_scores <- vegan::scores(nmds) |>
    as.data.frame()

  # Get values
  nmds_df <- data.frame(phyloseq::sample_data(physeq_rel),
                        "NMDS1" = nmds_scores$NMDS1,
                        "NMDS2" = nmds_scores$NMDS2)

  group_color_col <- dplyr::pull(nmds_df,
                                 !!group_color_sym)

  group_shape_col <- dplyr::pull(nmds_df,
                                 !!group_shape_sym)

  # Get the color and shape number
  if(is.factor(group_color_col)){
    group_color_num <- length(levels(group_color_col))
  } else {
    group_color_num <- length(unique(group_color_col))
  }

  if(is.factor(group_shape_col)){
    group_shape_num <- length(levels(group_shape_col))
  } else {
    group_shape_num <- length(unique(group_shape_col))
  }

  # Make the plot
  plot <- ggplot2::ggplot(data = nmds_df,
                          mapping = ggplot2::aes(x = NMDS1,
                                                 y = NMDS2,
                                                 color = !!group_color_sym,
                                                 shape = !!group_shape_sym))

  # Add circles
  if (encircle == TRUE) {

    # Compute the convex hull for each group
    convex_hull <- nmds_df |>
      dplyr::rename(x = NMDS1, y = NMDS2) |>
      dplyr::group_by(!!group_circle_sym) |>
      tidyr::nest() |>
      dplyr::mutate(hull = purrr::map(data, ~ with(.x, grDevices::chull(x, y))),
                    out = purrr::map2(data, hull, ~ .x[.y,,drop=FALSE])) |>
      dplyr::select(-data) |>
      tidyr::unnest(cols = out)

    if (fill_circle == TRUE) {
      if (group_circle == group_color) {
        plot <- plot +
          ggforce::geom_shape(convex_hull,
                              mapping = ggplot2::aes(x = x,
                                                     y = y,
                                                     group = !!group_circle_sym,
                                                     fill  = !!group_circle_sym,
                                                     color = !!group_circle_sym),
                              linewidth = circle_edge_size,
                              alpha = 0.1,
                              expand = scale_circle,
                              radius = smooth_circle,
                              show.legend = FALSE)
      } else {
        plot <- plot +
          ggforce::geom_shape(convex_hull,
                              mapping = ggplot2::aes(x = x,
                                                     y = y,
                                                     group = !!group_circle_sym),
                              color = "black",
                              fill  = "black",
                              linewidth = circle_edge_size,
                              alpha = 0.1,
                              expand = scale_circle,
                              radius = smooth_circle,
                              show.legend = FALSE)
        }
    } else {
      if (group_circle == group_color) {
        plot <- plot +
          ggforce::geom_shape(convex_hull,
                              mapping = ggplot2::aes(x = x,
                                                     y = y,
                                                     group = !!group_circle_sym,
                                                     color = !!group_circle_sym),
                              fill = NA,
                              linewidth = circle_edge_size,
                              alpha = 0.1,
                              expand = scale_circle,
                              radius = smooth_circle,
                              show.legend = FALSE)
      } else {
        plot <- plot +
          ggforce::geom_shape(convex_hull,
                              mapping = ggplot2::aes(x = x,
                                                     y = y,
                                                     group = !!group_circle_sym),
                              color = "black",
                              fill = NA,
                              linewidth = circle_edge_size,
                              alpha = 0.1,
                              expand = scale_circle,
                              radius = smooth_circle,
                              show.legend = FALSE)
      }
    }
  }

  # Add the points
  plot <- plot +
    ggplot2::geom_point(mapping = ggplot2::aes(fill = !!group_color_sym),
                        size = 4,
                        alpha = set_alpha) +
    ggplot2::xlab("NMDS1") +
    ggplot2::ylab("NMDS2") +
    ggplot2::scale_fill_manual(values = fetch_color(group_color_num),
                               drop = FALSE) +
    ggplot2::scale_color_manual(values = fetch_color(group_color_num),
                                drop = FALSE) +
    ggplot2::scale_shape_manual(values = fetch_shape(group_shape_num),
                                drop = FALSE) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 15))) +
    theme_ENVS()

  # Fits environmental factors onto the NMDS
  if (!is.null(env_factors)){

    factors <- phyloseq::sample_data(physeq) |>
      data.frame() |>
      dplyr::select(all_of(env_factors))

    fit <- vegan::envfit(nmds,
                         factors,
                         perm = 999)

    arrows <- vegan::scores(fit,
                            display = "vectors") |>
      data.frame()
    arrows$factor <- rownames(arrows)

    if (!is.null(env_labels)){
      arrows$factor_label <- env_labels
    } else {
      arrows$factor_label <- env_factors
    }

    # Add p-values
    arrows$pval <- fit$vectors$pvals

    # Set parameters for plotting arrow labels
    arrows <- arrows |>
      mutate(xmid = NMDS1 / 2,
             ymid = NMDS2 / 2,
             angle = atan2(NMDS2, NMDS1) * 180 / pi,
             xtext = xmid - arrow_text_offset * sin(atan2(NMDS2, NMDS1)),
             ytext = ymid + arrow_text_offset * cos(atan2(NMDS2, NMDS1)),
             angle = ifelse(angle < -90 | angle > 90, angle + 180, angle),
             hjust = 0.5)

    # Sort out significant results
    arrows_sig <- subset(arrows, pval < 0.05)

    # Add the arrows to the plot
    plot <- plot +
      ggplot2::geom_segment(data = arrows,
                            mapping = ggplot2::aes(x = 0,
                                                   y = 0,
                                                   xend = NMDS1,
                                                   yend = NMDS2),
                            arrow = arrow(length = unit(0.25, "cm")),
                            color = "#B9BBB6",
                            inherit.aes = FALSE) +
      ggplot2::geom_text(data = arrows,
                         mapping = ggplot2::aes(x = xtext,
                                                y = ytext,
                                                label = factor_label,
                                                angle = angle,
                                                hjust = hjust),
                         color = "#B9BBB6",
                         size = arrow_text_size,
                         inherit.aes = FALSE) +
      ggplot2::geom_segment(data = arrows_sig,
                            mapping = ggplot2::aes(x = 0,
                                                   y = 0,
                                                   xend = NMDS1,
                                                   yend = NMDS2),
                            arrow = arrow(length = unit(0.25, "cm")),
                            color = "black",
                            inherit.aes = FALSE) +
      ggplot2::geom_text(data = arrows_sig,
                         mapping = ggplot2::aes(x = xtext,
                                                y = ytext,
                                                label = factor_label,
                                                angle = angle,
                                                hjust = hjust),
                         color = "black",
                         size = arrow_text_size,
                         inherit.aes = FALSE)
  }

  if (!is.null(env_factors)){
    x_range <- c(min(nmds_df$NMDS1, arrows$NMDS1) * (1 + scale_plot),
                 max(nmds_df$NMDS1, arrows$NMDS1) * (1 + scale_plot))
    y_range <- c(min(nmds_df$NMDS2, arrows$NMDS2) * (1 + scale_plot),
                 max(nmds_df$NMDS2, arrows$NMDS2) * (1 + scale_plot))
  } else {
    x_range <- c(min(nmds_df$NMDS1) * (1 + scale_plot),
                 max(nmds_df$NMDS1) * (1 + scale_plot))
    y_range <- c(min(nmds_df$NMDS2) * (1 + scale_plot),
                 max(nmds_df$NMDS2) * (1 + scale_plot))
  }

  plot <- plot +
    ggplot2::coord_cartesian(xlim = x_range,
                             ylim = y_range,
                             expand = TRUE)

  return(plot)
}

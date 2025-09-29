vis_nmds_with_arrows <- function(physeq,
                                 factors,
                                 factor_labels,
                                 group_color,
                                 group_shape = group_color,
                                 group_circle = group_color,
                                 convert_to_rel = TRUE,
                                 encircle = FALSE,
                                 fill_circle = FALSE,
                                 smooth_circle = 0,
                                 scale_circle = 0,
                                 scale_plot = 0,
                                 set_alpha = 0.5,
                                 size_circle = 0.5,
                                 offset = 0,
                                 text_size = 1){

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

  if (!is.numeric(set_alpha)){
    stop("`set_alpha` must be numeric")
  }

  if (!is.numeric(size_circle)){
    stop("`size_circle` must be numeric")
  }

  if (!is.numeric(offset)){
    stop("`offset` must be numeric")
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

  # Fit other variables to the NMDS scores
  factors <- phyloseq::sample_data(physeq_rel) |>
    data.frame() |>
    dplyr::select(all_of(factors))

  fit <- vegan::envfit(nmds, factors, perm = 999)

  arrows <- vegan::scores(fit, display = "vectors") |>
    data.frame()
  arrows$factor <- rownames(arrows)
  arrows$factor_label <- factor_labels

  # Add p-values
  arrows$pval <- fit$vectors$pvals

  # Set parameters for plotting arrow labels
  arrows <- arrows |>
    mutate(xmid = NMDS1 / 2,
           ymid = NMDS2 / 2,
           angle = atan2(NMDS2, NMDS1) * 180 / pi,
           xtext = xmid - offset * sin(atan2(NMDS2, NMDS1)),
           ytext = ymid + offset * cos(atan2(NMDS2, NMDS1)),
           angle = ifelse(angle < -90 | angle > 90, angle + 180, angle),
           hjust = 0.5)

  # Keep only significant
  arrows_sig <- subset(arrows, pval < 0.05)

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
                                                 y = NMDS2))

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
                              size = size_circle,
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
                              size = size_circle,
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
                              size = size_circle,
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
                              size = size_circle,
                              alpha = 0.1,
                              expand = scale_circle,
                              radius = smooth_circle,
                              show.legend = FALSE)
      }
    }
  }

  plot <- plot +
    ggplot2::geom_point(mapping = ggplot2::aes(color = !!group_color_sym,
                                               fill = !!group_color_sym,
                                               shape = !!group_shape_sym),
                        size = 4,
                        alpha = set_alpha) +
    ggplot2::geom_segment(data = arrows,
                          mapping = ggplot2::aes(x = 0,
                                                 y = 0,
                                                 xend = NMDS1,
                                                 yend = NMDS2),
                          arrow = arrow(length = unit(0.25, "cm")),
                          color = "#B9BBB6") +
    ggplot2::geom_text(data = arrows,
                       mapping = ggplot2::aes(x = xtext,
                                              y = ytext,
                                              label = factor_label,
                                              angle = angle,
                                              hjust = hjust),
                       color = "#B9BBB6",
                       size = text_size) +
    ggplot2::geom_segment(data = arrows_sig,
                          mapping = ggplot2::aes(x = 0,
                                                 y = 0,
                                                 xend = NMDS1,
                                                 yend = NMDS2),
                          arrow = arrow(length = unit(0.25, "cm")),
                          color = "black") +
    ggplot2::geom_text(data = arrows_sig,
                       mapping = ggplot2::aes(x = xtext,
                                              y = ytext,
                                              label = factor_label,
                                              angle = angle,
                                              hjust = hjust),
                       color = "black",
                       size = text_size)
    ggplot2::theme_classic() +
    ggplot2::xlab("NMDS1") +
    ggplot2::ylab("NMDS2") +
    ggplot2::theme(axis.title.x = ggplot2::element_text(face = "bold",
                                                        vjust = -1),
                   axis.title.y = ggplot2::element_text(face = "bold",
                                                        vjust = 3),
                   panel.background = ggplot2::element_rect(fill = "#F7F7F7",
                                                            color = NA),
                   legend.title = ggplot2::element_text(face = "bold")) +
    ggplot2::scale_fill_manual(values = fetch_color(group_color_num),
                               drop = FALSE) +
    ggplot2::scale_color_manual(values = fetch_color(group_color_num),
                                drop = FALSE) +
    ggplot2::scale_shape_manual(values = fetch_shape(group_shape_num),
                                drop = FALSE) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 15)))

  # Expand the plot
  x_range <- c(min(nmds_df$NMDS1, arrows_sig$NMDS1) * (1 + scale_plot),
               max(nmds_df$NMDS1, arrows_sig$NMDS1) * (1 + scale_plot))
  y_range <- c(min(nmds_df$NMDS2, arrows_sig$NMDS2) * (1 + scale_plot),
               max(nmds_df$NMDS2, arrows_sig$NMDS2) * (1 + scale_plot))

  plot <- plot +
    ggplot2::coord_cartesian(xlim = x_range,
                             ylim = y_range,
                             expand = TRUE)

  return(plot)
}

#' Plot the data i the new ordination after data reduction using NMDS
#'
#' @param physeq a phyloseq object.
#' @param convert_to_rel convert counts to relative abundances. Default is TRUE.
#' @param group_color the parameter in metadata to color by.
#' @param group_shape the parameter in metadata to shape by.
#' @param encircle encircle the points belonging to same group (as specified by group_color). Default is FALSE.
#' @param fill_circle fill the encircled area. Default is FALSE.
#'
#' @return NMDS plot created with ggplot.
#' @export
#'
#' @examples
vis_nmds <- function(physeq,
                     convert_to_rel = TRUE,
                     group_color,
                     group_shape,
                     encircle = FALSE,
                     fill_circle = FALSE){

  # Normalize.
  if (convert_to_rel == TRUE){
    physeq_rel <- physeq |>
      transform_sample_counts(function(x) x/sum(x)*100)
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
  nmds_df <- data.frame(sample_data(physeq_rel),
                        "NMDS1" = physeq_nmds$points[, 1],
                        "NMDS2" = physeq_nmds$points[, 2])

  # Get the color and shape number.
  group_color_num <- pull(nmds_df, {{group_color}})
  group_color_num <- length(unique(group_color_num))
  group_shape_num <- pull(nmds_df, {{group_shape}})
  group_shape_num <- length(unique(group_shape_num))

  # Create plot.
  plot <- ggplot(data = nmds_df,
                 mapping = aes(x = NMDS1,
                               y = NMDS2,
                               color = {{group_color}},
                               shape = {{group_shape}}))

  if (encircle == TRUE){
    x_range <- range(nmds_df$NMDS1) + c(-0.1, 0.1)
    y_range <- range(nmds_df$NMDS2) + c(-0.1, 0.1)
    if (fill_circle == TRUE){
      plot <- plot +
        geom_encircle(aes(group = {{group_color}},
                          fill = {{group_color}}),
                      size = 2,
                      alpha = 0.2,
                      show.legend = FALSE) +
        expand_limits(x = x_range, y = y_range)
    } else {
      plot <- plot +
        geom_encircle(aes(group = {{group_color}}),
                      size = 2,
                      alpha = 0.5,
                      show.legend = FALSE) +
        expand_limits(x = x_range, y = y_range)
    }
  }

  plot <- plot + geom_point(mapping = aes(fill = {{group_color}}),
                            size = 4) +
    theme_classic() +
    xlab("NMDS1") +
    ylab("NMDS2") +
    theme(axis.title.x = element_text(face = "bold",
                                      vjust = -1),
          axis.title.y = element_text(face = "bold",
                                      vjust = 3),
          panel.background = element_rect(fill = "#F7F7F7", color = NA),
          legend.title = element_text(face = "bold")) +
    scale_fill_manual(values = fetch_color("main1", group_color_num)) +
    scale_color_manual(values = fetch_color("main1", group_color_num)) +
    scale_shape_manual(values = fetch_shape(group_shape_num)) +
    guides(color = guide_legend(override.aes = list(shape = 15)))

  if (group_shape_num <= 5){
    plot <- plot +
      scale_color_manual(values = fetch_color("main2", group_color_num)) +
      guides(color = guide_legend(override.aes = list(shape = 22)))
  }

  return(plot)
}

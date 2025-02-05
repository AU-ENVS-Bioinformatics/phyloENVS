#' Plot the data i the new ordination after data reduction using NMDS
#'
#' @param physeq_rel a phyloseq object
#' @param group_color the parameter in metadata to color by.
#' @param group_shape the parameter in metadata to shape by.
#'
#' @return
#' @export
#'
#' @examples
#' vis_nmds(physeq_rel, group_color, group_shape)
vis_nmds <- function(physeq_rel, group_color, group_shape){

  physeq_nmds <- invisible(ordinate(physeq_rel,
                                    method = "NMDS",
                                    distance = "bray"))

  nmds_df <- data.frame(sample_data(physeq_rel),
                        "NMDS1" = physeq_nmds$points[, 1],
                        "NMDS2" = physeq_nmds$points[, 2])

  ggplot(data = nmds_df,
         mapping = aes(x = NMDS1,
                       y = NMDS2,
                       color = {{group_color}},
                       shape = {{group_shape}})) +
    theme_classic() +
    geom_point(size = 4) +
    xlab("NMDS1") +
    ylab("NMDS2") +
    scale_color_viridis(option = "D", discrete = TRUE) +
    scale_shape_manual(values = c(15, 16, 17, 18, 14, 8, 7, 6, 12)) +
    guides(fill = guide_legend(override.aes = list(shape = 21))) +
    theme(axis.title.x = element_text(face = "bold",
                                      vjust = -1),
          axis.title.y = element_text(face = "bold",
                                      vjust = 3),
          panel.background = element_rect(fill = "#F7F7F7", color = NA))


  #plot_ordination(x, y, ...) +
    #geom_point(size = 4, aes_string(shape = facet_col, color = xaxis_col)) +  # Increase point size
    #scale_color_viridis(option = "D", discrete = TRUE) +  # Use viridis color palette https://ggplot2.tidyverse.org/reference/scale_viridis.html
    #"magma" (or "A") - "inferno" (or "B") - "plasma" (or "C") - "viridis" (or "D") - "cividis" (or "E") - "rocket" (or "F") - "mako" (or "G") - "turbo" (or "H")
    #scale_shape_manual(values = c(15, 16, 17, 18, 14, 8, 7, 6, 12)) +  # Customize shapes https://ggplot2-book.org/scales-other#sec-scale-shape
    #theme(legend.title = element_blank(),
          #legend.position = "right",
          #text = element_text(size = 12),
          #axis.title = element_text(size = 14),
          #axis.text = element_text(size = 12),
          #panel.background = element_rect(fill = "#F7F7F7", color = NA))
}

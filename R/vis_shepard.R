#' @title Visualize Shepard diagram
#' @description Plot the Shepard diagram to asses the goodness of fit for data reduction using NMDS.
#'
#' @param physeq a phyloseq object
#' @param convert_to_rel convert counts to relative abundances. Default is TRUE.
#'
#' @return shepard diagram created with ggplot.
#' @export
#'
#' @examples
#' # Data phyloseq object:
#' data(qaanaaq_rRNA)
#' phylo <- qaanaaq_rRNA
#'
#' vis_shepard(physeq = phylo)
#'
vis_shepard <- function(physeq,
                        convert_to_rel = TRUE){

  # ------------#
  # Check inputs
  # ------------#

  if (class(physeq)[1] != "phyloseq") {
    stop("`physeq` must be a phyloseq object")
  }

  if (!is.logical(convert_to_rel)) {
    stop("`convert_to_rel` must be logical")
  }

  # ------------#

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
  # Get the values.
  shepard_df <- data.frame(dissimilarity = physeq_nmds$diss,
                           distance = physeq_nmds$dist,
                           fit = physeq_nmds$dhat)

  # Get the stats.
  nonmetric_fit_stat <- round(1-physeq_nmds$stress^2, 3)
  linear_fit_stat <- round(cor(physeq_nmds$dhat, physeq_nmds$dist)^2, 3)

  # Specify ranges of the axes.
  x_range <- range(shepard_df$dissimilarity) + c(-0.1, 0.1)
  y_range <- range(shepard_df$distance) + c(-0.1, 0.1)

  # Create plot.
  plot <- ggplot2::ggplot(data = shepard_df,
                          mapping = ggplot2::aes(x = dissimilarity,
                                                 y = distance)) +
    ggplot2::geom_point(color = "#003d73",
                        alpha = 0.6,
                        size = 2) +
    ggplot2::geom_step(mapping = ggplot2::aes(y = fit),
                       direction = "vh",
                       linewidth = 0.8) +
    ggplot2::geom_label(label = paste("Non-metric fit:",
                                      nonmetric_fit_stat,
                                      "\n",
                                      "Linear fit:",
                                      linear_fit_stat),
                        x = min(shepard_df$dissimilarity),
                        y = max(shepard_df$distance),
                        label.padding = ggplot2::unit(0.55, "lines"),
                        label.size = 0.35,
                        color = "#003d73",
                        fill="white",
                        fontface = "bold",
                        hjust = 0,
                        vjust = 1) +
    ggplot2::labs(x = "Observed Dissimilarity",
                  y = "Ordination Distance") +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.title.x = ggplot2::element_text(face = "bold",
                                                        vjust = -1),
                   axis.title.y = ggplot2::element_text(face = "bold",
                                                        vjust = 3),
                   panel.background = ggplot2::element_rect(fill = "#F7F7F7",
                                                            color = NA),
                   panel.grid.major = ggplot2::element_line(color = "white",
                                                            size = 1),
                   panel.grid.minor = ggplot2::element_line(color = "white",
                                                            size = 0.5))

  return(plot)
}

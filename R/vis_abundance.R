#' @title Visualize abundance
#' @description Plot the relative abundances for merged species of the same taxonomic rank.
#'
#' @param physeq a phyloseq object.
#' @param group_x the x axis sample variable to plot.
#' @param group_split name of the sample data column to group by.
#' @param level_glom name of the level to agglomerate counts. Default is "Phylum".
#' @param level_select specified level to target for specified group (e.g., domain level - useful with add_level()). Default is NULL.
#' @param group_select specified group to target in specified level (e.g., Prokaryotes in the domain level). Default is NULL.
#' @param lower_limit relative abundance threshold in percentages for visualizing the abundance. Abundances less than the threshold is pooled together.
#' @param color_source name of color source to use. Default is "AU2". Supported: "AU1", "AU2".
#' @param remove_grid remove the grid splitting groups. Default is FALSE.
#'
#' @return a abundance plot created with ggplot.
#' @export
#'
#' @examples
vis_abundance <- function(physeq,
                          group_x,
                          group_split,
                          level_glom = Phylum,
                          level_select = NULL,
                          group_select = NULL,
                          lower_limit = 2,
                          color_source = "AU2",
                          remove_grid = FALSE){

  # Convert character to symbol.
  group_x_sym <- rlang::sym(group_x)
  group_split_sym <- rlang::sym(group_split)
  level_glom_sym <- rlang::sym(level_glom)

  # Agglomerate counts.
  physeq_df <- physeq |>
    phyloseq::tax_glom(taxrank = level_glom,
                       NArm = FALSE) |>
    phyloseq::transform_sample_counts(function(x) x/sum(x)*100) |>
    phyloseq::psmelt() |>
    dplyr::mutate(!!level_glom := dplyr::na_if(!!level_glom_sym, ""),
                  !!level_glom := tidyr::replace_na(!!level_glom_sym, "Unassigned"),
                  !!level_glom := ifelse(Abundance < lower_limit,
                                         paste("< ", lower_limit, "%", sep = ""),
                                         !!level_glom_sym),
                  !!level_glom := reorder(!!level_glom_sym, Abundance),
                  !!level_glom := factor(!!level_glom_sym),
                  !!group_split := factor(!!group_split_sym))

  # Subset data if specified by user.
  if (!is.null(level_select) && !is.null(level_select)){
    level_select_sym <- rlang::sym(level_select)
    physeq_df <- physeq_df |>
      dplyr::filter(!!level_select_sym %in% group_select)
  }

  # Get number of unique colors to use.
  group_unique <- unique(dplyr::pull(physeq_df, !!level_glom_sym))
  group_color_num <- length(group_unique)

  # Specify the color of the low detection and unassigned groups.
  if ("Unassigned" %in% group_unique){
    physeq_df <- physeq_df |>
      dplyr::mutate(!!level_glom := forcats::fct_relevel(!!level_glom_sym, "Unassigned", after = 1))
    group_color <- c("#878787", "#4b4b4a", rev(fetch_color(group_color_num-2, color_source)))
  } else if ("Unknown function" %in% group_unique){
    physeq_df <- physeq_df |>
      dplyr::mutate(!!level_glom := forcats::fct_relevel(!!level_glom_sym, "Unknown function", after = 1))
    group_color <- c("#878787", "#4b4b4a", rev(fetch_color(group_color_num-2, color_source)))
  } else if ("Others" %in% group_unique){
    physeq_df <- physeq_df |>
      dplyr::mutate(!!level_glom := forcats::fct_relevel(!!level_glom_sym, "Others", after = 1))
    group_color <- c("#878787", "#4b4b4a", rev(fetch_color(group_color_num-2, color_source)))
  } else if (paste("< ", lower_limit, "%", sep = "") %in% group_unique){
    group_color <- c("#878787", rev(fetch_color(group_color_num-1, color_source)))
  } else {
    group_color <- rev(fetch_color(group_color_num, color_source))
  }

  # Create plot.
  plot <- ggplot2::ggplot(data = physeq_df,
                          mapping = ggplot2::aes(x = !!group_x_sym,
                                                 y = Abundance,
                                                 fill = !!level_glom_sym)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(x = "",
                  y = "Relative abundance [%]") +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
                   axis.title.x = ggplot2::element_text(face = "bold", vjust = -1),
                   axis.title.y = ggplot2::element_text(face = "bold", vjust = 3),
                   panel.background = ggplot2::element_rect(fill = "#F7F7F7", color = NA),
                   panel.grid.major.y = ggplot2::element_line(color = "white", size = 1),
                   panel.grid.minor.y = ggplot2::element_line(color = "white", size = 0.5),
                   panel.border = ggplot2::element_rect(color = "black", size = 1, fill = NA),
                   legend.title = ggplot2::element_text(face = "bold"),
                   strip.background = ggplot2::element_rect(color="black", fill="black", size=1.5, linetype="solid"),
                   strip.text.x = ggplot2::element_text(color = "white", face = "bold"),
                   strip.text.y = ggplot2::element_text(color = "white", face = "bold")) +
    ggplot2::scale_fill_manual(values = group_color)

  # Add the grid to distinguish between groups.
  if (!is.null(level_select) && !is.null(level_select)){
    if(level_glom == level_select){
      plot <- plot + ggplot2::theme(legend.position = "none")
    }
    if (remove_grid == FALSE){
      plot <- plot + ggplot2::facet_grid(cols = ggplot2::vars(!!group_split_sym),
                                         rows = ggplot2::vars(!!level_select_sym),
                                         scales = "free_x",
                                         space = "free_x",
                                         switch = "y")
    } else {
      plot <- plot + ggplot2::facet_wrap(ggplot2::vars(!!level_select_sym),
                                         scales = "free")
      }

  } else {
    if (remove_grid == FALSE){
      plot <- plot + ggplot2::facet_grid(cols = ggplot2::vars(!!group_split_sym),
                                         scales = "free_x",
                                         space = "free_x",
                                         switch = "y")}

  }

  return(plot)
}


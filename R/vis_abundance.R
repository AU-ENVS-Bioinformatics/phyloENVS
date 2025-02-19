#' Plot the relative abundances for merged species of the same taxonomic rank.
#'
#' @param physeq
#' @param group_x
#' @param group_split
#' @param level_glom
#' @param level_select
#' @param group_select
#' @param lower_limit
#'
#' @return
#' @export
#'
#' @examples
vis_abundance <- function(physeq, group_x, group_split, level_glom = Phylum, level_select = NULL, group_select = NULL, lower_limit = 2){

  physeq_df <- physeq |>
    tax_glom(taxrank = as_name(enquo(level_glom)),
             NArm = FALSE) |>
    transform_sample_counts(function(x) x/sum(x)*100) |>
    psmelt() |>
    mutate({{level_glom}} := replace_na(.data[[as_name(enquo(level_glom))]], "Unassigned"),  # Replace NA with "Unassigned"
           {{level_glom}} := ifelse(Abundance < lower_limit,
                                   paste("< ", lower_limit, "%", sep = ""),
                                  .data[[as_name(enquo(level_glom))]]),
           {{level_glom}} := reorder(.data[[as_name(enquo(level_glom))]], Abundance),
           {{level_glom}} := factor({{level_glom}}),
           {{level_glom}} := fct_relevel({{level_glom}}, "Unassigned", after = 1),
           {{group_split}} := factor({{group_split}}))

  if (!is.null(level_select) && !is.null(level_select)){
    physeq_df <- physeq_df |>
      filter(.data[[as_name(sym(level_select))]] %in% group_select)
  }

  group_color_num <- pull(physeq_df, {{level_glom}})
  group_color_num <- length(unique(group_color_num))

  plot <- ggplot(data = physeq_df,
                 mapping = aes(x = {{group_x}},
                               y = Abundance,
                               fill = {{level_glom}})) +
    geom_bar(stat = "identity") +
    theme_bw() +
    labs(y = "Relative abundance [%]",
         x = "") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title.x = element_text(face = "bold",
                                      vjust = -1),
          axis.title.y = element_text(face = "bold",
                                      vjust = 3),
          panel.background = element_rect(fill = "#F7F7F7", color = NA),
          panel.grid.major.y = element_line(color = "white", size = 1),
          panel.grid.minor.y = element_line(color = "white", size = 0.5),
          panel.border = element_rect(color = "black", size = 1, fill = NA),
          legend.title = element_text(face = "bold"),
          strip.background = element_rect(color="black",
                                          fill="black",
                                          size=1.5,
                                          linetype="solid"),
          strip.text.x = element_text(color = "white",
                                      face = "bold")) +
    facet_grid(cols = vars({{group_split}}), scales = "free_x", space = "free_x")
    #scale_fill_manual(values = rev(c(fetch_color("main3", group_color_num-2), "red", "#4b4b4a")))
    #scale_fill_manual(values = c("#878787", "#4b4b4a", fetch_color("main3", group_color_num-1)))

  return(plot)
}

#"#4b4b4a"

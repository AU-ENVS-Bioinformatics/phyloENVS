#' @title Merge Data
#' @description Merge samples based on a sample variable similar to merge_samples.
#'
#'
#' @param physeq a phyloseq object.
#' @param group variable name in the corresponding sample_data of the phyloseq object.
#'
#' @return merged phyloseq object
#' @export
#'
#' @examples
merge_data <- function(physeq, group){

  # Convert character to symbol.
  group_sym <- sym(group)

  # Make sure the group is character in sample data to make merge_samples work correct.
  metadata <- sample_data(physeq) |>
    data.frame() |>
    mutate(!!group := as.character(!!group_sym))

  sample_data(physeq) <- sample_data(metadata)

  # Merge samples.
  physeq <- suppressWarnings(merge_samples(physeq, group = group))

  # Make a customized merged metadata table.
  metadata_dense <- metadata |>
    group_by(!!group_sym) |>
    summarise(across(everything(),
                     ~if(is.numeric(.)) mean(., na.rm = TRUE) else unique(na.omit(.))[1])) |>
    ungroup()

  # Update the metadata for merged samples.
  new_sample_data <- sample_data(physeq) |>
    data.frame() |>
    mutate(!!group := as.character(!!group_sym)) |>
    select(!!group) |>
    left_join(metadata_dense, by = group) |>
    mutate(row_id = !!group_sym) |>
    tibble::column_to_rownames(var = "row_id")

  sample_data(physeq) <- sample_data(new_sample_data)

  return(physeq)
}

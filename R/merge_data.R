#' @title Merge Data
#' @description Merge samples based on a sample variable similar to merge_samples. For numeric sample data features, values are averaged. Otherwise, the first class among unique classes are selected.
#'
#' @param physeq a phyloseq object.
#' @param group variable name in the corresponding sample_data of the phyloseq object.
#' @param func function used to merge the values. Can be either the mean or the median. Default is "mean".
#'
#' @return merged phyloseq object.
#' @export
#'
#' @examples
#' # Data phyloseq object:
#' data(genepease_rRNA)
#' phylo <- genepease_rRNA
#'
#' merge_data(physeq = phylo,
#'            group = "Group_ID")
#'
merge_data <- function(physeq,
                       group,
                       func = "mean"){

  # ------------#
  # Check inputs
  # ------------#

  if (class(physeq)[1] != "phyloseq") {
    stop("`physeq` must be a phyloseq object")
  }

  if (!is.character(group)){
    stop("`group` must be character")
  }

  if (!(group %in%  colnames(phyloseq::sample_data(physeq)))) {
    stop(paste(group, "is not found in sample data"))
  }

  if (!is.character(func)){
    stop("`func` must be character")
  }

  if (!(func %in% c("mean", "median"))) {
    stop(paste(func, "is not a possible merging function"))
  }

  # ------------#

  # Convert character to symbol.
  group_sym <- rlang::sym(group)

  # Make sure the group is character in sample data to make merge_samples work correct.
  metadata <- phyloseq::sample_data(physeq) |>
    data.frame() |>
    dplyr::mutate(!!group := as.character(!!group_sym))

  phyloseq::sample_data(physeq) <- phyloseq::sample_data(metadata)

  # Merge samples.
  if (func == "mean"){
    physeq <- suppressWarnings(phyloseq::merge_samples(physeq,
                                                       group = group,
                                                       fun = mean))
  } else if (func == "median") {
    physeq <- suppressWarnings(phyloseq::merge_samples(physeq,
                                                       group = group,
                                                       fun = median))
  }

  # Make a customized merged metadata table.
  metadata_dense <- metadata |>
    dplyr::group_by(!!group_sym) |>
    dplyr::summarise(across(
      where(~ {
        is.numeric(.) || is.character(.) || is.factor(.)
      }),
      ~ {
        if (is.numeric(.)) {
          mean(., na.rm = TRUE)
        } else if (length(na.omit(unique(.))) == 1) {
          na.omit(unique(.))[1]
        } else {
          NA
        }
      }
    )) |>
    dplyr::ungroup()

  # Update the metadata for merged samples.
  new_sample_data <- phyloseq::sample_data(physeq) |>
    data.frame() |>
    dplyr::select(-!!group) |>
    tibble::rownames_to_column(var = group) |>
    dplyr::select(!!group) |>
    dplyr::left_join(metadata_dense, by = group) |>
    dplyr::mutate(row_id = !!group_sym) |>
    tibble::column_to_rownames(var = "row_id")

  phyloseq::sample_data(physeq) <- phyloseq::sample_data(new_sample_data)

  return(physeq)
}

#' @title Convert data to phyloseq object
#' @description Creates a phyloseq object directly from read counts including taxonomy table and associated metadata file.
#'
#' @param totalRNA_file the data path to the read counts including SILVA taxonomy (xlsx).
#' @param meta_file the data path to the metadata for the samples (xlsx).
#' @param sample_names name of the column in the meta file to identify samples between the OTUs and metadata.
#'
#' @return a phyloseq object
#' @export
#'
data_to_phyloseq <- function(totalRNA_file, meta_file, sample_names){

  # Total RNA-Seq read counts are loaded.
  totalrna <- readxl::read_xlsx(totalRNA_file)

  # SILVA ranks.
  silva_ranks <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Specie")

  # The taxonomy is extracted and removed from the read counts.
  tax <- totalrna |>
    dplyr::select(ContigID, taxonomy) |>
    tidyr::separate_wider_delim(cols = taxonomy,
                                delim = ";",
                                names = silva_ranks) |>
    dplyr::mutate(across(Kingdom:Specie, ~ stringr::str_remove(.,
                                                              pattern = "\\w{1}\\_{2}")),
                  across(Kingdom:Specie, ~ stringr::str_trim(.)),
                  across(Kingdom:Specie, ~ dplyr::na_if(., ""))) |>
    dplyr::rename("Contig ID" = ContigID) |>
    tibble::column_to_rownames(var = "Contig ID") |>
    as.matrix() |>
    phyloseq::tax_table()

  otu <- totalrna |>
    dplyr::select(!taxonomy) |>
    dplyr::rename("Contig ID" = ContigID) |>
    tibble::column_to_rownames(var = "Contig ID") |>
    phyloseq::otu_table(taxa_are_rows = TRUE)

  # The meta data is loaded.
  meta <- readxl::read_xlsx(meta_file) |>
    tibble::column_to_rownames(var = sample_names) |>
    phyloseq::sample_data()

  if (ncol(otu) != nrow(meta)){
    stop("Number of columns in OTU table does not correspond to the number of rows in metadata.")
  }

  if (nrow(otu) != nrow(tax)){
    stop("Number of rows in OTU table does not correspond to the number of rows in the taxonomy table.")
  }

  physeq <- phyloseq::phyloseq(otu, tax, meta)

  return(physeq)
}

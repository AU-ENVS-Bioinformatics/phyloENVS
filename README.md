
<!-- README.md is generated from README.Rmd. Please edit that file -->

<div align="right">

<img src="https://github.com/johanneBiO/phyloENVS/blob/main/images/au.png" width="2000"/>

</div>

# phyloENVS

<!-- badges: start -->
<!-- badges: end -->

The goal of phyloENVS is to provide tailored tools for visualizing and
analyzing microbial community data, with a focus on rRNA and mRNA
datasets from environmental samples such as soil, ice, and other
ecosystems. Built to enhance the functionality of the phyloseq package,
it offers customizable and publication-quality visualizations, including
ordination, abundance and richness plots for community composition
analysis. The package is designed to streamline workflows for
researchers within the Environmental Microbiology section at Department
of Environmental Science (ENVS) at Aarhus University.

## Installation

You can install the phyloENVS from [GitHub](https://github.com/) with
the devtools package.

``` r
# install.packages("devtools")
devtools::install_github("johanneBiO/phyloENVS", build_vignettes = TRUE)
```

## Data Structure

All functions within phyloENVS builds upon the phyloseq package for
microbial community analysis. The phyloseq data structure in R organizes
multi-table datasets into a single coherent object. The components of a
phyloseq object are listed below and visualized below.

- OTU Table (otu_table): Abundance data of taxa (read counts) across
  samples.

- Taxonomy Table (tax_table): Taxonomic classification (Kingdom to
  Species) of each contig.

- Sample Data (sample_data): Metadata for each sample (e.g., location,
  timepoint, concentration, etc.).

<div align="center">

<img src="https://github.com/johanneBiO/phyloENVS/blob/main/images/phyloseq_data_structure.png" width="2000"/>

</div>

## Example

This is a basic example which shows how you can visualize the relative
abundance for a microbial community. The data used comes from ice
sampled at the Qaanaaq glacier and at the Greenland ice sheet. We subset
the data and only look at transect samples from the Qaanaaq glacier.

``` r
library(phyloENVS)

data("qaanaaq_rRNA")

qaanaaq_rRNA_sub <- subset_samples(qaanaaq_rRNA,
                                   Transect != "Non-transect") 

vis_abundance(physeq = qaanaaq_rRNA_sub,
              group_x = "SampleName",
              group_split = "Wetness",
              level_glom = "Phylum",
              lower_limit = 2)
```

<div class="figure">

<img src="man/figures/README-example1-1.png" alt="Relative abundance plot." width="80%" />
<p class="caption">
Relative abundance plot.
</p>

</div>

NMDS (Non-metric Multidimensional Scaling) is a popular technique used
for visualizing and interpreting the relationships between samples in
high-dimensional datasets. We can look into how different samples
cluster based on the available metadata, e.g., wetness.

``` r
vis_nmds(qaanaaq_rRNA_sub,
         group_color = "Wetness",
         group_shape = "Transect",
         encircle = TRUE,
         fill_circle = TRUE,
         scale_circle = 0.09,
         scale_plot = 0.3)
```

<div class="figure">

<img src="man/figures/README-example2-1.png" alt="NMDS plot." width="80%" />
<p class="caption">
NMDS plot.
</p>

</div>

## Details

The available color scales can be seen below.

<div align="left">

<img src="https://github.com/johanneBiO/phyloENVS/blob/main/images/color_scales.png" width="2000"/>

</div>

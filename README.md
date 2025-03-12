
<!-- README.md is generated from README.Rmd. Please edit that file -->

# phyloENVS

<!-- badges: start -->
<!-- badges: end -->

The goal of phyloENVS is to provide tailored tools for visualizing and
analyzing microbial community data, with a focus on rRNA and mRNA
datasets from environmental samples such as soil, ice, and other
ecosystems. Built to enhance the functionality of the ‘phyloseq’
package, it offers customizable and publication-quality visualizations,
including abundance plots, diversity metrics, and community composition
analyses. The package is designed to streamline workflows for
researchers within Environmental Microbiology at Aarhus University.

## Installation

You can install the phyloENVS from [GitHub](https://github.com/) with
the devtools package.

``` r
# install.packages("devtools")
devtools::install_github("johanneBiO/phyloENVS")
```

## Example

This is a basic example which shows you can visualize the relative
abundance for a microbial community of ice sampled at the Qaanaaq
glacier and the Greenland ice sheet. We subset the data and only look at
transect samples from the Qaanaaq glacier.

``` r
library(phyloENVS)

data("qaanaaq_rRNA")

qaanaaq_rRNA_sub <- subset_samples(qaanaaq_rRNA,
                                   Transect != "Non-transect") 

vis_abundance(physeq = qaanaaq_rRNA_sub,
              group_x = SampleName,
              group_split = Wetness,
              level_glom = Phylum,
              level_select = NULL,
              group_select = NULL,
              lower_limit = 2,
              remove_grid = FALSE)
```

<div class="figure">

<img src="man/figures/README-example1-1.png" alt="Relative abundance plot." width="80%" />
<p class="caption">
Relative abundance plot.
</p>

</div>

NMDS (Non-metric Multidimensional Scaling) plot is a popular technique
used for visualizing and interpreting the relationships between
sampleshigh-dimensional datasets. We can look into how different samples
cluster based on the available metadata, e.g., as location.

    #> Wisconsin double standardization
    #> Run 0 stress 0.07476995 
    #> Run 1 stress 0.143494 
    #> Run 2 stress 0.1441608 
    #> Run 3 stress 0.07476995 
    #> ... New best solution
    #> ... Procrustes: rmse 1.629663e-06  max resid 2.303711e-06 
    #> ... Similar to previous best
    #> Run 4 stress 0.07476995 
    #> ... Procrustes: rmse 3.100334e-06  max resid 4.508387e-06 
    #> ... Similar to previous best
    #> Run 5 stress 0.2200962 
    #> Run 6 stress 0.1434946 
    #> Run 7 stress 0.07476995 
    #> ... New best solution
    #> ... Procrustes: rmse 1.735298e-06  max resid 4.378827e-06 
    #> ... Similar to previous best
    #> Run 8 stress 0.1434948 
    #> Run 9 stress 0.1473152 
    #> Run 10 stress 0.07476995 
    #> ... Procrustes: rmse 1.361745e-06  max resid 2.475677e-06 
    #> ... Similar to previous best
    #> Run 11 stress 0.07476995 
    #> ... Procrustes: rmse 3.997475e-07  max resid 7.118867e-07 
    #> ... Similar to previous best
    #> Run 12 stress 0.07476995 
    #> ... Procrustes: rmse 2.88933e-06  max resid 5.545695e-06 
    #> ... Similar to previous best
    #> Run 13 stress 0.1400042 
    #> Run 14 stress 0.07476995 
    #> ... Procrustes: rmse 3.031947e-06  max resid 7.504358e-06 
    #> ... Similar to previous best
    #> Run 15 stress 0.07476995 
    #> ... Procrustes: rmse 5.866751e-07  max resid 1.277362e-06 
    #> ... Similar to previous best
    #> Run 16 stress 0.07476995 
    #> ... Procrustes: rmse 3.082737e-06  max resid 4.332764e-06 
    #> ... Similar to previous best
    #> Run 17 stress 0.07476995 
    #> ... Procrustes: rmse 3.506248e-06  max resid 6.753072e-06 
    #> ... Similar to previous best
    #> Run 18 stress 0.1434938 
    #> Run 19 stress 0.1441607 
    #> Run 20 stress 0.07476995 
    #> ... Procrustes: rmse 2.11295e-06  max resid 4.315474e-06 
    #> ... Similar to previous best
    #> *** Best solution repeated 9 times
    #> Scale for colour is already present.
    #> Adding another scale for colour, which will replace the existing scale.

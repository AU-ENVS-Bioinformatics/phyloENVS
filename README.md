
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
    #> Run 1 stress 0.07476995 
    #> ... Procrustes: rmse 7.151125e-06  max resid 1.376814e-05 
    #> ... Similar to previous best
    #> Run 2 stress 0.07476995 
    #> ... Procrustes: rmse 5.169914e-06  max resid 1.020543e-05 
    #> ... Similar to previous best
    #> Run 3 stress 0.07476995 
    #> ... New best solution
    #> ... Procrustes: rmse 2.792143e-06  max resid 5.241902e-06 
    #> ... Similar to previous best
    #> Run 4 stress 0.1434942 
    #> Run 5 stress 0.07476995 
    #> ... Procrustes: rmse 2.002331e-06  max resid 4.960036e-06 
    #> ... Similar to previous best
    #> Run 6 stress 0.07476995 
    #> ... Procrustes: rmse 2.415659e-06  max resid 4.924733e-06 
    #> ... Similar to previous best
    #> Run 7 stress 0.07476995 
    #> ... Procrustes: rmse 5.062195e-06  max resid 7.342618e-06 
    #> ... Similar to previous best
    #> Run 8 stress 0.1434941 
    #> Run 9 stress 0.07476995 
    #> ... New best solution
    #> ... Procrustes: rmse 1.850061e-06  max resid 3.577571e-06 
    #> ... Similar to previous best
    #> Run 10 stress 0.280878 
    #> Run 11 stress 0.1441608 
    #> Run 12 stress 0.07476995 
    #> ... Procrustes: rmse 2.688617e-06  max resid 6.736147e-06 
    #> ... Similar to previous best
    #> Run 13 stress 0.07476995 
    #> ... Procrustes: rmse 2.575351e-06  max resid 5.212555e-06 
    #> ... Similar to previous best
    #> Run 14 stress 0.07476995 
    #> ... Procrustes: rmse 6.825046e-07  max resid 1.16635e-06 
    #> ... Similar to previous best
    #> Run 15 stress 0.07476995 
    #> ... Procrustes: rmse 3.639403e-06  max resid 5.612016e-06 
    #> ... Similar to previous best
    #> Run 16 stress 0.07476995 
    #> ... Procrustes: rmse 1.699832e-06  max resid 3.498498e-06 
    #> ... Similar to previous best
    #> Run 17 stress 0.07476995 
    #> ... Procrustes: rmse 1.757011e-06  max resid 3.518899e-06 
    #> ... Similar to previous best
    #> Run 18 stress 0.07476995 
    #> ... Procrustes: rmse 3.717768e-06  max resid 6.130266e-06 
    #> ... Similar to previous best
    #> Run 19 stress 0.07476995 
    #> ... Procrustes: rmse 1.584284e-06  max resid 3.064061e-06 
    #> ... Similar to previous best
    #> Run 20 stress 0.07476995 
    #> ... Procrustes: rmse 2.573238e-06  max resid 5.667465e-06 
    #> ... Similar to previous best
    #> *** Best solution repeated 10 times

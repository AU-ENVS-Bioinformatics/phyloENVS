
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

``` r
vis_nmds(qaanaaq_rRNA_sub,
         convert_to_rel = TRUE,
         group_color = Wetness,
         group_shape = Transect,
         encircle = TRUE,
         fill_circle = FALSE)
#> Wisconsin double standardization
#> Run 0 stress 0.07476995 
#> Run 1 stress 0.07476995 
#> ... Procrustes: rmse 1.001875e-06  max resid 1.840631e-06 
#> ... Similar to previous best
#> Run 2 stress 0.07476995 
#> ... Procrustes: rmse 7.001035e-07  max resid 1.40393e-06 
#> ... Similar to previous best
#> Run 3 stress 0.07476995 
#> ... Procrustes: rmse 1.515304e-06  max resid 3.027232e-06 
#> ... Similar to previous best
#> Run 4 stress 0.07476995 
#> ... New best solution
#> ... Procrustes: rmse 3.200493e-06  max resid 5.843974e-06 
#> ... Similar to previous best
#> Run 5 stress 0.07476995 
#> ... Procrustes: rmse 1.2636e-06  max resid 3.207607e-06 
#> ... Similar to previous best
#> Run 6 stress 0.07476995 
#> ... Procrustes: rmse 5.430754e-06  max resid 1.041588e-05 
#> ... Similar to previous best
#> Run 7 stress 0.1441608 
#> Run 8 stress 0.1473151 
#> Run 9 stress 0.07476995 
#> ... Procrustes: rmse 4.680578e-06  max resid 9.389433e-06 
#> ... Similar to previous best
#> Run 10 stress 0.07476995 
#> ... Procrustes: rmse 6.33387e-07  max resid 1.58235e-06 
#> ... Similar to previous best
#> Run 11 stress 0.07476995 
#> ... Procrustes: rmse 1.836801e-06  max resid 2.767109e-06 
#> ... Similar to previous best
#> Run 12 stress 0.07476995 
#> ... Procrustes: rmse 2.15781e-06  max resid 5.758192e-06 
#> ... Similar to previous best
#> Run 13 stress 0.213687 
#> Run 14 stress 0.147315 
#> Run 15 stress 0.1473151 
#> Run 16 stress 0.07476995 
#> ... Procrustes: rmse 2.080242e-06  max resid 4.166984e-06 
#> ... Similar to previous best
#> Run 17 stress 0.07476995 
#> ... New best solution
#> ... Procrustes: rmse 3.138985e-07  max resid 5.193449e-07 
#> ... Similar to previous best
#> Run 18 stress 0.147315 
#> Run 19 stress 0.07476995 
#> ... Procrustes: rmse 1.980136e-07  max resid 3.76985e-07 
#> ... Similar to previous best
#> Run 20 stress 0.1473151 
#> *** Best solution repeated 2 times
#> Scale for colour is already present.
#> Adding another scale for colour, which will replace the existing scale.
```

<div class="figure">

<img src="man/figures/README-example2-1.png" alt="NMDS plot." width="80%" />
<p class="caption">
NMDS plot.
</p>

</div>

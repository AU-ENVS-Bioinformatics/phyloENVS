
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
#> Run 1 stress 0.1441608 
#> Run 2 stress 0.07476995 
#> ... New best solution
#> ... Procrustes: rmse 2.120032e-06  max resid 3.471499e-06 
#> ... Similar to previous best
#> Run 3 stress 0.07476995 
#> ... Procrustes: rmse 1.785535e-06  max resid 3.841723e-06 
#> ... Similar to previous best
#> Run 4 stress 0.07476995 
#> ... Procrustes: rmse 1.309919e-06  max resid 2.538998e-06 
#> ... Similar to previous best
#> Run 5 stress 0.1441607 
#> Run 6 stress 0.07476995 
#> ... Procrustes: rmse 2.409642e-06  max resid 5.521965e-06 
#> ... Similar to previous best
#> Run 7 stress 0.07476995 
#> ... Procrustes: rmse 3.110086e-06  max resid 7.956842e-06 
#> ... Similar to previous best
#> Run 8 stress 0.07476995 
#> ... Procrustes: rmse 1.761549e-06  max resid 3.128008e-06 
#> ... Similar to previous best
#> Run 9 stress 0.07476995 
#> ... Procrustes: rmse 3.500473e-06  max resid 6.230269e-06 
#> ... Similar to previous best
#> Run 10 stress 0.07476995 
#> ... Procrustes: rmse 1.938975e-06  max resid 3.821407e-06 
#> ... Similar to previous best
#> Run 11 stress 0.07476995 
#> ... Procrustes: rmse 1.804841e-06  max resid 3.969019e-06 
#> ... Similar to previous best
#> Run 12 stress 0.07476995 
#> ... Procrustes: rmse 3.183724e-06  max resid 5.867013e-06 
#> ... Similar to previous best
#> Run 13 stress 0.07476995 
#> ... Procrustes: rmse 2.430024e-06  max resid 3.640362e-06 
#> ... Similar to previous best
#> Run 14 stress 0.1473151 
#> Run 15 stress 0.07476995 
#> ... Procrustes: rmse 2.134449e-06  max resid 4.603016e-06 
#> ... Similar to previous best
#> Run 16 stress 0.1434945 
#> Run 17 stress 0.07476995 
#> ... Procrustes: rmse 6.697101e-07  max resid 1.455106e-06 
#> ... Similar to previous best
#> Run 18 stress 0.2200962 
#> Run 19 stress 0.07476995 
#> ... Procrustes: rmse 1.437756e-06  max resid 2.799233e-06 
#> ... Similar to previous best
#> Run 20 stress 0.07476995 
#> ... Procrustes: rmse 3.964178e-06  max resid 7.016127e-06 
#> ... Similar to previous best
#> *** Best solution repeated 15 times
```

<div class="figure">

<img src="man/figures/README-example2-1.png" alt="NMDS plot." width="80%" />
<p class="caption">
NMDS plot.
</p>

</div>

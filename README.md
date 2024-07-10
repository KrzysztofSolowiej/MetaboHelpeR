# MetaboHelpeR <img src='man/figures/logo.png' align="right" height="150"/>

*MetaboHelpeR* is an R package and a shiny app designed to clean and visualize metabolomic data.

## Installation and Running

To install the *MetaboHelpeR* package, please use the following commands in *R* or *RStudio*:
```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

remotes::install_github("KrzysztofSolowiej/MetaboHelpeR", repos = BiocManager::repositories())
```
Sometimes, not all packages can be installed on the first try.
In this case, please restart *R* and try:
```r
remove.packages("MetaboHelpeR")
remotes::install_github("KrzysztofSolowiej/MetaboHelpeR", repos = BiocManager::repositories())
```
After successful installation you can run *MetaboHelpeR* app by typing the following commands into an *R* console.
``` r
library(MetaboHelpeR)
MetaboHelpeR_launch()
```

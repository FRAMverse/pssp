# pssp
<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/FRAMverse/pssp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FRAMverse/pssp/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->


## Overview

pssp is a package that works with WDFW collected Puget Sound creel sampling data. 
None of the functions with retrieve data will work outside of the state network,
although functions focused on analysis will work give the proper dataset.

## Installation

pssp can be installed through R-Universe like so:

``` r
install.packages("pssp", repos = "https://framverse.r-universe.dev")
```

Alternatively, with Rtools and `devtools` or `remotes` installed, pssp can be installed like so:

``` r
devtools::install_github("FRAMverse/pssp")

# Alternatively 
remotes::install_github("FRAMverse/pssp")
```

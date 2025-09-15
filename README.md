
<!-- README.md is generated from README.Rmd. Please edit that file -->

# synthsensor

<!-- badges: start -->

[![R-CMD-check](https://github.com/AstridMarie2/synthsensor/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AstridMarie2/synthsensor/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of synthsensor is to …

## Installation

You can install the development version of synthsensor like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Anomaly flags

AnomalyFlag1/2 are factors with levels: Normal, Drift, Spike, Both,
SpikeCorr Overlap rule: any Drift overlapping with Spike or SpikeCorr is
labeled Both. Correlated spikes use SpikeCorr; uncorrelated spikes use
Spike.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(synthsensor)
## basic example code
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.


<!-- README.md is generated from README.Rmd. Please edit that file -->

# giniCI: Gini-based Composite Indicators

<!-- badges: start -->

[![CRAN](https://www.r-pkg.org/badges/version/giniCI)](https://cran.r-project.org/package=giniCI)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

`giniCI` provides an implementation of Gini-based weighting approaches
for composite indicator construction. The package includes functions for
normalization, aggregation, and ranking comparison to support
multidimensional measurement based on distributional dispersion across
individual components.

## Installation

You can install the latest released version from CRAN:

``` r
install.packages("giniCI")
```

Alternatively, you can install the development version from GitHub:

``` r
devtools::install_github("novidu/giniCI", build_vignettes = TRUE)
```

## Usage

Below is a simple example of constructing Gini-based composite
indicators. For more details, please take a look at the package
vignettes using `browseVignettes("giniCI")`.

``` r
library(giniCI)
data(bli)

# Indicator polarity
bli.pol = c("neg", "pos", "pos", "pos", "pos", "neg",
            "pos", "pos", "pos", "neg", "pos")

# Goalpost normalization using time factors and a reference time
bli.norm.2014 <- normalize(inds = bli[, 3:13], method = "goalpost",
                           ind.pol = bli.pol, time = bli$YEAR,
                           ref.time = 2014)

# Composite indices
ci.gini <- giniCI(bli.norm.2014, method = "gini",
                  ci.pol = "pos", time = bli$YEAR, ref.time = 2014,
                  only.ci = TRUE)
ci.reci <- giniCI(bli.norm.2014, method = "reci", agg = "geo",
                  ci.pol = "pos", time = bli$YEAR, ref.time = 2014,
                  only.ci = TRUE)

# Ranking comparison
ci.comp <- rankComp(ci.gini, ci.reci, id = bli$COUNTRY, time = bli$YEAR)
summary(ci.comp)
```

## Authors and Contributions

Authors: Viet Duong Nguyen (maintainer), Chiara Gigliarano, and
Mariateresa Ciommi

Suggested improvements, as well as technical issues and bug reports, are
highly welcome.

Please direct development questions to <viet-duong.nguyen@outlook.com>.

## References

Ciommi, M., Gigliarano, C., Emili, A., Taralli, S., & Chelli, F. M.
(2017). A new class of composite indicators for measuring well-being at
the local level: An application to the Equitable and Sustainable
Well-being (BES) of the Italian Provinces. <em>Ecological
Indicators</em>, 76, 281–296.
<https://doi.org/10.1016/j.ecolind.2016.12.050>

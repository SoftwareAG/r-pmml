
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pmml <a href='https://CRAN.R-project.org/package=pmml'><img src='man/figures/logo3.png' align="right" height="139" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/pmml)](https://cran.r-project.org/package=pmml)
[![R-CMD-check](https://github.com/SoftwareAG/r-pmml/workflows/R-CMD-check/badge.svg)](https://github.com/SoftwareAG/r-pmml/actions)
[![Codecov test
coverage](https://codecov.io/gh/SoftwareAG/r-pmml/branch/master/graph/badge.svg)](https://app.codecov.io/gh/SoftwareAG/r-pmml?branch=master)
<!-- badges: end -->

## Overview

Export various machine learning and statistical models to PMML and
generate data transformations in PMML format.

For a description of the supported packages, see the vignette:
[Supported Packages and Additional
Functions](https://open-source.softwareag.com/r-pmml/articles/packages_and_functions.html).

## Installation

You can install the released version of pmml from
[CRAN](https://CRAN.R-project.org/package=pmml) with:

``` r
install.packages("pmml")
```

## Example

``` r
library(pmml)

# Build an lm model
iris_lm <- lm(Sepal.Length ~ ., data=iris)

# Convert to pmml
iris_lm_pmml <- pmml(iris_lm)

# Write to file
# save_pmml(iris_lm_pmml,"iris_lm.pmml")
```

------------------------------------------------------------------------

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/SoftwareAG/r-pmml/blob/master/.github/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

These tools are provided as-is and without warranty or support. They do
not constitute part of the Software AG product suite. Users are free to
use, fork and modify them, subject to the license agreement. While
Software AG welcomes contributions, we cannot guarantee to include every
contribution in the master project.

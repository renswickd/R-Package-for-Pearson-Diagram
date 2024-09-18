
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pearson.diagram

<!-- badges: start -->
<!-- badges: end -->

The Pearson Diagram R package provides tools to visualize distributions
using Pearson Diagrams. It helps users calculate skewness and kurtosis
for unknown data distributions, plot the Pearson Diagram, and compare
multiple distributions. This package leverages **Rcpp** for efficient
computation and follows the **S3** object-oriented programming paradigm
in R.

## Key Features

- **Plotting Pearson Diagrams:** Visualize distributions on a Pearson
  Diagram by plotting skewness and kurtosis.
- **Comparison of Multiple Distributions:** Compute and compare skewness
  and kurtosis for multiple distributions.
- **Efficient Calculation with Rcpp:** Uses C++ for high-performance
  calculation of skewness and kurtosis.
- **S3 Object-Oriented Programming:** Encapsulates functionality within
  S3 classes for Pearson Diagrams.

## Installation

You can install the development version of pearson.diagram like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(pearson.diagram)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

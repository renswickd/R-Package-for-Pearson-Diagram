
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pearson.diagram

<!-- badges: start -->
<!-- badges: end -->

## Overview

The `pearson.diagram` package provides tools to visualize different
statistical distributions by plotting skewness and kurtosis values on a
Pearson Diagram. This package is useful for comparing multiple
distributions, calculating skewness and kurtosis for unknown data
distributions, and identifying data characteristics in terms of central
moments. The Pearson diagram illustrates the relationship between the
skewness and kurtosis of several probability distributions. The
$x-\text{axis}$ shows the square of skewness, while the $y-\text{axis}$
shows the kurtosis. This figure helps in visually identifying the
properties of various distributions and how they fit in the
skewness-kurtosis space.

Skewness $\beta_{1}=\mu_{3}^{2}/\mu_{2}^{3}$

Kurtosis $\beta_{2}=\mu_{4}/\mu_{2}^{2}$

Key features of the package include:

- Plotting of known and user-defined distributions on a Pearson Diagram
- Efficient skewness and kurtosis calculations using **Rcpp**
- Bootstrap analysis to understand variability in skewness and kurtosis
  estimates
- Handling of extreme outliers for robust data visualization
- Flexibility in customizing plot appearance

The package follows the **S3** object-oriented programming paradigm for
flexibility and extensibility.

## Installation

To install the `pearson.diagram` package, clone the repository and build
the package using the following commands:

``` r
# Clone the repository from GitHub

# Install the package
install.packages("renswickd/R-Package-for-Pearson-Diagram", repos = NULL, type = "source")
## 1. Installation and loading the package
Install the package from GitHub using the following command:
```

devtools::install_github(“renswickd/R-Package-for-Pearson-Diagram”)

    ## Getting Started
    Once installed, load the package and explore its main functions.

    ``` r
    library(pearson.diagram)

### Main Function: plot_diagram

The primary function for visualizing data on the Pearson Diagram is
plot_diagram. It allows plotting both known distributions and
user-defined data distributions.

- This is the main function that generates the Pearson diagram. It
  accepts a variety of input parameters, including a Pearson object,
  custom input data, and option to enable bootstrap functionality or
  hover-over interactivity.
- If custom (unknown) data is provided, it calculates the square of
  skewness and kurtosis for the data points and highlights them on the
  plot. Otherwise, the function generates a pearson plot with known
  distribution families and no extra points.

## Contributing

If you find a bug or would like to request a feature, please create an
issue on GitHub. Pull requests are welcome; please make sure that the
code is thoroughly tested and documented.

## License

This package is licensed under the MIT License. See the LICENSE file for
more details.

## Acknowledgments

The pearson.diagram package leverages Rcpp for high-performance
computation and ggplot2 for flexible data visualization. We thank the R
community for their invaluable support and resources.

### 2.3 `Object Oriented Programming`

The `pearson.diagram` package utilizes OOP approach using R S3 classes.
This gives for greater flexibility when it comes to adding custom data
points and customizing plot characteristics. By designing the package in
such a way, it is easier to expand or modify the behavior of each
function for different kinds of data and objects. The following
functions are crucial for implementing OOP:

- `PearsonDiagram()`: Creates an object of the `PearsonDiagram` class
  containing information on values required to create the canvas and
  plot the points such as square of skewness, kurtosis, and the
  distribution name.

- `add_point.PearsonDiagram()`: Uses the dispatch function to add points
  to the `PearsonDiagram` object. This method allows users to add
  additional data to the object in a systematic manner.

- `plot_diagram.PearsonDiagram()` plots the Pearson diagram by
  dispatching based on the object’s class.

#' Create a PearsonDiagram Object
#'
#' This function creates an empty PearsonDiagram object. The PearsonDiagram object is used to store skewness and kurtosis values and allows for plotting and comparing distributions.
#'
#' @return An empty PearsonDiagram object.
#' @examples
#' pd <- PearsonDiagram()
#' @export
PearsonDiagram <- function() {
  structure(list(points = data.frame(skewness = numeric(), kurtosis = numeric())),
            class = "PearsonDiagram")
}

# Add skewness and kurtosis points to the PearsonDiagram object
#' Add a point to the Pearson Diagram
#' @param object PearsonDiagram object
#' @param skewness Skewness value of the distribution
#' @param kurtosis Kurtosis value of the distribution
#' @return Updated PearsonDiagram object with new points
#' @export
add_point <- function(object, skewness, kurtosis) {
  UseMethod("add_point")
}

# Default S3 method for adding points to PearsonDiagram
#' @export
add_point.PearsonDiagram <- function(object, skewness, kurtosis) {
  object$points <- rbind(object$points, data.frame(skewness = skewness, kurtosis = kurtosis))
  return(object)
}

# Plot the Pearson Diagram based on stored points
#' Plot the Pearson Diagram
#' @param object PearsonDiagram object
#' @return ggplot2 object representing the Pearson diagram
#' @export
plot_diagram <- function(object) {
  UseMethod("plot_diagram")
}

# Plot the Pearson Diagram based on stored points
#' Plot the Pearson Diagram
#' @param object PearsonDiagram object
#' @return ggplot2 object representing the Pearson diagram
#' @export
plot_diagram.PearsonDiagram <- function(object) {

  if (nrow(object$points) == 0) {
    stop("No points to plot. Add points to the Pearson diagram first.")
  }

  # Ensure the values are obtained directly from the data frame within the function
  p <- ggplot2::ggplot(object$points, ggplot2::aes(x = object$points$skewness, y = object$points$kurtosis)) +
    ggplot2::geom_point(color = "blue") +
    ggplot2::xlab("Skewness") + ggplot2::ylab("Kurtosis") +
    ggplot2::ggtitle("Pearson Diagram")

  print(p)
  return(p)
}

# Compare multiple distributions by adding them to the Pearson Diagram
#' Compare multiple distributions
#' @param object PearsonDiagram object
#' @param data_list A list of numeric vectors
#' @return Updated PearsonDiagram object with new points and a plot
#' @export
compare_distributions <- function(object, data_list) {
  UseMethod("compare_distributions")
}

# Default S3 method for comparing distributions
#' @export
compare_distributions.PearsonDiagram <- function(object, data_list) {
  for (data in data_list) {
    sk_kt <- cpp_calculate_skewness_kurtosis(data)
    object <- add_point(object, sk_kt[1], sk_kt[2])
  }
  plot_diagram(object)
  return(object)
}

#' Calculate Skewness and Kurtosis Using Rcpp
#'
#' This function calculates the skewness and kurtosis using a high-performance C++ implementation via Rcpp.
#'
#' @param data A numeric vector containing the data points
#' @return A numeric vector with two values: skewness and kurtosis
#' @name cpp_calculate_skewness_kurtosis
#' @useDynLib pearson.diagram, .registration = TRUE
#' @import Rcpp
#' @importFrom Rcpp sourceCpp
#' @export
cpp_calculate_skewness_kurtosis

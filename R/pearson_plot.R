#' Create a PearsonDiagram Object
#'
#' This function creates an empty PearsonDiagram object. The PearsonDiagram object is used to store
#' skewness and kurtosis values and allows for plotting and comparing distributions.
#'
#' @return An empty PearsonDiagram object.
#' @export
#'
#' @examples
#' pd <- PearsonDiagram()
PearsonDiagram <- function() {
  structure(list(points = data.frame(skewness = numeric(), kurtosis = numeric(), distribution = character())),
            class = "PearsonDiagram")
}

#' Add a point to the Pearson Diagram
#'
#' This function adds a new point (skewness and kurtosis) to the Pearson diagram for a given distribution.
#'
#' @param object PearsonDiagram object
#' @param skewness Skewness value of the distribution
#' @param kurtosis Kurtosis value of the distribution
#' @param distribution Name of the distribution
#' @return Updated PearsonDiagram object with new points
#' @export
add_point <- function(object, skewness, kurtosis, distribution) {
  UseMethod("add_point")
}

#' @export
add_point.PearsonDiagram <- function(object, skewness, kurtosis, distribution) {
  object$points <- rbind(object$points,
                         data.frame(skewness = skewness,
                                    kurtosis = kurtosis,
                                    distribution = distribution))
  return(object)
}

#' Add Unknown Distribution to Pearson Diagram
#'
#' This function calculates the skewness and kurtosis for an unknown distribution and adds it to the Pearson diagram.
#' The unknown distribution is plotted as a black "X" on the diagram.
#'
#' @param object PearsonDiagram object
#' @param data A numeric vector representing the unknown distribution
#' @return Updated PearsonDiagram object with the unknown distribution point added
#' @export
add_unknown_data <- function(object, data) {

  validate_input(data)

  sk_kt <- cpp_calculate_skewness_kurtosis(data)
  object$points <- rbind(object$points,
                         data.frame(skewness = sk_kt[1],
                                    kurtosis = sk_kt[2],
                                    distribution = "Unknown"))

  # Plot the diagram with the new unknown distribution
  plot_diagram_with_unknown(object)
  return(object)
}


#' Plot the Pearson Diagram
#' @param object PearsonDiagram object
#' @return ggplot2 object representing the Pearson diagram
#' @export
plot_diagram <- function(object) {
  UseMethod("plot_diagram")
}

#' Plot the Pearson Diagram
#' @param object PearsonDiagram object
#' @return ggplot2 object representing the Pearson diagram
#' @export
plot_diagram.PearsonDiagram <- function(object) {
  if (nrow(object$points) == 0) {
    stop("No points to plot. Add points to the Pearson diagram first.")
  }

  # Plot the points with colors based on distribution
  p <- ggplot2::ggplot(object$points, ggplot2::aes(x = object$points$skewness, y = object$points$kurtosis, color = object$points$distribution)) +
    ggplot2::geom_point(size = 1) +
    ggplot2::xlab("Skewness") +
    ggplot2::ylab("Kurtosis") +
    ggplot2::ggtitle("Pearson Diagram") +
    ggplot2::scale_color_manual(values = c("Normal" = "blue", "Exponential" = "red",
                                           "Uniform" = "green", "Beta" = "orange",
                                           "Gamma" = "purple")) +
    ggplot2::theme_minimal()

  print(p)
  return(p)
}

#' Plot the Pearson Diagram with Unknown Distribution
#' @param object PearsonDiagram object
#' @return ggplot2 object representing the Pearson diagram with an unknown distribution
#' @export
plot_diagram_with_unknown <- function(object) {
  if (nrow(object$points) == 0) {
    stop("No points to plot. Add points to the Pearson diagram first.")
  }


  p <- ggplot2::ggplot(object$points, ggplot2::aes(x = object$points$skewness, y = object$points$kurtosis)) +

    ggplot2::geom_point(ggplot2::aes(color = object$points$distribution, shape = object$points$distribution), size = 1) +
    ggplot2::scale_color_manual(values = c("Normal" = "blue", "Exponential" = "red",
                                           "Uniform" = "green", "Beta" = "orange",
                                           "Gamma" = "purple", "Unknown" = "black")) +
    ggplot2::scale_shape_manual(values = c("Normal" = 16, "Exponential" = 16,
                                           "Uniform" = 16, "Beta" = 16,
                                           "Gamma" = 16, "Unknown" = 4)) +  # 'X' symbol for unknown

    ggplot2::xlab("Skewness") +
    ggplot2::ylab("Kurtosis") +
    ggplot2::ggtitle("Pearson Diagram with Unknown Distribution") +
    ggplot2::theme_minimal()

  print(p)
  return(p)
}


#' Compare and Plot Multiple Distributions on Pearson Diagram
#'
#' This function generates samples from various distributions (Normal, Exponential, Uniform, Beta, Gamma),
#' calculates their skewness and kurtosis, and plots them on the Pearson Diagram.
#'
#' @param object PearsonDiagram object
#' @param n_samples Number of samples to generate for each distribution
#' @param sample_size Size of each sample
#' @return Updated PearsonDiagram object with new points and a plot
#' @export
compare_multiple_distributions <- function(object, n_samples = 100, sample_size = 1000) {

  # Define distributions
  distributions <- list(
    "Normal" = function() stats::rnorm(sample_size),
    "Exponential" = function() stats::rexp(sample_size),
    "Uniform" = function() stats::runif(sample_size),
    "Beta" = function() stats::rbeta(sample_size, 2, 5),
    "Gamma" = function() stats::rgamma(sample_size, shape = 2, rate = 1)
  )

  # Iterate over each distribution
  for (dist_name in names(distributions)) {
    for (i in 1:n_samples) {
      data <- distributions[[dist_name]]()
      sk_kt <- cpp_calculate_skewness_kurtosis(data)
      object <- add_point(object, sk_kt[1], sk_kt[2], dist_name)
    }
  }

  # Plot the Pearson Diagram
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

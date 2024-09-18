#' Plot Pearson Diagram
#'
#' This function plots the Pearson diagram based on provided skewness and kurtosis values.
#'
#' @param skewness The skewness of the data
#' @param kurtosis The kurtosis of the data
#' @return A ggplot2 object representing the Pearson diagram
#' @examples
#' plot_pearson_diagram(0, 3) # Plot a normal distribution
#' @export
plot_pearson_diagram <- function(skewness, kurtosis) {
  # Use explicit package namespace for ggplot2 functions
  plot_data <- data.frame(skewness = skewness, kurtosis = kurtosis)
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = skewness, y = kurtosis)) +
    ggplot2::geom_point(color = "blue") +
    ggplot2::xlab("Skewness") + ggplot2::ylab("Kurtosis") +
    ggplot2::ggtitle("Pearson Diagram")
  print(p)
  return(p)
}

#' Compare Multiple Distributions on Pearson Diagram
#'
#' This function compares multiple distributions by calculating and plotting their skewness and kurtosis.
#'
#' @param data_list A list of numeric vectors, each representing a dataset
#' @return A ggplot2 object showing multiple points on the Pearson diagram
#' @examples
#' data1 <- rnorm(100)
#' data2 <- runif(100)
#' compare_distributions(list(data1, data2))
#' @export
compare_distributions <- function(data_list) {
  results <- data.frame(skewness = numeric(), kurtosis = numeric())

  for (data in data_list) {
    sk_kt <- cpp_calculate_skewness_kurtosis(data)
    results <- rbind(results, data.frame(skewness = sk_kt[1], kurtosis = sk_kt[2]))
  }

  p <- ggplot2::ggplot(results, ggplot2::aes(x = sk_kt[1], y = sk_kt[2])) +
    ggplot2::geom_point(color = "red") +
    ggplot2::xlab("Skewness") + ggplot2::ylab("Kurtosis") +
    ggplot2::ggtitle("Comparison of Distributions")

  print(p)
  return(p)
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

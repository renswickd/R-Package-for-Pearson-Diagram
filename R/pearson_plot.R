#' Create a PearsonDiagram Object
#'
#' This function creates an empty PearsonDiagram object.
#' The PearsonDiagram object is used to store sq_skewness, kurtosis values, and distribution.
#'
#' @return A PearsonDiagram object.
#' @export
#' @examples
#' pd <- PearsonDiagram()
PearsonDiagram <- function() {
  structure(
    list(points = data.frame(sq_skewness = numeric(), kurtosis = numeric(), distribution = character())),
    class = "PearsonDiagram"
  )
}

#' Add a point to the Pearson Diagram
#'
#' Adds a point with the given sq_skewness and kurtosis values.
#'
#' @param object PearsonDiagram object
#' @param sq_skewness Square of skewness value of the distribution
#' @param kurtosis Kurtosis value of the distribution
#' @param distribution The name of the distribution
#' @return Updated PearsonDiagram object with new points
#' @export
add_point <- function(object, sq_skewness, kurtosis, distribution=NA) {
  UseMethod("add_point")
}

#' Add a point to the Pearson Diagram
#'
#' @param object PearsonDiagram object
#' @param sq_skewness Square of skewness value of the distribution
#' @param kurtosis Kurtosis value of the distribution
#' @param distribution The name of the distribution
#' @return Updated PearsonDiagram object with new points
#' @export
add_point.PearsonDiagram <- function(object, sq_skewness, kurtosis, distribution=NA) {
  object$points <- rbind(
    object$points,
    data.frame(sq_skewness = sq_skewness, kurtosis = kurtosis, distribution = distribution)
  )
  return(object)
}

#' Plot the Pearson Diagram
#'
#' This function plots the Pearson diagram with known distributions and added points.
#' @param object A PearsonDiagram object.
#' @return A ggplot2 object representing the Pearson diagram.
#' @export
plot_diagram <- function(object) {
  UseMethod("plot_diagram")
}

#' @export
plot_diagram.PearsonDiagram <- function(object) {

  if (nrow(object$points) == 0) {
    stop("No points to plot. Add points to the Pearson diagram first.")
  }

  # Generate known distributions' data
  data <- generate_data()

  point_data <- data[data$distribution %in% c("Normal", "Uniform"), ]
  exp_line_data <- data[data$distribution == "Exponential", ]
  gamma_line_data <- data[data$distribution == "Gamma", ]
  area_data <- data[data$distribution == "Beta", ]

  # Plot known and unknown distributions
  p <- ggplot2::ggplot() +
    # ggplot2::geom_point(data = point_data, ggplot2::aes(x = point_data$sq_skewness, y = point_data$kurtosis, color = point_data$distribution), size = 5) +
    # ggplot2::geom_line(data = exp_line_data, ggplot2::aes(x = exp_line_data$sq_skewness, y = exp_line_data$kurtosis, color = exp_line_data$distribution), size = 0.8) +
    # ggplot2::geom_line(data = gamma_line_data, ggplot2::aes(x = gamma_line_data$sq_skewness, y = gamma_line_data$kurtosis, color = gamma_line_data$distribution), size = 0.8) +
    # ggplot2::geom_polygon(data = area_data, ggplot2::aes(x = area_data$sq_skewness, y = area_data$kurtosis, fill = area_data$distribution), alpha = 0.5) +

    with(point_data, ggplot2::geom_point(ggplot2::aes(x = sq_skewness, y = kurtosis, color = distribution), size = 5)) +
    with(exp_line_data, ggplot2::geom_line(ggplot2::aes(x = sq_skewness, y = kurtosis, color = distribution), linewidth = 0.8)) +
    with(gamma_line_data, ggplot2::geom_line(ggplot2::aes(x = sq_skewness, y = kurtosis, color = distribution), linewidth = 0.8)) +
    with(area_data, ggplot2::geom_polygon(ggplot2::aes(x = sq_skewness, y = kurtosis, fill = distribution), alpha = 0.5)) +

    # Plot unknown points with thick X and circle
    ggplot2::geom_point(data = object$points, ggplot2::aes(x = object$points$sq_skewness, y = object$points$kurtosis),
                        color = "black", shape = 21, fill = "white", size = 2) +
    ggplot2::geom_point(data = object$points, ggplot2::aes(x = object$points$sq_skewness, y = object$points$kurtosis),
                        color = "black", shape = 4, size = 1) +

    # Reverse the y-axis for Kurtosis
    ggplot2::scale_y_reverse() +

    # Axis labels
    ggplot2::xlab(expression(paste("Square of Skewness (", beta[1]^2, ")"))) +
    ggplot2::ylab(expression(paste("Kurtosis (", beta[2], ")"))) +

    # Legend and title
    ggplot2::ggtitle("Pearson Diagram with Different Distribution Families") +
    ggplot2::scale_color_manual(name = "Point & Line Representations",  # Custom legend title
                                values = c("Normal" = "skyblue",
                                           "Uniform" = "lightgreen",
                                           "Exponential" = "lightcoral",
                                           "Gamma" = "plum",
                                           "Beta" = "lightsalmon")) +
    ggplot2::scale_fill_manual(name= "Area Representations", values = c("Beta" = "lightsalmon")) +
    ggplot2::theme_minimal()

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

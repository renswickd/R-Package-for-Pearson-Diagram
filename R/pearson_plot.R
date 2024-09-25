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

#' Create Pearson Diagram Canvas
#'
#' This function creates a Pearson diagram canvas with known distributions without any input data points.
#' The canvas represents Normal, Exponential, Gamma, and Beta distributions.
#'
#' @return A ggplot2 object representing the Pearson diagram canvas.
#' @export
canvas_creation <- function() {
  # Generate known distributions' data
  data <- generate_data()

  point_data <- data[data$distribution %in% c("Normal", "Uniform"), ]
  exp_line_data <- data[data$distribution == "Exponential", ]
  gamma_line_data <- data[data$distribution == "Gamma", ]
  area_data <- data[data$distribution == "Beta", ]

  # Create the canvas
  p <- ggplot2::ggplot() +
    with(point_data, ggplot2::geom_point(ggplot2::aes(x = sq_skewness, y = kurtosis, color = distribution), size = 5)) +
    with(exp_line_data, ggplot2::geom_line(ggplot2::aes(x = sq_skewness, y = kurtosis, color = distribution), linewidth = 0.8)) +
    with(gamma_line_data, ggplot2::geom_line(ggplot2::aes(x = sq_skewness, y = kurtosis, color = distribution), linewidth = 0.8)) +
    with(area_data, ggplot2::geom_polygon(ggplot2::aes(x = sq_skewness, y = kurtosis, fill = distribution), alpha = 0.5)) +

    # Reverse the y-axis for Kurtosis
    ggplot2::scale_y_reverse() +

    # Axis labels
    ggplot2::xlab(expression(paste("Square of Skewness (", beta[1]^2, ")"))) +
    ggplot2::ylab(expression(paste("Kurtosis (", beta[2], ")"))) +

    # Legend and title
    ggplot2::ggtitle("Pearson Diagram Canvas") +
    ggplot2::scale_color_manual(name = "Distributions",  # Custom legend title
                                values = c("Normal" = "skyblue",
                                           "Uniform" = "lightgreen",
                                           "Exponential" = "lightcoral",
                                           "Gamma" = "plum",
                                           "Beta" = "lightsalmon")) +
    ggplot2::scale_fill_manual(name= NULL, values = c("Beta" = "lightsalmon")) +
    ggplot2::theme_minimal()

  return(p)
}

#' Plot the Pearson Diagram with Input Data
#'
#' This function plots the Pearson diagram with known distributions and user-provided data points.
#' It supports input_data as a numeric vector or a list of numeric vectors. For each input, it calculates the
#' skewness and kurtosis and plots them on the canvas.
#'
#' @param object A PearsonDiagram object.
#' @param input_data A numeric vector or a list of numeric vectors containing the data points (optional).
#' @param bootstrap A boolean indicating whether to perform bootstrap analysis (placeholder for now).
#' @param hover A boolean indicating whether to add hover functionality (placeholder for now).
#' @return A ggplot2 object representing the Pearson diagram.
#' @export
plot_diagram <- function(object, input_data = NULL, bootstrap = FALSE, hover = FALSE) {
  # Validate the PearsonDiagram object
  validate_input(object)

  # If input_data is provided, validate and add points to the PearsonDiagram object
  if (!is.null(input_data)) {
    validate_input(input_data)

    if (is.list(input_data)) {
      object <- add_points_from_list(object, input_data)
    } else {
      moments <- cpp_calculate_moments(input_data)
      object <- add_point(object, sq_skewness = moments$sq_skewness, kurtosis = moments$kurtosis, distribution = "Input data")
    }
  }

  # Start with the canvas
  p <- canvas_creation()

  # Add points for the user-supplied data
  p <- p + ggplot2::geom_point(data = object$points, ggplot2::aes(x = object$points$sq_skewness, y = object$points$kurtosis),
                               color = "black", shape = 21, fill = "white", size = 2) +
    ggplot2::geom_point(data = object$points, ggplot2::aes(x = object$points$sq_skewness, y = object$points$kurtosis),
                        color = "black", shape = 4, size = 1)

  # Placeholder for hover functionality
  if (hover) {
    p <- highlight_point_on_hover(p)
  }

  # Placeholder for bootstrap functionality
  if (bootstrap) {
    message("Bootstrap functionality is not implemented yet.")
  }

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

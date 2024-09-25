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
#' This function plots the Pearson diagram, calculates moments if input data is provided,
#' and adds bootstrap functionality.
#'
#' @param object A PearsonDiagram object.
#' @param data Optional input data for which moments (skewness, kurtosis) will be calculated.
#' @param bootstrap Boolean indicating if bootstrap should be applied (default is FALSE).
#' @param hover Boolean indicating if hover functionality should be enabled (default is FALSE).
#' @return A ggplot2 object representing the Pearson diagram.
#' @export
plot_diagram <- function(object, data = NULL, bootstrap = FALSE, hover = FALSE) {
  validate_input(object)

  # Create the base canvas with known distributions
  p <- create_pearson_canvas()

  # If input data is provided, calculate the skewness and kurtosis and add points
  if (!is.null(data)) {
    validate_input(data)
    moments <- cpp_calculate_moments(data)
    object <- add_point(object, sq_skewness = moments$sq_skewness, kurtosis = moments$kurtosis, distribution = "Input Data")
    if (nrow(object$points) > 0) {
      # Plot the points for the Pearson diagram
      p <- p + ggplot2::geom_point(data = object$points, ggplot2::aes(x = object$points$sq_skewness, y = object$points$kurtosis),
                                   color = "black", shape = 21, fill = "white", size = 2)
      p <- p + ggplot2::geom_point(data = object$points, ggplot2::aes(x = object$points$sq_skewness, y = object$points$kurtosis),
                                   color = "black", shape = 4, size = 1)
    }
  }

  # If bootstrap is enabled, add the placeholder function
  if (bootstrap) {
    placeholder_bootstrap_function()
  }

  # If hover is enabled, add hover functionality (Placeholder function for now)
  if (hover) {
    p <- highlight_point_on_hover(p)
  }

  return(p)
}

#' Create Pearson Diagram Canvas
#'
#' Creates the base Pearson diagram with known distributions.
#' @return A ggplot2 object for the Pearson diagram.
#' @export
create_pearson_canvas <- function() {
  # Generate known distributions' data
  data <- generate_data()

  point_data <- data[data$distribution %in% c("Normal", "Uniform"), ]
  exp_line_data <- data[data$distribution == "Exponential", ]
  gamma_line_data <- data[data$distribution == "Gamma", ]
  area_data <- data[data$distribution == "Beta", ]

  # Create the base plot
  p <- ggplot2::ggplot() +
    with(point_data, ggplot2::geom_point(ggplot2::aes(x = sq_skewness, y = kurtosis, color = distribution), size = 5)) +
    with(exp_line_data, ggplot2::geom_line(ggplot2::aes(x = sq_skewness, y = kurtosis, color = distribution), linewidth = 0.8)) +
    with(gamma_line_data, ggplot2::geom_line(ggplot2::aes(x = sq_skewness, y = kurtosis, color = distribution), linewidth = 0.8)) +
    with(area_data, ggplot2::geom_polygon(ggplot2::aes(x = sq_skewness, y = kurtosis, fill = distribution), alpha = 0.5)) +
    ggplot2::scale_y_reverse() +
    ggplot2::xlab(expression(paste("Square of Skewness (", beta[1]^2, ")"))) +
    ggplot2::ylab(expression(paste("Kurtosis (", beta[2], ")"))) +
    ggplot2::ggtitle("Pearson Diagram") +
    ggplot2::scale_color_manual(name = "Points & Lines", values = c("Normal" = "skyblue", "Uniform" = "lightgreen", "Exponential" = "lightcoral", "Gamma" = "plum", "Beta" = "lightsalmon")) +
    ggplot2::scale_fill_manual(name = NULL, values = c("Beta" = "lightsalmon")) +
    ggplot2::theme_minimal()

  return(p)
}

# Placeholder for the bootstrap functionality
placeholder_bootstrap_function <- function() {
  message("Bootstrap functionality is a placeholder and not implemented yet.")
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

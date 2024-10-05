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
  area_data <- generate_area_data()
  area_data_limit <- area_data$min_param1
  area_data_region <- area_data$merged_data

  point_data <- data[data$distribution %in% c("Normal", "Uniform", "Exponential"), ]
  # exp_line_data <- data[data$distribution == "Exponential", ]
  gamma_line_data <- data[data$distribution == "Gamma", ]
  inv_gamma_line_data <- data[data$distribution == "Inverse Gamma", ]
  # area_data <- data[data$distribution == "Beta", ]

  # Create the canvas
  p <- ggplot2::ggplot() +
    with(gamma_line_data, ggplot2::geom_line(ggplot2::aes(x = sq_skewness, y = kurtosis, color = distribution), linewidth = 0.8)) +
    with(inv_gamma_line_data, ggplot2::geom_line(ggplot2::aes(x = sq_skewness, y = kurtosis, color = distribution), linewidth = 0.8)) +
    with(area_data_limit, ggplot2::geom_line(data = area_data_limit, ggplot2::aes(x = sq_skewness, y = kurtosis, color = "Beta"), linewidth = 0.8)) +
    with(area_data_region, ggplot2::geom_ribbon(data = area_data_region,
                ggplot2::aes(x = sq_skewness, ymin = kurtosis_min, ymax = kurtosis_max, fill = "Beta"),
                alpha = 0.5)) +
    with(point_data, ggplot2::geom_point(ggplot2::aes(x = sq_skewness, y = kurtosis, color = distribution), size = 5)) +



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
                                           "Inverse Gamma" = "salmon",
                                           "Beta" = "lightblue")) +
    # ggplot2::scale_fill_manual(name= NULL, values = c("Beta" = "lightsalmon")) +
    ggplot2::scale_fill_manual(values = c("Beta" = "lightblue")) +
    ggplot2::theme_minimal()

  return(p)
}

#' Plot the Pearson Diagram with Input Data and Bootstrap Samples
#'
#' This function plots the Pearson diagram with known distributions and user-provided data points.
#' It supports input_data as a numeric vector or a list of numeric vectors. For each input, it calculates the
#' skewness and kurtosis and plots them on the canvas.
#'
#' @param object A PearsonDiagram object.
#' @param input_data A numeric vector or a list of numeric vectors containing the data points (optional).
#' @param bootstrap A boolean indicating whether to perform bootstrap analysis.
#' @param hover A boolean indicating whether to add hover functionality (placeholder for now).
#' @param treat.outliers A boolean indicating whether to exclude extreme outliers (default is FALSE).
#' @return A ggplot2 object representing the Pearson diagram.
#' @export
plot_diagram <- function(object, input_data = NULL, bootstrap = FALSE, hover = FALSE, treat.outliers = FALSE) {
  # Validate the PearsonDiagram object
  validate_input(object)
  p <- canvas_creation()

  # If input_data is provided, validate and add points to the PearsonDiagram object
  if (!is.null(input_data)) {
    validate_input(input_data)

    # Handle outliers if requested
    if (treat.outliers) {
      if (is.list(input_data)) {
        cleaned_list <- lapply(input_data, function(data) {
          result <- handle_outliers(data)
          if (result$num_outliers > 0) {
            warning(sprintf("Outliers detected and excluded: %d outliers removed from the dataset.", result$num_outliers))
          }
          return(result$cleaned_data)
        })
        input_data <- cleaned_list
      } else {
        result <- handle_outliers(input_data)
        if (result$num_outliers > 0) {
          warning(sprintf("Outliers detected and excluded: %d outliers removed from the dataset.", result$num_outliers))
        }
        input_data <- result$cleaned_data
      }
    }

    if (is.list(input_data)) {
      object <- add_points_from_list(object, input_data)
    } else {
      moments <- cpp_calculate_moments(input_data)
      object <- add_point(object, sq_skewness = moments$sq_skewness, kurtosis = moments$kurtosis, distribution = "Input data")
    }

    # Generate bootstrap samples if requested
    if (bootstrap) {
      bootstrap_results <- bootstrap_samples(input_data)

      # Create a data frame from bootstrap results
      bootstrap_df <- do.call(rbind, lapply(bootstrap_results, function(res) {
        data.frame(sq_skewness = res$sq_skewness, kurtosis = res$kurtosis)
      }))

      # Add bootstrap points to the plot
      p <- canvas_creation() +
        with(bootstrap_df, ggplot2::geom_point( ggplot2::aes(x = sq_skewness, y = kurtosis),
                                                color = "yellow", alpha = 0.2, size = 1))
    }
  } else {
    p <- canvas_creation()
  }
  # Add points for the user-supplied data
  p <- p + ggplot2::geom_point(data = object$points, ggplot2::aes(x = object$points$sq_skewness, y = object$points$kurtosis),
                               color = "black", shape = 21, fill = "white", size = 2) +
    ggplot2::geom_point(data = object$points, ggplot2::aes(x = object$points$sq_skewness, y = object$points$kurtosis),
                        color = "darkblue", shape = 4, size = 1)

  # Placeholder for hover functionality
  if (hover) {
    p <- highlight_point_on_hover(p)
  }

  return(p)
}


#' Generate Bootstrap Samples
#'
#' This function generates bootstrap samples from the provided input data.
#' @param input_data A numeric vector or list of numeric vectors
#' @param n_samples The number of bootstrap samples to generate
#' @param sample_size The size of each bootstrap sample (default: same as the length of the input data)
#' @return A list of bootstrap samples with calculated moments
#' @export
bootstrap_samples <- function(input_data, n_samples = 100, sample_size = NULL) {
  # If the input_data is a list, process each vector
  if (is.list(input_data)) {
    bootstrap_list <- lapply(input_data, function(data) {
      sample_size <- if (is.null(sample_size)) length(data) else sample_size
      replicate(n_samples, cpp_calculate_moments(sample(data, sample_size, replace = TRUE)), simplify = FALSE)
    })
    # Flatten the list of lists into a single list of bootstrap results
    return(do.call(c, bootstrap_list))
  }

  # If input_data is a single numeric vector
  sample_size <- if (is.null(sample_size)) length(input_data) else sample_size
  bootstrap_results <- replicate(n_samples, cpp_calculate_moments(sample(input_data, sample_size, replace = TRUE)), simplify = FALSE)

  return(bootstrap_results)
}

#' Calculate Moments for Bootstrap Samples
#'
#' This function calculates skewness and kurtosis for each bootstrap sample.
#' @param bootstrap_samples A list of numeric vectors representing bootstrap samples
#' @return A data frame containing skewness and kurtosis for each sample
#' @export
calculate_bootstrap_moments <- function(bootstrap_samples) {
  moments_list <- lapply(bootstrap_samples, cpp_calculate_moments)
  moments_df <- do.call(rbind, lapply(moments_list, function(x) {
    data.frame(sq_skewness = x$sq_skewness, kurtosis = x$kurtosis)
  }))
  return(moments_df)
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

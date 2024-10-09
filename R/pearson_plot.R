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
#' @param title.font.family Font family of the plot title
#' @param title.font.size Font size of the plot title
#' @param title.font.color Font color of the plot title
#' @param title.hjust alignment of the plot title
#' @param legend.font.family Font family of the plot legend
#' @param legend.font.size Font size of the plot legend
#' @param legend.font.color Font color of the plot legend
#' @param legend.hjust alignment of the plot legend
#' @param axis.font.family Font family of the plot axis
#' @param axis.font.size Font size of the plot axis
#' @param axis.font.color Font color of the plot axis
#' @return A ggplot2 object representing the Pearson diagram canvas.
#' @export
canvas_creation <- function(title.font.family = "Arial", title.font.size = 16, title.font.color = "black", title.hjust=NULL,
                            legend.font.family = "Arial", legend.font.size = 12, legend.font.color = "black", legend.hjust=NULL,
                            axis.font.family = "Arial", axis.font.size = 14, axis.font.color = "black") {
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
    ggplot2::scale_x_continuous(limits = c(0,6)) +

    # Axis labels
    ggplot2::xlab(expression(paste("Square of Skewness (", beta[1]^2, ")"))) +
    ggplot2::ylab(expression(paste("Kurtosis (", beta[2], ")"))) +
    ggplot2::theme_minimal() +

    # Legend and title
    ggplot2::labs(title = "Pearson Diagram") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(family = title.font.family, color = title.font.color, size = title.font.size, hjust = title.hjust),
      legend.title = ggplot2::element_text(family = legend.font.family, color = legend.font.color, size = legend.font.size, hjust = legend.hjust),
      axis.title = ggplot2::element_text(family = axis.font.family, color = axis.font.color, size = axis.font.size)
    )
    # ggplot2::scale_color_manual(name = "Distributions",  # Custom legend title
    #                             values = c("Normal" = "skyblue",
    #                                        "Uniform" = "lightgreen",
    #                                        "Exponential" = "lightcoral",
    #                                        "Gamma" = "plum",
    #                                        "Inverse Gamma" = "salmon",
    #                                        "Beta" = "lightblue")) +
    # ggplot2::scale_fill_manual(name= NULL, values = c("Beta" = "lightsalmon")) +
    # ggplot2::scale_fill_manual(values = c("Beta" = "lightblue")) +


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
#' @param hover A boolean indicating whether to add hover functionality.
#' @param treat.outliers A boolean indicating whether to exclude extreme outliers (default is FALSE).
#' @param title.font.family Font family of the plot title
#' @param title.font.size Font size of the plot title
#' @param title.font.color Font color of the plot title
#' @param title.hjust alignment of the plot title
#' @param legend.font.family Font family of the plot legend
#' @param legend.font.size Font size of the plot legend
#' @param legend.font.color Font color of the plot legend
#' @param legend.hjust alignment of the plot legend
#' @param axis.font.family Font family of the plot axis
#' @param axis.font.size Font size of the plot axis
#' @param axis.font.color Font color of the plot axis
#' @return A ggplot2 object representing the Pearson diagram.
#' @export
plot_diagram <- function(object, input_data = NULL, bootstrap = FALSE, hover = FALSE, treat.outliers = FALSE,
                         title.font.family = "Arial", title.font.size = 16, title.font.color = "black", title.hjust = NULL,
                         legend.font.family = "Arial", legend.font.size = 12, legend.font.color = "black", legend.hjust = NULL,
                         axis.font.family = "Arial", axis.font.size = 14, axis.font.color = "black") {
  # Validate the PearsonDiagram object
  validate_input(object)
  p <- canvas_creation(title.font.family, title.font.size, title.font.color, title.hjust,
                       legend.font.family, legend.font.size, legend.font.color, legend.hjust,
                       axis.font.family, axis.font.size, axis.font.color)

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
      object <- add_point(object, sq_skewness = moments$sq_skewness, kurtosis = moments$kurtosis, distribution = "Sample")
    }

    # Generate bootstrap samples if requested
    if (bootstrap) {
      bootstrap_results <- bootstrap_samples(input_data)

      # Create a data frame from bootstrap results
      bootstrap_df <- do.call(rbind, lapply(bootstrap_results, function(res) {
        data.frame(sq_skewness = res$sq_skewness, kurtosis = res$kurtosis)
      }))

      # Add bootstrap points to the plot
      p <- canvas_creation(title.font.family, title.font.size, title.font.color, title.hjust,
                           legend.font.family, legend.font.size, legend.font.color, legend.hjust,
                           axis.font.family, axis.font.size, axis.font.color) +
        with(bootstrap_df, ggplot2::geom_point( ggplot2::aes(x = sq_skewness, y = kurtosis),
                                                color = "yellow", alpha = 0.2, size = 1))
    }
  } else {
    p <- canvas_creation(title.font.family, title.font.size, title.font.color, title.hjust,
                         legend.font.family, legend.font.size, legend.font.color, legend.hjust,
                         axis.font.family, axis.font.size, axis.font.color)
  }
  plot_data = data.frame(object$points)
  plot_data$shape <- as.factor(22:(22+nrow(plot_data)-1))
  # print(plot_data)
  shapes <- c(21, 22, 23, 24, 25, 26)
  # n_input <- function(input) if (is.list(input)) length(input) else if (is.vector(input)) 1
  # n <- n_input(input_data)
  p_ggplot <- p + with(plot_data, ggplot2::geom_point(data = plot_data, #object$points,
                                                      ggplot2::aes(
                                                        x = sq_skewness,
                                                        y = kurtosis,
                                                        shape = distribution
                                                      ),
                                                      color = "black", size = 2)) +
    ggplot2::scale_shape_manual(
      values = shapes,
      name = NULL
    ) +
    ggplot2::scale_color_manual(
      name = "Distributions",
      values = c("Normal" = "orange",
                 "Uniform" = "skyblue",
                 "Exponential" = "blue",
                 "Gamma" = "green",
                 "Inverse Gamma" = "purple",
                 "Beta" = "red"),
      # ,
      labels = function(x) gsub(",1,NA", "", x)  # Clean the labels
      # values = RColorBrewer::brewer.pal(max(3, 6), "Set1"),
      # labels = 1:6 #function(x) gsub(",1,NA", "", x)
    ) +
    ggplot2::scale_fill_manual(
      name = NULL,
      values = RColorBrewer::brewer.pal(max(3, 6), "Set2")
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(title = "Distributions"),
      shape = ggplot2::guide_legend(title = NULL)
    ) #+

    # ggplot2::theme_minimal()
    # ggplot2::scale_shape_manual(values = distribution)

  if (hover) {
    p_ggplot <- p_ggplot + ggplot2::xlab("Square of Skewness") +
      ggplot2::ylab("Kurtosis")
  } else {
    # Use the more detailed labels for static plots
    p_ggplot <- p_ggplot + ggplot2::xlab(expression(paste("Square of Skewness (", beta[1]^2, ")"))) +
      ggplot2::ylab(expression(paste("Kurtosis (", beta[2], ")")))
  }

  if (hover) {
    p <- highlight_point_on_hover(p_ggplot)
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

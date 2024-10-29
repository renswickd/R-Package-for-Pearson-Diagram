#' Plot the Pearson Diagram for Unknown Data
#'
#' This function plots the Pearson diagram with known distributions and user-provided data points.
#' It supports input_data as a numeric vector or a list of numeric vectors. For each input, it calculates the
#' skewness and kurtosis and plots them on the canvas.
#'
#' @param input_data A numeric vector or a list of numeric vectors containing the data points (optional).
#' @param bootstrap A boolean indicating whether to perform bootstrap analysis.
#' @param treat.outliers A boolean indicating whether to exclude extreme outliers (default is FALSE).
#' @param summary.file Filename of the summary report
#' @param plot.name Filename of the output plot
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
#' @param cb.friendly Logical, if TRUE uses a colorblind-friendly palette
#' @return A ggplot2 object representing the Pearson diagram.
#' @examples
#' # Example 1: Basic plot with a numeric vector
#' data_vector <- c(1.2, 2.3, 3.4, 4.5, 5.6)
#' plot_diagram(input_data = data_vector)
#'
#' # Example 2: Plot with a list of numeric vectors
#' data_list <- list(c(1.1, 2.2, 3.3), c(2.2, 3.3, 4.4))
#' plot_diagram(input_data = data_list)
#'
#' # Example 3: Plot with bootstrap analysis
#' plot_diagram(input_data = data_vector, bootstrap = TRUE)
#'
#' # Example 4: Plot with outlier treatment
#' noisy_data <- c(1, 2, 3, 100, 5, 6)
#' plot_diagram(input_data = noisy_data, treat.outliers = TRUE, bootstrap = TRUE)
#'
#' # Example 5: Customizing fonts and plot appearance
#' plot_diagram(input_data = data_vector,
#'              title.font.family = "Arial", title.font.size = 20, title.font.color = "darkred",
#'              legend.font.size = 14, axis.font.size = 12)
#' @export
plot_diagram <- function(input_data = NULL, bootstrap = FALSE, treat.outliers = FALSE, summary.file = NULL, plot.name = NULL,
                         title.font.family = "Arial", title.font.size = 16, title.font.color = "black", title.hjust = NULL,
                         legend.font.family = "Arial", legend.font.size = 12, legend.font.color = "black", legend.hjust = NULL,
                         axis.font.family = "Arial", axis.font.size = 14, axis.font.color = "black", cb.friendly=FALSE) {

  # Custom parameter validation
  validate_customization <- function(param, param_name, expected_type, condition = TRUE) {
    if (!inherits(param, expected_type) || !condition) {
      stop(paste("Invalid value for", param_name,
                 "- expected", expected_type, "and valid value, got:", param))
    }
  }

  # Validate title customization parameters
  validate_customization(title.font.family, "title.font.family", "character")
  validate_customization(title.font.size, "title.font.size", "numeric", title.font.size > 0)
  validate_customization(title.font.color, "title.font.color", "character")
  if (!is.null(title.hjust)) validate_customization(title.hjust, "title.hjust", "numeric", title.hjust >= 0 && title.hjust <= 1)

  # Validate legend customization parameters
  validate_customization(legend.font.family, "legend.font.family", "character")
  validate_customization(legend.font.size, "legend.font.size", "numeric", legend.font.size > 0)
  validate_customization(legend.font.color, "legend.font.color", "character")
  if (!is.null(legend.hjust)) validate_customization(legend.hjust, "legend.hjust", "numeric", legend.hjust >= 0 && legend.hjust <= 1)

  # Validate axis customization parameters
  validate_customization(axis.font.family, "axis.font.family", "character")
  validate_customization(axis.font.size, "axis.font.size", "numeric", axis.font.size > 0)
  validate_customization(axis.font.color, "axis.font.color", "character")

  if (!is.null(summary.file)) validate_customization(summary.file, "summary.file", "character")
  if (!is.null(plot.name)) validate_customization(plot.name, "plot.name", "character")

  object <- PearsonDiagram()
  hover <- TRUE
  # Validate the PearsonDiagram object
  validate_input(object)
  p <- canvas_creation(title.font.family, title.font.size, title.font.color, title.hjust,
                       legend.font.family, legend.font.size, legend.font.color, legend.hjust,
                       axis.font.family, axis.font.size, axis.font.color, cb.friendly)

  # If input_data is provided, validate and add points to the PearsonDiagram object
  if (!is.null(input_data)) {
    validate_input(input_data)
    summary_df <- summary_stats(input_data, 0)

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
        cleaned_summary_df <- summary_stats(input_data, 1)
        summary_df <- rbind(summary_df, cleaned_summary_df)
      } else {
        result <- handle_outliers(input_data)
        if (result$num_outliers > 0) {
          warning(sprintf("Outliers detected and excluded: %d outliers removed from the dataset.", result$num_outliers))
        }
        input_data <- result$cleaned_data
        # cleaned_summary_df <-
        summary_df <- rbind(summary_df, summary_stats(input_data, 1))
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
      bootstrap_color = if (cb.friendly) "orange" else "yellow"
      p <- canvas_creation(title.font.family, title.font.size, title.font.color, title.hjust,
                           legend.font.family, legend.font.size, legend.font.color, legend.hjust,
                           axis.font.family, axis.font.size, axis.font.color, cb.friendly) +
        with(bootstrap_df, ggplot2::geom_point( ggplot2::aes(x = sq_skewness, y = kurtosis),
                                                color = bootstrap_color, alpha = 0.5, size = 1))
    }
  } else {
    p <- canvas_creation(title.font.family, title.font.size, title.font.color, title.hjust,
                         legend.font.family, legend.font.size, legend.font.color, legend.hjust,
                         axis.font.family, axis.font.size, axis.font.color, cb.friendly)
  }

  plot_data = data.frame(object$points)
  plot_data$shape <- as.factor(22:(22+nrow(plot_data)-1))
  shapes <- c(21, 22, 23, 24, 25, 26)
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
    ggplot2::guides(
      color = ggplot2::guide_legend(title = "Distributions"),
      shape = ggplot2::guide_legend(title = NULL)
    )

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
  print(summary_df)
  # if (csv.file) {print("rens.csv")} #
  if (!is.null(summary.file) && !grepl("\\.csv$", summary.file, ignore.case = TRUE)) {
    summary.file <- paste0(summary.file, ".csv")
    utils::write.csv(summary_df, summary.file)
    message("Summary table is saved in the location: ", summary.file)
  }

  if (!is.null(plot.name)) {
    # print("plot.name is not null")
    ensure_python_dependencies()
    # reticulate::use_condaenv("r-reticulate", required = TRUE)  # Use your conda environment
    # reticulate::py_config()
    # plotly::save_image(p, "./sample_output.pdf")

    if (grepl("\\.pdf$", plot.name)) {
      # plotly::kaleido(p, file = paste0(file.name, ".pdf"))
      plotly::save_image(p, plot.name)
      message("Plot is saved as a PDF file in the location: ", plot.name)
    } else if (grepl("\\.png$", plot.name)) {
      plotly::save_image(p, plot.name)
      message("Plot is saved as a PNG file in the location: ", plot.name)
    } else {
      stop("Unsupported file type. Please use 'pdf' or 'png'.")
    }
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

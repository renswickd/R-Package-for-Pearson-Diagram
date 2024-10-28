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
#' @param cb.friendly Logical, if TRUE uses a colorblind-friendly palette
#' @return A ggplot2 object representing the Pearson diagram canvas.
#' @export
canvas_creation <- function(title.font.family = "Arial", title.font.size = 16, title.font.color = "black", title.hjust=NULL,
                            legend.font.family = "Arial", legend.font.size = 12, legend.font.color = "black", legend.hjust=NULL,
                            axis.font.family = "Arial", axis.font.size = 14, axis.font.color = "black", cb.friendly=FALSE) {

  # # Define color palettes
  # standard_palette <- c(
  #   "Normal" = "#FFCC80",           # Light orange
  #   "Uniform" = "#90CAF9",          # Light blue
  #   "Exponential" = "#64B5F6",      # Blue
  #   "Gamma" = "#A5D6A7",            # Light green
  #   "Inverse Gamma" = "#9575CD",    # Purple
  #   "Beta" = "#EF9A9A",             # Light red
  #   "Beta Area" = "#FFCDD2"         # Very light red for the shaded region
  # )

  standard_palette <- c(
    "Normal" = "#FFA07A",           # Light Salmon
    "Uniform" = "#87CEFA",          # Light Sky Blue
    "Exponential" = "#6495ED",      # Cornflower Blue
    "Gamma" = "#66CDAA",            # Medium Aquamarine
    "Inverse Gamma" = "#BA55D3",    # Medium Orchid
    "Beta" = "#FF6347",             # Tomato
    "Beta Area" = "#FFDAB9"         # Peach Puff for the shaded region
  )



  colorblind_palette <- c(
    "Normal" = "#E69F00",           # Orange
    "Uniform" = "#56B4E9",          # Sky blue
    "Exponential" = "#0072B2",      # Blue
    "Gamma" = "#009E73",            # Green
    "Inverse Gamma" = "#D55E00",    # Reddish-orange
    "Beta" = "#CC79A7",             # Pink
    "Beta Area" = "#F0E442"         # Yellow for shaded area
  )

  # Select the palette based on the colorblind_friendly parameter
  selected_palette <- if (cb.friendly) colorblind_palette else standard_palette

  # Generate known distributions' data
  data <- generate_data()
  area_data <- generate_area_data()
  area_data_limit <- area_data$min_param1
  area_data_region <- area_data$merged_data
  area_data_region <- area_data_region[, c("sq_skewness", "kurtosis_min")]
  area_data_region$kurtosis_max <- c(3, 9)

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
                                                ggplot2::aes(x = sq_skewness, ymin = kurtosis_min, ymax = kurtosis_max, fill = "Beta Area"),
                                                alpha = 0.5)) +
    with(point_data, ggplot2::geom_point(ggplot2::aes(x = sq_skewness, y = kurtosis, color = distribution), size = 5)) +

    # Reverse the y-axis for Kurtosis
    ggplot2::scale_y_reverse() +
    ggplot2::scale_x_continuous()+#limits = c(0,6)) +


    # Axis labels
    ggplot2::xlab(expression(paste("Square of Skewness (", beta[1]^2, ")"))) +
    ggplot2::ylab(expression(paste("Kurtosis (", beta[2], ")"))) +
    ggplot2::theme_minimal() +

    # Custom color and fill scales
    ggplot2::scale_color_manual(name = "Distributions", values = selected_palette) +
    ggplot2::scale_fill_manual(name = NULL, values = selected_palette) +

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

#' Highlight Points on Pearson Diagram with Hover Information
#'
#' This function adds interactive tooltip functionality to Pearson diagram plots using plotly.
#'
#' @param p A ggplot2 object with points plotted on it.
#' @return A plotly object with added interactivity.
#' @export
highlight_point_on_hover <- function(p) {

  # Convert ggplot to plotly for interactivity
  interactive_plot <- plotly::ggplotly(
    p,
    tooltip = c("x", "y", "color","shape")
  )

  return(interactive_plot)
}


#' Add points to the Pearson Diagram from a list of datasets
#'
#' This function adds multiple points to the Pearson Diagram, calculated from the given list of datasets.
#' @param object PearsonDiagram object
#' @param input_list A list of numeric vectors representing datasets
#' @return Updated PearsonDiagram object with new points
#' @export
add_points_from_list <- function(object, input_list) {
  #   for (data in input_list) {
  #     moments <- cpp_calculate_moments(data)
  #     object <- add_point(object, sq_skewness = moments$sq_skewness, kurtosis = moments$kurtosis, distribution = "Input data")
  #   }
  #   return(object)
  # }
  shape_index <- 1

  for (data in input_list) {
    moments <- cpp_calculate_moments(data)
    object <- add_point(
      object,
      sq_skewness = moments$sq_skewness,
      kurtosis = moments$kurtosis,
      distribution = paste0("Sample ", shape_index)
    )
    shape_index <- shape_index + 1
  }
  return(object)
}

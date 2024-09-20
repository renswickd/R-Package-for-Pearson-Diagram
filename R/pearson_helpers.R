#' Validate Input Data
#'
#' This function checks if the input data is valid.
#'
#' @param data A numeric vector containing the data points
#' @return TRUE if the input is valid, otherwise stops with an error
#' @export
validate_input <- function(data) {
  if (!is.numeric(data)) {
    stop("Input data must be numeric.")
  }

  if (length(data) < 3) {
    stop("Input data must contain at least 3 data points.")
  }

  return(TRUE)
}

#' Compare Multiple Distributions on Pearson Diagram
#'
#' This function compares multiple distributions, validates the data, and adds them to the Pearson Diagram.
#'
#' @param object PearsonDiagram object
#' @param data_list A list of numeric vectors
#' @return Updated PearsonDiagram object with new points and a plot
#' @export
compare_multiple_distributions.PearsonDiagram <- function(object, data_list) {
  for (data in data_list) {
    validate_input(data)
    sk_kt <- cpp_calculate_skewness_kurtosis(data)
    object <- add_point(object, sk_kt[1], sk_kt[2])
  }
  plot_diagram(object)
  return(object)
}

#' Highlight Points on Pearson Diagram
#'
#' This function adds interactive tooltip functionality to Pearson diagram plots.
#'
#' @param point A ggplot2 object with points plotted on it
#' @return A modified ggplot2 object
#' @export
highlight_point_on_hover <- function(point) {
  # Placeholder function for future interactivity
  return(point)
}

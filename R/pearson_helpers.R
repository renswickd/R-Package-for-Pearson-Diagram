# Helper file: R/pearson_helpers.R

#' Validate Input Data
#'
#' This function checks if the input data is valid.
#'
#' @param data A numeric vector containing the data points
#' @return TRUE if the input is valid, otherwise stops with an error
#' @examples
#' validate_input(rnorm(100)) # Should return TRUE
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

#' Highlight Points on Pearson Diagram
#'
#' This function adds interactive tooltip functionality to Pearson diagram plots.
#'
#' @param point A ggplot2 object with points plotted on it
#' @return A modified ggplot2 object with tooltip functionality
#' @export
highlight_point_on_hover <- function(point) {
  # Placeholder function - you can use interactive libraries like plotly to enhance interactivity.
  return(point)
}

#' Handle Outliers in the Data
#'
#' This function detects extreme values in a dataset based on z-scores.
#' Values with z-scores above a specified threshold are considered outliers.
#' @param data A numeric vector containing the data points.
#' @param threshold Z-score threshold for detecting outliers (default: 3).
#' @return A list containing the cleaned data (without outliers) and the number of detected outliers.
#' @export
handle_outliers <- function(data, threshold = 3) {
  z_scores <- (data - mean(data)) / stats::sd(data)
  outliers <- abs(z_scores) > threshold
  cleaned_data <- data[!outliers]
  num_outliers <- sum(outliers)
  return(list(cleaned_data = cleaned_data, num_outliers = num_outliers))
}

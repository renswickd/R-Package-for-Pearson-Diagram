#' Bootstrap Samples for PearsonDiagram (S3 Method)
#'
#' This function generates bootstrap samples from the provided input data for a PearsonDiagram object.
#' @param object A PearsonDiagram object
#' @param n_samples The number of bootstrap samples to generate
#' @param sample_size The size of each bootstrap sample (default: same as the length of the input data)
#' @return A list of bootstrap samples with calculated moments
#' @export
bootstrap_samples <- function(object, n_samples = 100, sample_size = NULL) {
  UseMethod("bootstrap_samples")
}

#' Bootstrap Samples for PearsonDiagram
#'
#' @param object A PearsonDiagram object
#' @param n_samples The number of bootstrap samples to generate
#' @param sample_size The size of each bootstrap sample
#' @return A list of bootstrap samples with calculated moments
#' @export
bootstrap_samples.PearsonDiagram <- function(object, n_samples = 100, sample_size = NULL) {
  input_data <- object$points
  sample_size <- if (is.null(sample_size)) nrow(input_data) else sample_size
  bootstrap_results <- replicate(n_samples, cpp_calculate_moments(sample(input_data$sq_skewness, sample_size, replace = TRUE)), simplify = FALSE)
  return(bootstrap_results)
}

#' Bootstrap Samples for Numeric Input
#'
#' This function generates bootstrap samples for numeric vectors directly.
#' @param object A numeric vector
#' @param n_samples The number of bootstrap samples to generate
#' @param sample_size The size of each bootstrap sample
#' @return A list of bootstrap samples with calculated moments
#' @export
bootstrap_samples.numeric <- function(object, n_samples = 100, sample_size = NULL) {
  sample_size <- if (is.null(sample_size)) length(object) else sample_size
  bootstrap_results <- replicate(n_samples, cpp_calculate_moments(sample(object, sample_size, replace = TRUE)), simplify = FALSE)
  return(bootstrap_results)
}

#' Bootstrap Samples for List Input
#'
#' This function generates bootstrap samples for a list of numeric vectors.
#' @param object A list of numeric vectors
#' @param n_samples The number of bootstrap samples to generate
#' @param sample_size The size of each bootstrap sample
#' @return A list of bootstrap samples with calculated moments
#' @export
bootstrap_samples.list <- function(object, n_samples = 100, sample_size = NULL) {
  # Process each numeric vector in the list
  bootstrap_list <- lapply(object, function(data) {
    sample_size <- if (is.null(sample_size)) length(data) else sample_size
    replicate(n_samples, cpp_calculate_moments(sample(data, sample_size, replace = TRUE)), simplify = FALSE)
  })
  return(do.call(c, bootstrap_list))  # Flatten the list of lists into a single list of bootstrap results
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

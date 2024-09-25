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
  if (any(is.na(data))) {
    stop("Input contains missing values.")
  }
  if (any(is.infinite(data))) {
    stop("Input contains infinite values.")
  }
  if (stats::sd(data) == 0) {
    stop("Input data has zero variance, please provide more varied data.")
  }
  return(TRUE)
}


#' Calculate Skewness and Kurtosis for Known Distributions
#'
#' This function calculates the skewness and kurtosis for different known distributions such as Normal, Exponential, Gamma, etc.
#' @param distribution A string representing the type of distribution (e.g., "Normal", "Gamma", etc.)
#' @param params A list of distribution-specific parameters (e.g., shape, rate, etc.)
#' @importFrom magrittr %>%
#' @return A data frame with squared skewness, kurtosis, and distribution name.
#' @export
calculate_moments <- function(distribution, params = list()) {
  if (distribution == "Normal") {
    return(data.frame(sq_skewness = 0, kurtosis = 3, distribution = "Normal"))
  } else if (distribution == "Uniform") {
    return(data.frame(sq_skewness = 0, kurtosis = 1.8, distribution = "Uniform"))
  } else if (distribution == "Exponential") {
    lambda <- params$rate
    skewness <- 2 / sqrt(lambda)
    kurtosis <- 6 / lambda + 3
    return(data.frame(sq_skewness = skewness^2, kurtosis = kurtosis, distribution = "Exponential"))
  } else if (distribution == "Gamma") {
    k <- params$shape
    skewness <- 2 / sqrt(k)
    kurtosis <- 6 / k
    return(data.frame(sq_skewness = skewness^2, kurtosis = kurtosis, distribution = "Gamma"))
  } else if (distribution == "Beta") {
    shape1 <- params$shape1
    shape2 <- params$shape2
    skewness <- 2 * (shape2 - shape1) / sqrt((shape1 + shape2 + 1) * shape1 * shape2)
    kurtosis <- 6 * ((shape1 - shape2)^2 * (shape1 + shape2 + 1) - shape1 * shape2 * (shape1 + shape2 + 2)) /
      (shape1 * shape2 * (shape1 + shape2 + 2) * (shape1 + shape2 + 3))
    return(data.frame(sq_skewness = skewness^2, kurtosis = kurtosis, distribution = "Beta"))
  } else if (distribution == "F") {
    d1 <- params$d1
    d2 <- params$d2
    skewness <- ((2 * d2^2) * (d1 + d2 - 2)) / (d1 * ((d2 - 2)^2) * (d2 - 4))
    kurtosis <- 3 + 12 * ((d1 * (5 * d2 - 22) * (d1 + d2 - 2)) + (d2 - 4) * (d2 - 2)^2) /
      (d1 * (d2 - 6) * (d2 - 8) * (d1 + d2 - 2))
    return(data.frame(sq_skewness = skewness^2, kurtosis = kurtosis, distribution = "F"))
  } else if (distribution == "t") {
    nu <- params$nu
    skewness <- 0
    kurtosis <- 6 / (nu - 4)
    return(data.frame(sq_skewness = skewness^2, kurtosis = kurtosis, distribution = "t"))
  }

  # Return a default data frame if no match
  return(data.frame(sq_skewness = NA, kurtosis = NA, distribution = NA))
}

#' Generate Data for Known Distributions
#'
#' This function generates skewness and kurtosis values for various known distributions using the calculate_moments function.
#' @return A data frame containing skewness, kurtosis, and distribution names for multiple distributions.

#' @export
generate_data <- function() {
  normal_data <- calculate_moments("Normal")
  uniform_data <- calculate_moments("Uniform")

  # Exponential distribution for different rate parameters (line representation)
  exp_data <- do.call(rbind, lapply(seq(0.6, 5, by = 0.1), function(rate) {
    calculate_moments("Exponential", list(rate = rate))
  }))

  # Gamma distribution for different shape parameters (line representation)
  gamma_data <- do.call(rbind, lapply(seq(0.5, 20, by = 0.5), function(k) {
    calculate_moments("Gamma", list(shape = k))
  }))

  # Beta distribution for different shape1 and shape2 parameters (area representation)
  beta_data <- as.data.frame(t(do.call(rbind, expand.grid(shape1 = seq(0.3, 10, by = 0.1), shape2 = seq(0.3, 10, by = 0.1)) %>%
                                         purrr::pmap_dfr(~ calculate_moments("Beta", list(shape1 = .x, shape2 = .y))))))

       # # F-distribution (area representation)
       # f_data <- do.call(t(rbind, expand.grid(d1 = seq(1, 5, by = 1), d2 = seq(8, 14, by = 2)) %>%
       #                     purrr::pmap_dfr(~ calculate_moments("F", list(d1 = .x, d2 = .y)))))
       #
       # # t-distribution for different degrees of freedom (point representation)
       # t_data <- do.call(rbind, lapply(seq(5, 15, by = 1), function(nu) {
       #   calculate_moments("t", list(nu = nu))
       # }))

       # Combine all data and return
       df <- rbind(normal_data, uniform_data, exp_data, gamma_data, beta_data) ##, f_data, t_data)
       df$sq_skewness <- as.numeric(df$sq_skewness)
       df$kurtosis <- as.numeric(df$kurtosis)
       return(df)
}

#' Calculate Moments Using Rcpp
#'
#' This function calculates the skewness and kurtosis using the Rcpp implementation.
#'
#' @param data A numeric vector containing the data points
#' @return A numeric vector with two values: square of skewness and kurtosis
#' @useDynLib pearson.diagram, .registration = TRUE
#' @import Rcpp
#' @export
cpp_calculate_moments <- function(data) {
  sk_kt <- cpp_calculate_skewness_kurtosis(data)
  list(sq_skewness = sk_kt[1]^2, kurtosis = sk_kt[2])
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

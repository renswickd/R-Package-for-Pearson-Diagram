#' Validate Input (S3 Generic)
#'
#' This function serves as a generic method for validating inputs.
#' @param object The object to be validated, which could be a PearsonDiagram object, numeric vector, or list.
#' @return TRUE if the input is valid, otherwise an error will be thrown.
#' @export
validate_input <- function(object) {
  UseMethod("validate_input")
}

#' Validate PearsonDiagram Object
#'
#' This function validates if a PearsonDiagram object has the correct structure.
#' @param object A PearsonDiagram object
#' @return TRUE if valid, otherwise throws an error
#' @export
validate_input.PearsonDiagram <- function(object) {
  if (!is.data.frame(object$points)) {
    stop("Invalid PearsonDiagram object: 'points' must be a data frame.")
  }

  # Check if the required columns exist in the data frame
  if (!all(c("sq_skewness", "kurtosis", "distribution") %in% colnames(object$points))) {
    stop("Invalid PearsonDiagram object: 'points' data frame must contain 'sq_skewness', 'kurtosis', and 'distribution' columns.")
  }

  return(TRUE)
}

#' Validate Numeric Vector
#'
#' This function checks if the input is a valid numeric vector with no missing values or zero variance.
#' @param object A numeric vector
#' @return TRUE if valid, otherwise throws an error
#' @export
validate_input.numeric <- function(object) {
  if (!is.numeric(object)) {
    stop("Input data must be numeric.")
  }
  if (length(object) < 3) {
    stop("Input data must contain at least 3 data points.")
  }
  if (any(is.na(object))) {
    stop("Input contains missing values.")
  }
  if (any(is.infinite(object))) {
    stop("Input contains infinite values.")
  }
  if (stats::sd(object) == 0) {
    stop("Input data has zero variance, please provide more varied data.")
  }

  return(TRUE)
}

#' Validate List of Numeric Vectors
#'
#' This function checks if the input is a valid list of numeric vectors, ensuring each element meets validation criteria.
#' @param object A list of numeric vectors
#' @return TRUE if valid, otherwise throws an error
#' @export
validate_input.list <- function(object) {
  for (vec in object) {
    if (!is.numeric(vec)) {
      stop("Each element in the list must be numeric.")
    }
    if (length(vec) < 3) {
      stop("Each numeric vector must contain at least 3 data points.")
    }
    if (any(is.na(vec))) {
      stop("List contains missing values.")
    }
    if (any(is.infinite(vec))) {
      stop("List contains infinite values.")
    }
    if (stats::sd(vec) == 0) {
      stop("Each numeric vector must have non-zero variance.")
    }
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
    return(data.frame(sq_skewness = 4, kurtosis = 9, distribution = "Exponential"))
  } else if (distribution == "Gamma") {
    k <- params$shape
    gamma_skewness_squared <- (2 / sqrt(k))^2
    gamma_kurtosis <- (6 / k) + 3
    return(data.frame(sq_skewness = gamma_skewness_squared, kurtosis = gamma_kurtosis, distribution = "Gamma"))
  } else if (distribution == "Inverse Gamma") {
    inverse_shape_params <- params$shape
    inverse_gamma_skewness_squared <- (4 * sqrt(inverse_shape_params - 2) / (inverse_shape_params - 3))^2
    inverse_gamma_kurtosis <- 3+ (6 * (5 * inverse_shape_params - 11) / ((inverse_shape_params - 3) * (inverse_shape_params - 4)))

    # Create a dataframe for the Inverse Gamma distribution
    inverse_gamma_distribution <- data.frame(
      # shape_param = inverse_shape_params,
      sq_skewness = inverse_gamma_skewness_squared,
      kurtosis = inverse_gamma_kurtosis,
      distribution = "Inverse Gamma"
    )
    return(inverse_gamma_distribution)
  } else if (distribution == "Beta") {
    alpha_vals <- params$shape1
    beta_vals <- params$shape2

    beta_skewness <- function(alpha, beta) {
      2 * (beta - alpha) * sqrt(alpha + beta + 1) / ((alpha + beta + 2) * sqrt(alpha * beta))
    }

    beta_kurtosis <- function(alpha, beta) {
      6 * ((alpha - beta)^2 * (alpha + beta + 1) - alpha * beta * (alpha + beta + 2)) /
        (alpha * beta * (alpha + beta + 2) * (alpha + beta + 3))
    }
    # skewness <- 2 * (shape2 - shape1) / sqrt((shape1 + shape2 + 1) * shape1 * shape2)
    # kurtosis <- 6 * ((shape1 - shape2)^2 * (shape1 + shape2 + 1) - shape1 * shape2 * (shape1 + shape2 + 2)) /
    #   (shape1 * shape2 * (shape1 + shape2 + 2) * (shape1 + shape2 + 3))

    beta_skewness_squared <- c()
    beta_kurtosis_values <- c()
    param1 <- c()
    param2 <- c()

    # Calculate skewness and kurtosis for each pair of alpha and beta
    for (alpha in alpha_vals) {
      for (beta in beta_vals) {
        skew <- beta_skewness(alpha, beta)
        kurt <- beta_kurtosis(alpha, beta)

        # Store squared skewness and kurtosis values
        beta_skewness_squared <- c(beta_skewness_squared, skew^2)
        beta_kurtosis_values <- c(beta_kurtosis_values, kurt)
        param1 <- c(param1, alpha)
        param2 <- c(param2, beta)
      }
    }

    # Create a dataframe for the Beta distribution results
    beta_distribution <- data.frame(
      sq_skewness = beta_skewness_squared,
      kurtosis = beta_kurtosis_values + 3,
      param1 = param1,
      param2 = param2
    )
    min_param1 <- subset(beta_distribution, param1 == min(beta_distribution$param1))
    max_param1 <- subset(beta_distribution, param1 == max(beta_distribution$param1))
    merged_data <- merge(min_param1, max_param1, by = "sq_skewness", suffixes = c("_min", "_max"))

    # return(data.frame(sq_skewness = skewness^2, kurtosis = kurtosis, distribution = "Beta"))
    return(list(min_param1 = min_param1, merged_data = merged_data))
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
  exp_data <- calculate_moments("Exponential")

  # # Exponential distribution for different rate parameters (line representation)
  # exp_data <- do.call(rbind, lapply(seq(0.6, 5, by = 0.1), function(rate) {
  #   calculate_moments("Exponential", list(rate = rate))
  # }))

  # Gamma distribution for different shape parameters (line representation)
  gamma_data <- do.call(rbind, lapply(seq(0.5, 20, by = 0.1), function(k) {
    calculate_moments("Gamma", list(shape = k))
  }))

  inverse_gamma_data <- do.call(rbind, lapply(seq(6, 100, length.out = 1000), function(k) {
    calculate_moments("Inverse Gamma", list(shape = k))
  }))

  # Combine all data and return
  df <- rbind(normal_data, uniform_data, exp_data, gamma_data, inverse_gamma_data)#, beta_data) ##, f_data, t_data)
  df$sq_skewness <- as.numeric(df$sq_skewness)
  df$kurtosis <- as.numeric(df$kurtosis)
  return(df)
}

#' Generate Data for Known Distributions - Area Representations
#'
#' This function generates skewness and kurtosis values for known distributions representing area in pearson diagram using the calculate_moments function.
#' @return A data frame containing skewness, kurtosis, and parameter space for multiple distributions.
#' @export
generate_area_data <- function() {
  return(calculate_moments("Beta", list(shape1=seq(0.48, 100, length.out = 100), shape2=seq(0.48, 100, length.out = 100))))
}

#' Calculate Moments Using Rcpp for a List of Vectors
#'
#' This function calculates skewness and kurtosis using Rcpp for a list of numeric vectors.
#'
#' @param data_list A list of numeric vectors containing data points.
#' @return A list containing the skewness and kurtosis for each vector.
#' @export
cpp_calculate_moments_for_list <- function(data_list) {
  moments_list <- lapply(data_list, function(data) {
    cpp_calculate_moments(data)
  })
  return(moments_list)
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


#' Summary Statistics for Numeric Inputs
#'
#' This function calculates summary statistics for numeric inputs, which can either be a numeric vector or a list of numeric vectors.
#' The summary statistics include the number of observations, number of missing values, mean, median, and standard deviation.
#'
#' @param x A numeric vector or a list of numeric vectors for which the summary statistics are to be calculated.
#' @param censored binary value indicating if the outliers are removed from the input
#' @return A data frame containing the summary statistics. If the input is a vector, the row is labeled as "sample1".
#'         If the input is a list, each row corresponds to a sample (e.g., "sample1", "sample2", etc.), and the summary statistics are computed for each vector.
#' @export
summary_stats <- function(x, censored) {
  calc_summary <- function(vec) {
    num_obs <- length(vec)                # Number of observations
    num_missing <- sum(is.na(vec))        # Number of missing values
    mean_val <- round(mean(vec, na.rm = TRUE),4)   # Mean
    median_val <- round(stats::median(vec, na.rm = TRUE),4)  # Median
    sd_val <- round(stats::sd(vec, na.rm = TRUE),4)       # Standard deviation
    moments <- cpp_calculate_moments(vec)
    skew_val <- round(moments$sq_skewness, 4)
    kurt_val <- round(moments$kurtosis, 4)
    return(c(num_obs, num_missing, mean_val, median_val, sd_val, skew_val, kurt_val))
  }

  results <- list()
  if (is.numeric(x)) {
    results[[1]] <- c("sample1", censored, calc_summary(x))
  } else if (is.list(x)) {

    for (i in seq_along(x)) {
      if (is.numeric(x[[i]])) {
        results[[i]] <- c(paste0("sample", i), censored, calc_summary(x[[i]]))
      } else {
        stop("All elements of the list must be numeric vectors.")
      }
    }
  } else {
    stop("Input must be a numeric vector or a list of numeric vectors.")
  }
  result_df <- as.data.frame(do.call(rbind, results), stringsAsFactors = FALSE)
  colnames(result_df) <- c("input", "censored" ,"n", "n_missing", "mean", "median", "sd", "skewness", "kurtosis")
  result_df[, 2:6] <- lapply(result_df[, 2:6], as.numeric)

  return(result_df)
}


#' Ensure required Python dependencies are installed
#'
#' This function ensures that the necessary Python packages for rendering plots
#' are installed (kaleido and plotly).
#' It uses reticulate to install the packages via conda.
#' @export
ensure_python_dependencies <- function() {
  if (!reticulate::py_module_available("kaleido")) {
    message("Installing 'kaleido' via conda...")
    reticulate::conda_install('r-reticulate', 'python-kaleido')
  }

  if (!reticulate::py_module_available("plotly")) {
    message("Installing 'plotly' via conda...")
    reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
  }
}

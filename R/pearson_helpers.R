#' Validate Input Data
#'
#' This function checks if the input data or PearsonDiagram object is valid.
#' It validates numeric vectors or lists of numeric vectors for input data or a PearsonDiagram object with valid points data.
#'
#' @param data A numeric vector, a list of numeric vectors containing the data points, or a PearsonDiagram object.
#' @return TRUE if the input is valid, otherwise stops with an error.
#' @export
validate_input <- function(data) {
  # Check if the data is a PearsonDiagram object
  if (inherits(data, "PearsonDiagram")) {
    if (!is.data.frame(data$points)) {
      stop("Invalid PearsonDiagram object: 'points' must be a data frame.")
    }
    return(TRUE)
  }

  # If the input is a list, validate each element in the list
  if (is.list(data)) {
    for (vec in data) {
      if (!is.numeric(vec)) {
        stop("Each element in the list must be numeric.")
      }
      if (length(vec) < 3) {
        stop("Each numeric vector must contain at least 3 data points.")
      }
      if (any(is.na(vec))) {
        stop("Input contains missing values.")
      }
      if (any(is.infinite(vec))) {
        stop("Input contains infinite values.")
      }
      if (stats::sd(vec) == 0) {
        stop("Each numeric vector must have non-zero variance.")
      }
    }
    return(TRUE)
  }

  # Validate if the input is a single numeric vector
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

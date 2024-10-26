# test_that("validate_input works with PearsonDiagram object", {
#   pd <- PearsonDiagram()
#   pd <- add_point(pd, sq_skewness = 0.5, kurtosis = 3.0, distribution = "Sample")
#   expect_true(validate_input(pd))
# })
#
# test_that("validate_input throws error for invalid PearsonDiagram", {
#   pd <- PearsonDiagram()
#   pd$points <- NULL  # Invalid PearsonDiagram
#   expect_error(validate_input(pd), "Invalid PearsonDiagram object")
# })
#
# test_that("validate_input handles numeric vector", {
#   input_data <- rnorm(100)
#   expect_true(validate_input(input_data))
# })
#
# test_that("validate_input handles list of numeric vectors", {
#   input_list <- list(rnorm(50), rnorm(50, mean = 5))
#   expect_true(validate_input(input_list))
# })
#
# test_that("validate_input throws error for invalid numeric data", {
#   input_data <- c(1, 2, NA)
#   expect_error(validate_input(input_data), "Input contains missing values")
# })


library(testthat)

# Test validate_input() generic and methods

test_that("validate_input.PearsonDiagram validates correct PearsonDiagram object", {
  pd <- PearsonDiagram()
  pd <- add_point(pd, sq_skewness = 0.5, kurtosis = 2.5, distribution = "Sample")

  expect_true(validate_input(pd))  # Valid object
})

test_that("validate_input.PearsonDiagram throws error for invalid PearsonDiagram", {
  invalid_pd <- list(points = data.frame(a = c(1, 2, 3)))  # Missing required columns
  class(invalid_pd) <- "PearsonDiagram"

  expect_error(validate_input(invalid_pd), "Invalid PearsonDiagram object")
})

test_that("validate_input.numeric validates correct numeric vector", {
  valid_vector <- c(1.2, 3.4, 5.6)
  expect_true(validate_input(valid_vector))  # Valid numeric vector
})

test_that("validate_input.numeric throws error for invalid numeric vector", {
  invalid_vector <- c(1.2, NA, 5.6)  # Contains NA
  expect_error(validate_input(invalid_vector), "Input contains missing values")

  invalid_vector <- c(1.2, Inf, 5.6)  # Contains Inf
  expect_error(validate_input(invalid_vector), "Input contains infinite values")

  zero_variance_vector <- c(1, 1, 1)  # Zero variance
  expect_error(validate_input(zero_variance_vector), "Input data has zero variance")
})

test_that("validate_input.list validates correct list of numeric vectors", {
  valid_list <- list(c(1.2, 3.4, 5.6), c(2.3, 4.5, 6.7))
  expect_true(validate_input(valid_list))  # Valid list
})

test_that("validate_input.list throws error for invalid list of numeric vectors", {
  invalid_list <- list(c(1.2, 3.4, NA), c(2.3, 4.5, 6.7))  # Contains NA
  expect_error(validate_input(invalid_list), "List contains missing values")
})

# Test calculate_moments() for known distributions

test_that("calculate_moments calculates moments for known distributions", {
  moments <- calculate_moments("Normal")
  expect_equal(moments$sq_skewness, 0)
  expect_equal(moments$kurtosis, 3)

  moments <- calculate_moments("Exponential")
  expect_equal(moments$sq_skewness, 4)
  expect_equal(moments$kurtosis, 9)

  # moments <- calculate_moments("Gamma", list(shape = 2))
  # expect_equal(moments$sq_skewness, 1)
  # expect_equal(moments$kurtosis, 6)
})


# Test generate_data()

test_that("generate_data creates correct data frame of known distributions", {
  df <- generate_data()
  expect_true("Normal" %in% df$distribution)
  expect_true("Gamma" %in% df$distribution)
  expect_true(is.data.frame(df))
  expect_gt(nrow(df), 0)  # Ensure data is generated
})

# Test summary_stats()

test_that("summary_stats calculates summary statistics for numeric vector", {
  vec <- c(1.2, 2.3, 3.4, 4.5)
  summary <- summary_stats(vec, censored = 0)

  expect_equal(summary$n, 4)
  expect_equal(summary$n_missing, 0)
  expect_equal(summary$mean, round(mean(vec), 4))
  expect_equal(as.numeric(summary$sd), round(sd(vec), 4))
  expect_equal(as.numeric(summary$skewness), round(cpp_calculate_moments(vec)$sq_skewness, 4))
  expect_equal(as.numeric(summary$kurtosis), round(cpp_calculate_moments(vec)$kurtosis, 4))
})

test_that("summary_stats calculates summary for list of numeric vectors", {
  list_vec <- list(c(1.2, 2.3, 3.4), c(4.5, 5.6, 6.7))
  summary <- summary_stats(list_vec, censored = 0)

  expect_equal(nrow(summary), 2)
  expect_equal(summary$mean[1], round(mean(list_vec[[1]]), 4))
  expect_equal(summary$mean[2], round(mean(list_vec[[2]]), 4))
})

test_that("summary_stats throws error for invalid input", {
  expect_error(summary_stats("invalid", censored = 0), "Input must be a numeric vector or a list")
})

# Test cpp_calculate_moments()

test_that("cpp_calculate_moments calculates correct skewness and kurtosis for numeric vector", {
  vec <- c(1.2, 3.4, 5.6, 7.8)
  moments <- cpp_calculate_moments(vec)

  expect_true(is.list(moments))
  expect_true(!is.na(moments$sq_skewness))
  expect_true(!is.na(moments$kurtosis))
})


# Test calculate_moments_for_list()
test_that("cpp_calculate_moments_for_list calculates moments for list of numeric vectors", {
  list_vec <- list(c(1.2, 2.3, 3.4), c(4.5, 5.6, 6.7))
  moments_list <- cpp_calculate_moments_for_list(list_vec)

  expect_equal(length(moments_list), 2)
  expect_true(is.list(moments_list[[1]]))
  expect_true(is.list(moments_list[[2]]))
})




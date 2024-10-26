library(testthat)

# Test bootstrap_samples for PearsonDiagram object

test_that("bootstrap_samples.PearsonDiagram generates bootstrap samples", {
  pd <- PearsonDiagram()
  pd <- add_point(pd, sq_skewness = 0.5, kurtosis = 2.5, distribution = "Sample")

  samples <- bootstrap_samples(pd, n_samples = 10)

  expect_true(is.list(samples))
  expect_equal(length(samples), 10)
  expect_true(is.list(samples[[1]]))
  expect_true("sq_skewness" %in% names(samples[[1]]))
})

# Test bootstrap_samples for numeric input

test_that("bootstrap_samples.numeric generates bootstrap samples for numeric vector", {
  input_data <- c(1.2, 2.3, 3.4)
  samples <- bootstrap_samples(input_data, n_samples = 10)

  expect_true(is.list(samples))
  expect_equal(length(samples), 10)
  expect_true(is.list(samples[[1]]))
  expect_true("sq_skewness" %in% names(samples[[1]]))
})


# Test bootstrap_samples for list input

test_that("bootstrap_samples.list generates bootstrap samples for list of numeric vectors", {
  input_list <- list(c(1.2, 2.3, 3.4), c(2.1, 3.4, 5.6))
  samples <- bootstrap_samples(input_list, n_samples = 10)

  expect_true(is.list(samples))
  expect_gt(length(samples), 10)  # Should be more than 10 because of multiple vectors in the list
})


# # Test calculate_bootstrap_moments
#
# test_that("calculate_bootstrap_moments calculates skewness and kurtosis for bootstrap samples", {
#   input_data <- c(1.2, 2.3, 3.4)
#   samples <- bootstrap_samples(input_data, n_samples = 10)
#   moments <- calculate_bootstrap_moments(samples)
#
#   expect_true(is.data.frame(moments))
#   expect_equal(nrow(moments), 10)
#   expect_true("sq_skewness" %in% colnames(moments))
#   expect_true("kurtosis" %in% colnames(moments))
# })

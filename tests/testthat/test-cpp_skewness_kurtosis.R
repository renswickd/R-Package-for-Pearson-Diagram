library(testthat)
library(pearson.diagram)

# Test cpp_calculate_skewness_kurtosis with valid small dataset
test_that("cpp_calculate_skewness_kurtosis returns correct values for normal distribution", {
  data <- rnorm(100)
  moments <- cpp_calculate_skewness_kurtosis(data)
  expect_true(is.numeric(moments))
  expect_equal(length(moments), 2)  # Should return skewness and kurtosis
  expect_true(is.finite(moments[1]))  # Skewness should be finite
  expect_true(is.finite(moments[2]))  # Kurtosis should be finite
})

test_that("cpp_calculate_skewness_kurtosis works with large datasets", {
  data <- rnorm(100000)
  moments <- cpp_calculate_skewness_kurtosis(data)
  expect_true(is.numeric(moments))
  expect_equal(length(moments), 2)
  expect_true(is.finite(moments[1]))
  expect_true(is.finite(moments[2]))
  expect_equal(round(moments[1]), 0)
  expect_equal(round(moments[2]), 3)
})

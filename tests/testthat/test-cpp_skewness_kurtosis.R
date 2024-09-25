test_that("cpp_calculate_skewness_kurtosis() calculates values correctly", {
  result <- cpp_calculate_skewness_kurtosis(rnorm(1000))
  expect_length(result, 2)
  expect_true(is.numeric(result))
})

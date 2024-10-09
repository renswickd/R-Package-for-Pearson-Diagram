# library(testthat)
# library(pearson.diagram)
#
# # Test validate_input with correct inputs
# test_that("validate_input works with valid inputs", {
#   expect_true(validate_input(runif(10)))  # Random uniform distribution
#   expect_true(validate_input(rnorm(100)))  # Random normal distribution
# })
#
# # Test validate_input with invalid inputs
# test_that("validate_input catches invalid inputs", {
#   expect_error(validate_input(c(NA, 1, 2)), "Input contains missing values")
#   expect_error(validate_input(c(Inf, 1, 2)), "Input contains infinite values")
#   expect_error(validate_input("string"), "Input data must be numeric")
#   expect_error(validate_input(numeric(1)), "Input data must contain at least 3 data points")
#   expect_error(validate_input(rep(5, 10)), "Input data has zero variance")
# })
#
# # # Test calculate_moments function for known distributions
# # test_that("calculate_moments returns correct moments for known distributions", {
# #   moments <- calculate_moments("Normal")
# #   expect_equal(moments$sq_skewness, 0)
# #   expect_equal(moments$kurtosis, 3)
# #
# #   moments_gamma <- calculate_moments("Gamma", list(shape = 2))
# #   expect_equal(moments_gamma$sq_skewness, (2 / sqrt(2))^2)
# #   expect_equal(moments_gamma$kurtosis, 6 / 2)
# # })
#
# # Test calculate_moments with incorrect distribution name
# test_that("calculate_moments throws an error for unknown distribution", {
#   expect_equal(calculate_moments("Unknown"), data.frame(sq_skewness = NA, kurtosis = NA, distribution = NA))
# })
#
# # Test cpp_calculate_moments with valid and invalid data
# test_that("cpp_calculate_moments works with valid data", {
#   data <- rnorm(1000)
#   moments <- cpp_calculate_moments(data)
#   expect_true(is.list(moments))
#   expect_true(length(moments) == 2)  # Expect two values: sq_skewness and kurtosis
#   expect_true(is.numeric(moments$sq_skewness))
#   expect_true(is.numeric(moments$kurtosis))
# })
#

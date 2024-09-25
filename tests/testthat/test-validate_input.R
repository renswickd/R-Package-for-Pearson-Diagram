test_that("validate_input() works correctly", {
  expect_error(validate_input(c(NA, 1, 2)), "Input contains missing values.")
  expect_error(validate_input(c(1, 1, 1)), "Input data has zero variance.")
  expect_error(validate_input(c(Inf, 1, 2)), "Input contains infinite values.")
  expect_error(validate_input(1:2), "Input data must contain at least 3 data points.")
  expect_true(validate_input(c(1, 2, 3)))
})

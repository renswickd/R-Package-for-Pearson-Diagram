# Test handle_outliers()
test_that("handle_outliers removes outliers correctly", {
  data <- c(1, 2, 3, 100, 5, 6)
  result <- handle_outliers(data, threshold = 2)

  expect_equal(result$num_outliers, 1)
  expect_equal(length(result$cleaned_data), 5)
  expect_false(100 %in% result$cleaned_data)
})

test_that("handle_outliers handles no outliers", {
  data <- c(1, 2, 3, 4, 5)
  result <- handle_outliers(data)

  expect_equal(result$num_outliers, 0)
  expect_equal(length(result$cleaned_data), 5)
})

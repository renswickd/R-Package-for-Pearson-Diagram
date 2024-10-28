# test_that("plot_diagram handles numeric vector input", {
#   input_data <- rnorm(100)
#   p <- plot_diagram(input_data = input_data)
#   expect_type(p, "list")
# })
#
# test_that("plot_diagram handles list input and bootstrap", {
#   input_data <- list(rnorm(100), rnorm(50, mean = 5))
#   p <- plot_diagram(input_data = input_data, bootstrap = TRUE)
#   expect_type(p, "list")
# })


test_that("plot_diagram throws an error with invalid title.font.family", {
  data_vector <- c(1.2, 2.3, 3.4, 4.5)
  expect_error(
    plot_diagram(input_data = data_vector, title.font.family = 123),
    "Invalid value for title.font.family - expected character"
  )
})

test_that("plot_diagram throws an error with invalid title.font.size", {
  data_vector <- c(1.2, 2.3, 3.4, 4.5)
  expect_error(
    plot_diagram(input_data = data_vector, title.font.size = -10),
    "Invalid value for title.font.size - expected numeric and valid value"
  )
})

test_that("plot_diagram throws an error with invalid title.hjust", {
  data_vector <- c(1.2, 2.3, 3.4, 4.5)
  expect_error(
    plot_diagram(input_data = data_vector, title.hjust = 1.5),
    "Invalid value for title.hjust - expected numeric and valid value"
  )
})

# Test invalid legend customization parameters

test_that("plot_diagram throws an error with invalid legend.font.family", {
  data_vector <- c(1.2, 2.3, 3.4, 4.5)
  expect_error(
    plot_diagram(input_data = data_vector, legend.font.family = TRUE),
    "Invalid value for legend.font.family - expected character"
  )
})

test_that("plot_diagram throws an error with invalid legend.font.size", {
  data_vector <- c(1.2, 2.3, 3.4, 4.5)
  expect_error(
    plot_diagram(input_data = data_vector, legend.font.size = "large"),
    "Invalid value for legend.font.size - expected numeric"
  )
})

test_that("plot_diagram throws an error with invalid legend.hjust", {
  data_vector <- c(1.2, 2.3, 3.4, 4.5)
  expect_error(
    plot_diagram(input_data = data_vector, legend.hjust = -1),
    "Invalid value for legend.hjust - expected numeric and valid value"
  )
})

# Test invalid axis customization parameters

test_that("plot_diagram throws an error with invalid axis.font.family", {
  data_vector <- c(1.2, 2.3, 3.4, 4.5)
  expect_error(
    plot_diagram(input_data = data_vector, axis.font.family = NULL),
    "Invalid value for axis.font.family - expected character"
  )
})

test_that("plot_diagram throws an error with invalid axis.font.size", {
  data_vector <- c(1.2, 2.3, 3.4, 4.5)
  expect_error(
    plot_diagram(input_data = data_vector, axis.font.size = -5),
    "Invalid value for axis.font.size - expected numeric and valid value"
  )
})

test_that("plot_diagram throws an error with invalid axis.font.color", {
  data_vector <- c(1.2, 2.3, 3.4, 4.5)
  expect_error(
    plot_diagram(input_data = data_vector, axis.font.color = 123),
    "Invalid value for axis.font.color - expected character"
  )
})

test_that("plot_diagram throws an error with invalid summary.file", {
  data_vector <- c(1.2, 2.3, 3.4, 4.5)
  expect_error(
    plot_diagram(input_data = data_vector, summary.file = 123),
    "Invalid value for summary.file - expected character"
  )
})

test_that("plot_diagram throws an error with invalid plot.name", {
  data_vector <- c(1.2, 2.3, 3.4, 4.5)
  expect_error(
    plot_diagram(input_data = data_vector, plot.name = 123),
    "Invalid value for plot.name - expected character"
  )
})

# test_that("plot_diagram throws an error for unsupported plot.name file type", {
#   data_vector <- c(1.2, 2.3, 3.4, 4.5)
#   expect_error(
#     plot_diagram(input_data = data_vector, plot.name = "plot.bmp"),
#     "Unsupported file type. Please use 'pdf' or 'png'."
#   )
# })

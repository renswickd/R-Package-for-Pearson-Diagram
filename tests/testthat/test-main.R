test_that("plot_diagram handles numeric vector input", {
  input_data <- rnorm(100)
  p <- plot_diagram(input_data = input_data)
  expect_type(p, "list")
})

test_that("plot_diagram handles list input and bootstrap", {
  input_data <- list(rnorm(100), rnorm(50, mean = 5))
  p <- plot_diagram(input_data = input_data, bootstrap = TRUE)
  expect_type(p, "list")
})

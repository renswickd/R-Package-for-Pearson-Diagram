library(testthat)
library(pearson.diagram)  # Ensure your package is correctly named

# Test for PearsonDiagram creation
test_that("PearsonDiagram object is created correctly", {
  pd <- PearsonDiagram()
  expect_true(inherits(pd, "PearsonDiagram"))
  expect_equal(ncol(pd$points), 3)
  expect_true(all(c("sq_skewness", "kurtosis", "distribution") %in% colnames(pd$points)))
  expect_equal(nrow(pd$points), 0)
})

# Test adding a single point to PearsonDiagram
test_that("add_point correctly adds a single point", {
  pd <- PearsonDiagram()
  pd <- add_point(pd, 0.6, 3.2, "Normal")
  expect_equal(nrow(pd$points), 1)
  expect_equal(pd$points$sq_skewness, 0.6)
  expect_equal(pd$points$kurtosis, 3.2)
  expect_equal(pd$points$distribution, "Normal")
})

# Test adding multiple points to PearsonDiagram
test_that("add_point correctly adds multiple points", {
  pd <- PearsonDiagram()
  pd <- add_point(pd, 0.6, 3.2, "Normal")
  pd <- add_point(pd, 0.4, 3.8, "Exponential")
  expect_equal(nrow(pd$points), 2)
  expect_equal(pd$points$sq_skewness[2], 0.4)
  expect_equal(pd$points$kurtosis[2], 3.8)
  expect_equal(pd$points$distribution[2], "Exponential")
})

# Test plot_diagram with a single dataset
test_that("plot_diagram works with a single dataset", {
  pd <- PearsonDiagram()
  pd <- add_point(pd, 0.5, 3.2, "Normal")
  plot <- plot_diagram(pd)  # Should return a ggplot object
  expect_true("ggplot" %in% class(plot))
})

# Test plot_diagram with multiple datasets
test_that("plot_diagram works with multiple datasets", {
  pd <- PearsonDiagram()
  plot <- plot_diagram(pd, input_data = list(data1=rnorm(100), data2=rgamma(100, shape = 2)))  # Should return a ggplot object
  expect_true("ggplot" %in% class(plot))
})

# Test plot_diagram with hover enabled (place holder hover functionality)
test_that("plot_diagram works with hover enabled", {
  pd <- PearsonDiagram()
  plot <- plot_diagram(pd, input_data = rnorm(1000), hover = TRUE)
  expect_true("ggplot" %in% class(plot))
})

library(testthat)

# Test PearsonDiagram creation

test_that("PearsonDiagram creates an empty PearsonDiagram object", {
  pd <- PearsonDiagram()
  expect_true(inherits(pd, "PearsonDiagram"))
  expect_true(is.data.frame(pd$points))
  expect_equal(ncol(pd$points), 3)  # sq_skewness, kurtosis, distribution
  expect_equal(nrow(pd$points), 0)  # Empty initially
})

# Test add_point for PearsonDiagram

test_that("add_point.PearsonDiagram adds a point to PearsonDiagram", {
  pd <- PearsonDiagram()
  pd <- add_point(pd, sq_skewness = 0.5, kurtosis = 2.5, distribution = "Sample")

  expect_equal(nrow(pd$points), 1)
  expect_equal(pd$points$sq_skewness[1], 0.5)
  expect_equal(pd$points$kurtosis[1], 2.5)
  expect_equal(pd$points$distribution[1], "Sample")
})

# Test add_points_from_list

test_that("add_points_from_list adds multiple points from list to PearsonDiagram", {
  input_list <- list(c(1.2, 2.3, 3.4), c(4.5, 5.6, 6.7))
  pd <- PearsonDiagram()
  pd <- add_points_from_list(pd, input_list)

  expect_equal(nrow(pd$points), 2)  # Two samples
  expect_equal(pd$points$distribution[1], "Sample 1")
  expect_equal(pd$points$distribution[2], "Sample 2")
})


# Test canvas_creation

test_that("canvas_creation creates a valid ggplot2 object", {
  p <- canvas_creation()
  expect_true(inherits(p, "ggplot"))
  expect_equal(length(p$layers), 5)  # Check the layers used for the plot (distributions and ribbons)
})

test_that("canvas_creation handles different font settings", {
  p <- canvas_creation(title.font.size = 20, legend.font.size = 15, axis.font.size = 10)

  expect_true(inherits(p, "ggplot"))
  expect_equal(p$theme$plot.title$size, 20)
  expect_equal(p$theme$legend.title$size, 15)
  expect_equal(p$theme$axis.title$size, 10)
})

# Test highlight_point_on_hover

test_that("highlight_point_on_hover adds interactivity with plotly", {
  p <- canvas_creation()
  plotly_obj <- highlight_point_on_hover(p)

  expect_true(inherits(plotly_obj, "plotly"))
})

test_that("highlight_point_on_hover handles empty plot", {
  p <- ggplot2::ggplot()  # Empty ggplot object
  plotly_obj <- highlight_point_on_hover(p)

  expect_true(inherits(plotly_obj, "plotly"))
})

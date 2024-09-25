test_that("generate_data() generates correct known distribution data", {
  data <- generate_data()
  expect_true("sq_skewness" %in% colnames(data))
  expect_true("kurtosis" %in% colnames(data))
  expect_true("distribution" %in% colnames(data))
  expect_true(nrow(data) > 0)
})

test_that("plot_diagram.PearsonDiagram() creates the plot with valid data", {
  pd <- PearsonDiagram()
  pd <- add_point(pd, 1, 3)

  expect_silent(plot_diagram(pd))
})

test_that("add_point() adds points correctly", {
  pd <- PearsonDiagram()
  expect_equal(nrow(pd$points), 0)

  pd <- add_point(pd, 1, 3)
  expect_equal(nrow(pd$points), 1)
  expect_equal(pd$points$sq_skewness[1], 1)
  expect_equal(pd$points$kurtosis[1], 3)
})

test_that("Plot Pearson Diagram works with new data", {
  pd <- PearsonDiagram()

  new_data <- rnorm(1000)
  sk_kt <- cpp_calculate_skewness_kurtosis(new_data)

  pd <- add_point(pd, sk_kt[1], sk_kt[2])

  expect_silent(plot_diagram(pd))
})

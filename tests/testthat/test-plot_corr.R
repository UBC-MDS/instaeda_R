input_df <- palmerpenguins::penguins

test_that("resulting mark is geom_tile", {
  expect_equal(class(plot_corr(input_df)$layers[[1]]$geom)[1], "GeomTile")
})

test_that("map 'variable_1' to x-axis,
          'variable_2' to y-axis,
          'corr' to colour", {
  expect_true(rlang::get_expr(
    plot_corr(input_df)$mapping$x
  ) == as.symbol("variable_1"))
  expect_true(rlang::get_expr(
    plot_corr(input_df)$mapping$y
  ) == as.symbol("variable_2"))
  expect_true(rlang::get_expr(
    plot_corr(input_df)$mapping$fill
  ) == as.symbol("corr"))
})

test_that("range of correlation values between -1, 1", {
  expect_equal(plot_corr(input_df)$scales$scales[[1]]$limits[1], -1)
  expect_equal(plot_corr(input_df)$scales$scales[[1]]$limits[2], 1)
})

test_that("Input is a dataframe", {
  expect_error(plot_corr(c("a", "b")), "Data provided is not a data frame")
})

test_that("Input must contain >= 2 numeric columns", {
  expect_error(
    plot_corr(data.frame(a = c("a", "b", "c"), b = c("d", "e", "f"))),
    "Need at least two numeric columns to calculate correlation"
  )
})

test_that("Correlation method must be appropriate", {
  expect_error(
    plot_corr(input_df, method = "kendll"),
    "Correlation method not acceptable"
  )
})

test_that("Colour palette not diverging", {
  expect_warning(
    plot_corr(input_df, colour_palette = "Blues"),
    "Recommended ggplot continuous diverging colour palette"
  )
})

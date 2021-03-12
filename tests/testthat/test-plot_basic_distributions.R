library(testthat)
library(instaeda)
library(palmerpenguins)


input_df <- palmerpenguins::penguins
basic_distb_plots <- plot_basic_distributions(input_df)

test_that("8 Plots are generated", {
  expect_equal(length(basic_distb_plots), 8)
})

test_that("Character or factor feature plots", {
  expect_equal(basic_distb_plots[['island']]$labels$x, 'island')
  expect_equal(basic_distb_plots[['species']]$labels$x, 'species')
  expect_equal(basic_distb_plots[['sex']]$labels$x, 'sex')
})


test_that("Numeric plots", {
  expect_equal(basic_distb_plots[['bill_length_mm']]$labels$x, 'bill_length_mm')
  expect_equal(basic_distb_plots[['bill_depth_mm']]$labels$x, 'bill_depth_mm')
  expect_equal(basic_distb_plots[['flipper_length_mm']]$labels$x, 'flipper_length_mm')
  expect_equal(basic_distb_plots[['body_mass_g']]$labels$x, 'body_mass_g')
  expect_equal(basic_distb_plots[['year']]$labels$x, 'year')
})

test_that("All plots using GeomBar", {
  expect_equal(sapply(basic_distb_plots[['island']]$layers, function(x) class(x$geom)[1]), 'GeomBar')
  expect_equal(sapply(basic_distb_plots[['species']]$layers, function(x) class(x$geom)[1]), 'GeomBar')
  expect_equal(sapply(basic_distb_plots[['bill_length_mm']]$layers, function(x) class(x$geom)[1]), 'GeomBar')
  expect_equal(sapply(basic_distb_plots[['bill_depth_mm']]$layers, function(x) class(x$geom)[1]), 'GeomBar')
  expect_equal(sapply(basic_distb_plots[['flipper_length_mm']]$layers, function(x) class(x$geom)[1]), 'GeomBar')
  expect_equal(sapply(basic_distb_plots[['body_mass_g']]$layers, function(x) class(x$geom)[1]), 'GeomBar')
  expect_equal(sapply(basic_distb_plots[['sex']]$layers, function(x) class(x$geom)[1]), 'GeomBar')
  expect_equal(sapply(basic_distb_plots[['year']]$layers, function(x) class(x$geom)[1]), 'GeomBar')
})


# test_that("resulting mark is geom_tile", {
#   expect_equal(class(plot_corr(input_df)$layers[[1]]$geom)[1], "GeomTile")
# })
#
# test_that("map 'variable_1' to x-axis,
#           'variable_2' to y-axis,
#           'corr' to colour", {
#             expect_true(rlang::get_expr(
#               plot_corr(input_df)$mapping$x
#             ) == as.symbol("variable_1"))
#             expect_true(rlang::get_expr(
#               plot_corr(input_df)$mapping$y
#             ) == as.symbol("variable_2"))
#             expect_true(rlang::get_expr(
#               plot_corr(input_df)$mapping$fill
#             ) == as.symbol("corr"))
#           })
#
# test_that("range of correlation values between -1, 1", {
#   expect_equal(plot_corr(input_df)$scales$scales[[1]]$limits[1], -1)
#   expect_equal(plot_corr(input_df)$scales$scales[[1]]$limits[2], 1)
# })
#
# test_that("Input is a dataframe", {
#   expect_error(plot_corr(c("a", "b")), "Data provided is not a data frame")
# })
#
# test_that("Input must contain >= 2 numeric columns", {
#   expect_error(
#     plot_corr(data.frame(a = c("a", "b", "c"), b = c("d", "e", "f"))),
#     "Need at least two numeric columns to calculate correlation"
#   )
# })
#
# test_that("Correlation method must be appropriate", {
#   expect_error(
#     plot_corr(input_df, method = "kendll"),
#     "Correlation method not acceptable"
#   )
# })
#
# test_that("Colour palette not diverging", {
#   expect_warning(
#     plot_corr(input_df, colour_palette = "Blues"),
#     "Recommended ggplot continuous diverging colour palette"
#   )
# })

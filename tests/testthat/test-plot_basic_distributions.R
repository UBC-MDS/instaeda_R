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

test_that("Invalid function parameter tests", {
  expect_error(plot_basic_distributions(NULL),"Data provided is not a data frame")
  expect_error(plot_basic_distributions(input_df,include="not a valid input"),"include parameter invalid. Please choose NULL, 'number' or 'string'")
  expect_warning(plot_basic_distributions(input_df,ggtheme="IncorrectColor"),"Only the following ggthemes are supported: theme_gdocs, theme_excel,
            theme_economist, theme_wsj, theme_solarized")

})

test_that("Select specific columns from data frame", {
  expect_equal(length(plot_basic_distributions(input_df,cols=c('sex','year'))), 2)
})



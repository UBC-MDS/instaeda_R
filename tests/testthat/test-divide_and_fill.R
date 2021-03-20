library(testthat)
library(instaeda)
library(palmerpenguins)

input_df <- palmerpenguins::penguins

test_that("test input as a dataframe", {
  expect_error(divide_and_fill(c("a", "b")), "Dataframe must be a dataframe.")
})

test_that("test input as a dataframe", {
  expect_error(divide_and_fill(1), "Dataframe must be a dataframe.")
})

test_that("test verbose", {
  expect_error(divide_and_fill(input_df, verbose="a"), "Verbose must be an integer.")
})

test_that("test cols", {
  expect_error(divide_and_fill(input_df, cols=5), "The input cols must be of type character belong to the column
            names for input dataframe!")
})

test_that("test cols", {
  expect_error(divide_and_fill(input_df, cols=list()), "Need at least one numeric columns to fill.")
})

test_that("test cols", {
  expect_error(divide_and_fill(input_df, cols=NA), "The input cols must be of type character belong to the column
            names for input dataframe!")
})

test_that("test cols", {
  expect_error(divide_and_fill(input_df, cols=c("species")), "All items in list cols must be numeric.")
})

test_that("test strategy", {
  expect_error(divide_and_fill(input_df, strategy = 'madeup'), "Can only use these strategies: mean, median, majority, or random")
})

test_that("test random", {
  expect_error(divide_and_fill(input_df, random = 'madeup'), "Random must be logical.")
})


test_that("test return object dataframe", {
  filled_df <- divide_and_fill(input_df)
  expect_true(is.data.frame(filled_df))
})

test_that("test return object dataframe", {
  filled_df <- divide_and_fill(input_df, verbose = 1L)
  expect_true(is.data.frame(filled_df))
})

test_that("test return object dataframe", {
  filled_df <- divide_and_fill(input_df, cols = c("bill_length_mm", "body_mass_g"))
  expect_true(is.data.frame(filled_df))
})

test_that("test return object dataframe", {
  filled_df <- divide_and_fill(input_df, cols = "bill_length_mm")
  expect_true(is.data.frame(filled_df))
})

test_that("test return object dataframe", {
  filled_df <- divide_and_fill(input_df, cols = "bill_length_mm", strategy = "median", verbose = 0L)
  expect_true(is.data.frame(filled_df))
})

test_that("test return object dataframe", {
  filled_df <- divide_and_fill(input_df, cols = "bill_length_mm", verbose = 0L)
  expect_true(is.data.frame(filled_df))
})

test_that("test return object dataframe", {
  filled_df <- divide_and_fill(input_df, verbose = 1L, random = TRUE)
  expect_true(is.data.frame(filled_df))
})

test_that("test return object dataframe", {
  filled_df <- divide_and_fill(input_df, verbose = 1L, strategy = 'random')
  expect_true(is.data.frame(filled_df))
})

test_that("test return object dataframe", {
  filled_df <- divide_and_fill(input_df, verbose = 1L, strategy = 'median')
  expect_true(is.data.frame(filled_df))
})
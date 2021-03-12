library(testthat)
library(instaeda)
library(palmerpenguins)

input_df <- palmerpenguins::penguins

test_that("test input as a dataframe", {
  expect_error(plot_intro(c("a", "b")), "Input provided is not a dataframe")
})

test_that("test input as a dataframe", {
  expect_error(plot_intro(1), "Input provided is not a dataframe")
})

test_that("test plotting title as a string", {
  expect_error(plot_intro(input_df, title=1), "Plotting title provided is not a string")
})

test_that("get all missing columns", {
  test_df <- data.table(
    "a" = rnorm(10L),
    "b" = rep(NA_character_, 10L),
    "c" = rep(NA_integer_, 10L),
  )
  if (!is.data.table(test_df))
    test_df <- data.table(test_df)

  expect_equal(
    vapply(test_df, function(x)
      sum(is.na(x)) == length(x), TRUE),
    c("a" = FALSE, "b" = TRUE, "c" = TRUE))
})

test_that("test intro output", {
  df <- data.frame(
    "A" = letters,
    "B" = rep(NA_character_, 26L),
    "C" = rnorm(26L),
    "D" = rep(NA_integer_, 26L),
    "E" = c(NA, letters[1L:25L]),
    "F" = c(rpois(25L, 1L), NA)
  )

  ## Get intro data
  is_data_table <- is.data.table(df)
  data_class <- class(df)
  if (!is.data.table(df))
    data <- data.table(df)

  getMissingInx <- function(df) {
    if (!is.data.table(df))
      df <- data.table(df)
    vapply(df, function(x)
      sum(is.na(x)) == length(x), TRUE)
  }

  ## Find index for each type
  all_missing_index <- which(getMissingInx(data))
  numeric_index <-
    setdiff(which(vapply(data, is.numeric, TRUE)), all_missing_index)

  ## Count number of numeric and all-missing columns
  n_all_missing <- length(all_missing_index)
  n_numeric <- length(numeric_index)

  ## Create object for numeric columns
  numeric_obj <- data[, numeric_index, with = FALSE]
  setnames(numeric_obj, make.names(names(numeric_obj)))
  if (!is_data_table)
    class(numeric_obj) <- data_class

  ## Split data by types
  split_data <- list(
    "numeric" = numeric_obj,
    "num_numeric" = n_numeric,
    "num_all_missing" = n_all_missing
  )

  output <- data.table(
    "rows" = nrow(data),
    "columns" = ncol(data),
    "numeric_columns" = split_data[["num_numeric"]],
    "all_missing_columns" = split_data[["num_all_missing"]],
    "total_missing_values" = sum(is.na(data)),
    "complete_rows" = sum(complete.cases(data)),
    "total_observations" = nrow(data) * ncol(data),
    "memory_usage" = as.numeric(object.size(data))
  )

  expect_equal(output[["rows"]], 26L)
  expect_equal(output[["columns"]], 6L)
  expect_equal(output[["numeric_columns"]], 2L)
  expect_equal(output[["all_missing_columns"]], 2L)
  expect_equal(output[["total_missing_values"]], 54L)
  expect_equal(output[["total_observations"]], 156L)
  expect_is(output[["memory_usage"]], "numeric")
})

test_that("test return object", {
  plot_obj <- plot_intro(input_df)
  expect_true(is.ggplot(plot_obj))
})


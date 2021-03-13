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

test_that("test intro output", {
  ## Check missing data
  all_missing_index <- which(vapply(input_df, function(x)
    sum(is.na(x)) == length(x), TRUE))

  ## Find index for numeric columns
  numeric_index <-
    setdiff(which(vapply(input_df, is.numeric, TRUE)), all_missing_index)

  ## Count number of numeric and all-missing columns
  num_missing <- length(all_missing_index)
  num_numeric <- length(numeric_index)

  ## Create object for numeric columns
  numeric_obj <- input_df[, numeric_index, with = FALSE]
  setnames(numeric_obj, make.names(names(numeric_obj)))

  ## Split data by types
  col_list <- list(
    "numeric" = numeric_obj,
    "num_numeric" = num_numeric,
    "num_missing" = num_missing
  )

  data_summary <- data.table(
    "rows" = nrow(input_df),
    "columns" = ncol(input_df),
    "numeric_col" = col_list[["num_numeric"]],
    "missing_col" = col_list[["num_missing"]],
    "total_num_of_missing_values" = sum(is.na(input_df)),
    "complete_rows" = sum(complete.cases(input_df)),
    "total_counts" = nrow(input_df) * ncol(input_df),
    "memory" = as.numeric(object.size(input_df))
  )

  expect_equal(data_summary[["rows"]], 344L)
  expect_equal(data_summary[["columns"]], 8L)
  expect_equal(data_summary[["numeric_col"]], 5L)
  expect_equal(data_summary[["missing_col"]], 0L)
  expect_equal(data_summary[["total_num_of_missing_values"]], 19L)
  expect_equal(data_summary[["total_counts"]], 2752)
  expect_is(data_summary[["memory"]], "numeric")
})

test_that("test return object", {
  plot_obj <- plot_intro(input_df)
  expect_true(is.ggplot(plot_obj))
})


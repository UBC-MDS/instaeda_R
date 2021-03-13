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
  ## Check the number of missing data
  num_of_missing_data <- sum(is.na(input_df))

  ## Find numeric columns
  numeric_cols <- unlist(lapply(input_df, is.numeric))

  ## Count number of numeric columns
  num_numeric <- ncol(input_df[, numeric_cols])

  ## Split data by types
  col_list <- list("num_of_numeric" = num_numeric,
                   "num_of_missing" = num_of_missing_data)

  data_summary <- data.frame(
    "rows" = nrow(input_df),
    "columns" = ncol(input_df),
    "numeric_col" = col_list[["num_of_numeric"]],
    "factor_col" = ncol(input_df) - num_numeric,
    "missing_values" = col_list[["num_of_missing"]],
    "complete_rows" = sum(complete.cases(input_df)),
    "total_counts" = nrow(input_df) * ncol(input_df)
  )

  plot_data <- data.frame(
    "type" = c("column", "column", "observation", "rows"),
    "var_name" = c(
      "Numeric Columns",
      "Factor Columns",
      "Missing Values",
      "Complete Rows"
    ),
    "count" = c(data_summary[["numeric_col"]],
                data_summary[["factor_col"]],
                data_summary[["missing_values"]],
                data_summary[["complete_rows"]])

  )

  expect_equal(data_summary[["rows"]], 344L)
  expect_equal(data_summary[["columns"]], 8L)
  expect_equal(data_summary[["numeric_col"]], 5L)
  expect_equal(data_summary[["factor_col"]], 3L)
  expect_equal(data_summary[["missing_values"]], 19L)
  expect_equal(data_summary[["total_counts"]], 2752)
  expect_equal(plot_data[1, 3], 5)
  expect_equal(plot_data[2, 3], 3)
  expect_equal(plot_data[3, 3], 19)
  expect_equal(plot_data[4, 3], 333)

})

test_that("test return object", {
  plot_obj <- plot_intro(input_df)
  expect_true(is.ggplot(plot_obj))
})


input_df <- palmerpenguins::penguins

test_that("get all missing columns", {
  dt <- data.table(
    "a" = seq.int(10L),
    "b" = rep(NA_character_, 10L),
    "c" = rep(NA_integer_, 10L),
    "d" = rnorm(10L)
  )
  if (!is.data.table(dt))
    dt <- data.table(dt)

  expect_equal(
    vapply(dt, function(x)
      sum(is.na(x)) == length(x), TRUE),
    c("a" = FALSE, "b" = TRUE, "c" = TRUE, "d" = FALSE))
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

  getAllMissing <- function(dt) {
    if (!is.data.table(dt))
      dt <- data.table(dt)
    vapply(dt, function(x)
      sum(is.na(x)) == length(x), TRUE)
  }

  ## Find indicies for each feature type
  all_missing_ind <- which(getAllMissing(data))
  numeric_ind <-
    setdiff(which(vapply(data, is.numeric, TRUE)), all_missing_ind)

  ## Count number of discrete, continuous and all-missing features
  n_all_missing <- length(all_missing_ind)
  n_numeric <- length(numeric_ind)

  ## Create object for numeric features
  numeric <- data[, numeric_ind, with = FALSE]
  setnames(numeric, make.names(names(numeric)))

  ## Set data class back to original
  if (!is_data_table)
    class(numeric) <- data_class

  split_data <- list(
    "numeric" = numeric,
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

test_that("test input as a dataframe", {
  expect_error(plot_intro(c("a", "b")), "Input provided is not a dataframe")
})

test_that("test input as a dataframe", {
  expect_error(plot_intro(1), "Input provided is not a dataframe")
})

test_that("test plotting title as a string", {
  expect_error(plot_intro(input_df, title=1), "Plotting title provided is not a string")
})

test_that("test return object", {
  plot_obj <- plot_intro(input_df)
  expect_true(is.ggplot(plot_obj))
})


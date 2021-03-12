
#' Plot summary metrics for input data.
#'
#' @import ggplot2
#' @import data.table
#' @importFrom stats reorder
#' @importFrom scales comma percent
#' @importFrom stats complete.cases
#' @importFrom utils object.size
#' @import dplyr
#' @import stringr

#'
#' @param data input data
#' @param title plot title
#' @param theme_config a list of configurations to manually change colors
#' @return return the ggplot object
#' @keywords plot_intro
#' @export
#' @examples
#' \dontrun{
#' plot_intro(example_dataframe)
#' }
plot_intro <-
  function(data,
           title = "",
           theme_config = list()) {

    # Check input as a dataframe
    if (!is.data.frame(data)) {
      stop("Input provided is not a dataframe")
    }

    # Check title arg as a string
    if (!is.character(title)) {
      stop("Plotting title provided is not a string")
    }

    ## Get input data summary
    is_data_table <- is.data.table(data)
    data_class <- class(data)
    if (!is.data.table(data))
      data <- data.table(data)

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

    data_summary <- data.table(
      "rows" = nrow(data),
      "columns" = ncol(data),
      "numeric_columns" = split_data[["num_numeric"]],
      "all_missing_columns" = split_data[["num_all_missing"]],
      "total_missing_values" = sum(is.na(data)),
      "complete_rows" = sum(complete.cases(data)),
      "total_observations" = nrow(data) * ncol(data),
      "memory_usage" = as.numeric(object.size(data))
    )
    if (!is_data_table)
      class(data_summary) <- data_class

    id <- dimension <- variable <- value <- NULL

    ## Get plotting data
    memory_usage <- data_summary[["memory_usage"]]
    class(memory_usage) <- "object_size"
    memory_usage_string <- format(memory_usage, unit = "auto")
    plot_data <- data.table(
      "id" = seq.int(4L),
      "dimension" = c(rep("column", 2L), "row", "observation"),
      "variable" = c(
        "Numeric Columns",
        "All Missing Columns",
        "Complete Rows",
        "Missing Observations"
      ),
      "value" = c(
        data_summary[["numeric_columns"]] / data_summary[["columns"]],
        data_summary[["all_missing_columns"]] / data_summary[["columns"]],
        data_summary[["complete_rows"]] / data_summary[["rows"]],
        data_summary[["total_missing_values"]] / data_summary[["total_observations"]]
      )
    )

    ## Plot the intro data info
    output <-
      ggplot(plot_data, aes(
        x = reorder(variable, -id),
        y = value,
        fill = dimension
      )) +
      geom_bar(stat = "identity") +
      scale_y_continuous(labels = percent) +
      scale_fill_discrete("Dimension") +
      coord_flip() +
      labs(x = "Metrics", y = "Value") +
      guides(fill = guide_legend(override.aes = aes(label = ""))) +
      ggtitle(ifelse(
        str_length(title) == 0,
        paste("Memory Usage:", memory_usage_string),
        title
      )) +
      theme_gray() + scale_color_manual(values = theme_config)

    ## Plot object
    class(output) <- c("single", class(output))

    output
  }


#' Plot correlation between numeric features.
#' Takes a dataframe, subsets numeric columns and returns a correlation plot object.
#'
#' @param df Dataframe from which to take columns and calculate, plot correlation between columns.
#' @param cols List of columns to perform correlation on. By default, null (perform on all numeric).
#' @param method correlation calculation method, one of: {'pearson', 'kendall', 'spearman'}. By default 'pearson'
#' @param colour_palette one of ggplot accepted colour schemes
#'
#' @return ggplot plot object
#'
#' @examples
#' \dontrun{
#' plot_corr(example_dataframe)
#' }
plot_corr <- function(df,
                      cols = NULL,
                      method = "pearson",
                      colour_palette = "PuOr") {

  # subset dataframe for numeric values only
  if (!is.data.frame(df)) {
    stop("Data provided is not a data frame")
  }
  if (dplyr::select_if(df, is.numeric) %>% ncol() < 2) {
    stop("Need at least two numeric columns to calculate correlation")
  }
  num_df <- dplyr::select_if(df, is.numeric)
  if (length(cols) > 0) {
    num_df <- num_df %>% select(all_of(cols))
  }

  # calculate correlation
  correlation_methods <- c("pearson", "kendall", "spearman")
  if (!(method %in% correlation_methods)) {
    stop("Correlation method not acceptable")
  }
  corr_df <- as.data.frame(cor(num_df, use = "complete.obs", method = method))

  corr_df <- corr_df %>%
    dplyr::mutate("variable_1" = (corr_df %>% rownames())) %>%
    tidyr::pivot_longer(names_to = "variable_2", values_to = "corr", cols = where(is.numeric))

  # plot
  colour_palette_list <- c(
    "BrBG", "PiYG", "PRGn", "PuOr", "RdBu",
    "RdGy", "RdYlBu", "RdYlGn", "Spectral"
  )
  if (!(colour_palette %in% colour_palette_list)) {
    warning("Recommended ggplot continuous diverging colour palette")
  }
  ggplot2::ggplot(corr_df, ggplot2::aes(
    x = variable_1,
    y = variable_2,
    fill = corr
  )) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_distiller(palette = colour_palette, limits = c(-1, 1)) +
    ggplot2::labs(
      title = "Correlations between variables",
      x = "Variable 1",
      y = "Variable 2",
      fill = "Correlation"
    ) +
    ggplot2::geom_text(ggplot2::aes(label = round(corr, 4)), size = 3) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 50, vjust = 0.55))
}


#' Takes a dataframe, subsets selected columns and divides into parts for imputation of missing values and returns a data frame.
#'
#' @param dataframe Dataframe from which to take columns and check for missing values.
#' @param cols List of columns to perform imputation on.
#' By default, None (perform on all numeric columns).
#' @param missing_values int, float, str, np.nan or None.
#' The placeholder for the missing values. All occurences of missing values will be imputed.
#' @param strategy string. imputation strategy, one of: {'mean', 'median', 'constant', 'most_frequent'}.
#' By default, 'mean'.
#' @param fill_value string or numerical value, optional.
#' When strategy == 'constant', fill_value is used to replace all occurences of missing_values.
#' If left to default, fill_value will be 0 when filling numerical data and 'missing' for strings or object data types.
#' @param random boolean, optional
#' When random == True, shuffles data frame before filling. By default, False.
#' @param parts integer, optional
#' The number of parts to divide rows of data frame into. By default, 1.
#' @param verbose integer, optional
#' Controls the verbosity of the divide and fill. By default, 0.
#'
#' @return data.frame Data frame obtained after divide and fill on the corresponding columns.
#' @export
#'
#' @examples
#' \dontrun{
#' divide_and_fill(example_dataframe)
#' }
divide_and_fill <- function(dataframe,
                            cols = None,
                            missing_values = np.nan,
                            strategy = "mean",
                            fill_value = None,
                            random = False,
                            parts = 1,
                            verbose = 0) {
  NULL
}


#' Plot basic distributions for numeric and string features from a given dataframe
#'
#' @param df Dataframe from which to generate plots for each column from
#' @param cols List of columns to generate plots for. By default, NULL (perform on all numeric).
#' @param include select the data type to include. Supported values include NULL, "string" and "number". When selecting NULL, both string and number datatypes will be returned. By default, NULL.
#' @param colour_palette one of ggplot accepted colour schemes for the plots
#'
#' @return named list of ggplot objects with each name referencing a column name
#' @export
#'
#' @examples
#'  \dontrun{
#'  plot_basic_distributions(example_dataframe)
#'  }
plot_basic_distributions <- function(df,
                                     cols = NULL,
                                     include = NULL,
                                     colour_palette = "PuOr") {

  named_ls_plots <- list()

  # subset dataframe for numeric values only
  if (!is.data.frame(df)) {
    stop("Data provided is not a data frame")
  }

  # include string, number or all
  include_options <- c(NULL, "number", "string")
  if (length(include) > 0  && !(include %in% include_options)) {
    stop("include parameter invalid. Please choose NULL, 'number' or 'string'")
  }

  # plot
  colour_palette_list <- c("BrBG",
                           "PiYG",
                           "PRGn",
                           "PuOr",
                           "RdBu",
                           "RdGy",
                           "RdYlBu",
                           "RdYlGn",
                           "Spectral")
  if (length(colour_palette) > 0  &&
      !(colour_palette %in% colour_palette_list)) {
    warning("Recommended ggplot continuous diverging colour palette")
  }

  # First filter by only the columns filtered upoo
  # If NULL, no column filters
  if (length(cols) > 0) {
    df1 <- df %>% select(all_of(cols))
  } else {
    df1 <- df
  }

  if (is.null(include) |
      (length(include) > 0 && include == 'number')) {
    #Print only character
    df2 <- select_if(df1, is.numeric)

    for (i in colnames(df2)) {
      # Basic histogram
      named_ls_plots[[i]] <-
        ggplot2::ggplot(df2, ggplot2::aes_string(x = i)) + ggplot2::geom_histogram() +
        ggplot2::scale_fill_brewer(palette = colour_palette)

    }
  }

  if (is.null(include) |
      (length(include) > 0 && include == 'string')) {
    df3 <-
      df1[, sapply(df1, class) == 'character' |
            sapply(df1, class) == 'factor']

    for (i in colnames(df3)) {
      # Bar Chart
      named_ls_plots[[i]] <-
        df3 %>% add_count("{i}") %>% ggplot2::ggplot(ggplot2::aes_string(x = i)) +
        ggplot2::geom_bar() + ggplot2::scale_fill_brewer(palette = colour_palette)

    }
  }
  named_ls_plots
}

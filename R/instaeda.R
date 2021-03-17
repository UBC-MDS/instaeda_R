


#' Plot summary introduction for input data.
#'
#' @param data input data
#' @param title plot title
#' @param color_config configurations to manually change colors
#'
#' @return the ggplot object
#'
#' @import ggplot2
#' @import utils
#' @import dplyr
#' @import stringr
#' @importFrom stats, complete.cases
#'
#' @examples
#' \dontrun{
#' plot_info(example_dataframe)
#' }
plot_info <-
  function(data,
           title = "",
           color_config = list()) {
    # Check input as a dataframe
    if (!is.data.frame(data)) {
      stop("Input provided is not a dataframe")
    }

    # Check title arg as a string
    if (!is.character(title)) {
      stop("Plotting title provided is not a string")
    }

    ## Check the number of missing data
    num_of_missing_data <- sum(is.na(data))

    ## Find numeric columns
    numeric_cols <- unlist(lapply(data, is.numeric))

    ## Count number of numeric columns
    num_numeric <- ncol(data[, numeric_cols])

    ## Split data by types
    col_list <- list("num_of_numeric" = num_numeric,
                     "num_of_missing" = num_of_missing_data)

    data_summary <- data.frame(
      "rows" = nrow(data),
      "columns" = ncol(data),
      "numeric_col" = col_list[["num_of_numeric"]],
      "factor_col" = ncol(data) - num_numeric,
      "missing_values" = col_list[["num_of_missing"]],
      "complete_rows" = sum(complete.cases(data)),
      "total_counts" = nrow(data) * ncol(data)
    )

    ## Calculate the components for plotting
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

    ## Plot the intro data info
    var_name <- type <- NULL # initialize local variables
    output <- ggplot(plot_data, aes(x = var_name,
                                    y = count,
                                    fill = type)) +
      geom_bar(stat = "identity") +
      scale_fill_discrete("Measurements") +
      labs(x = "Category", y = "Count") +
      ggtitle(ifelse(str_length(title) == 0,
                     paste("Summary for input data"),
                     title)) +
      theme_gray() +
      scale_color_manual(values = color_config) +
      facet_wrap( ~ type) +
      theme(axis.text.x = element_text(angle = 90))

    ## Plot object
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
#' @import ggplot2
#' @importFrom stats cor
#' @import dplyr
#' @import tidyr
#' @importFrom tidyselect vars_select_helpers
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
  corr_df <- as.data.frame(stats::cor(num_df, use = "complete.obs", method = method))

  corr_df <- corr_df %>%
    dplyr::mutate("variable_1" = (corr_df %>% rownames())) %>%
    tidyr::pivot_longer(names_to = "variable_2", values_to = "corr", cols = tidyselect::vars_select_helpers$where(is.numeric))

  # plot
  variable_1 <- variable_2 <- corr <- NULL #initialize local variables
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


#' Takes a dataframe, subsets selected columns and imputes missing values and returns a data frame.
#' @import tidyr
#' @import dplyr
#' @import imputeR
#' @importFrom scales comma percent
#'
#' @param dataframe Dataframe from which to take columns and
#' check for missing values.
#' @param cols List of columns to perform imputation on.
#' By default, NULL(perform on all numeric columns).
#' @param strategy string. imputation strategy, one of:
#' {'mean', 'median', 'random'}. By default, 'mean'.
#' @param random boolean, optional
#' When random == True, shuffles data frame before filling. By default, False.
#' @param verbose integer, optional
#' Controls the verbosity of the divide and fill. By default, 0.
#'
#' @return data.frame Data frame obtained after divide and fill on the
#' corresponding columns.
#' @export
#'
#' @examples
#' \dontrun{
#' divide_and_fill(example_dataframe)
#' }
divide_and_fill <- function(dataframe,
                            cols = NULL,
                            strategy = "mean",
                            random = FALSE,
                            verbose = 0L) {
  allowed_strategies <- c("mean", "median", "random")

  if (verbose != 0){
    print("Checking inputs.")
  }
  #Check inputs
  if (!is.integer(verbose)){
    stop("Verbose must be an integer.")
  }
  if (!is.data.frame(dataframe)) {
    stop("Dataframe must be a dataframe.")
  }
  if (is.null(cols)){
    cols <-  names(dplyr::select_if(dataframe, is.numeric))
  }
  if (length(cols) < 1){
    stop("Need at least one numeric columns to fill.")
  }
  if (!is.character(cols) | (!all(cols %in% names(dataframe)))){
    stop("The input cols must be of type character belong to the column
            names for input dataframe!")
  }

  if (!strategy %in% allowed_strategies){
    stop("Can only use these strategies: mean, median, majority, or random")
  }
  if (!is.logical(random)){
    stop("Random must be logical.")
  }

  # Constructing filled dataframe skeleton.
  if (verbose != 0){
    print("Constructing filled dataframe skeleton.")
  }
  if (random == TRUE){
    print("Try random shuffle!")
    rows <- sample(nrow(dataframe))
    filled_df <- dataframe[rows, ]
  }else{
    filled_df <- dataframe
  }
  numeric_col_names <- names(dplyr::select_if(dataframe, is.numeric))
  non_numeric_col_names <- names(dataframe)[!names(dataframe) %in% numeric_col_names]
  if (all(cols %in% numeric_col_names)){
    numeric = TRUE
  }else {
    stop("All items in list cols must be numeric.")
  }

  # Filling dataframe
  for (j in 1:length(cols)){
    if(strategy == 'mean'){
      print(numeric)
      if (numeric){
        filled_df[, cols[j]] <- imputeR::guess(filled_df[, cols[j]], type = 'mean')
      }else{
        stop('Strategy mean can be used with numeric columns.')
      }
    }else if(strategy == 'random'){
      print(numeric)
      if (numeric){
        filled_df[, cols[j]] <- imputeR::guess(filled_df[, cols[j]], type = 'random')
      }else{
        stop('Strategy random can be used with numeric columns.')
      }
    }else{
      if (numeric){
        filled_df[, cols[j]] <- imputeR::guess(filled_df[, cols[j]], type = 'median')
      }else{
        stop('Strategy median can be used with numeric columns.')
      }
    }
  }
  return (filled_df)
}


#' Plot basic distributions for numeric and string features from a given dataframe
#'
#' @param df dataframe from which to generate plots for each column from
#' @param cols List of columns to generate plots for. By default, NULL (perform on all numeric).
#' @param include select the data type to include. Supported values include NULL, "string" and "number". When selecting NULL, both string and number datatypes will be returned. By default, NULL.
#' @param ggtheme customize the ggtheme used in plots. Only the following ggthemes are supported: theme_gdocs, theme_excel, theme_economist, theme_wsj, theme_solarized
#'
#' @return named list of ggplot objects with each name referencing a column name
#'
#' @import ggthemes
#' @import tidyverse
#' @import dplyr
#' @import stringr
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'  plot_basic_distributions(example_dataframe)
#'  }
plot_basic_distributions <- function(df,
                                     cols = NULL,
                                     include = NULL,
                                     ggtheme = "theme_economist") {

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
  ggtheme_list <- c("theme_gdocs",
                           "theme_excel",
                           "theme_economist",
                           "theme_wsj",
                           "theme_solarized"
                           )
  if (length(ggtheme) > 0  &&
      !(ggtheme %in% ggtheme_list)) {
    warning("Only the following ggthemes are supported: theme_gdocs, theme_excel,
            theme_economist, theme_wsj, theme_solarized")
  }


  ggtheme_chosen <- NULL
  if (is.null(ggtheme)){
    ggtheme_chosen <- NULL
  } else if (ggtheme == 'theme_gdocs'){
    ggtheme_chosen <- theme_gdocs()
  } else if (ggtheme == 'theme_excel'){
    ggtheme_chosen <- theme_excel()
  } else if (ggtheme == 'theme_economist'){
    ggtheme_chosen <- theme_economist()
  } else if (ggtheme == 'theme_wsj'){
    ggtheme_chosen <- theme_wsj()
  } else if (ggtheme == 'theme_solarized'){
    ggtheme_chosen <- theme_solarized()
  } else {
    ggtheme_chosen <- NULL
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
        ggplot(df2, aes_string(x = i)) + geom_histogram() + ggtheme_chosen

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
        df3 %>% add_count("{i}") %>% ggplot(aes_string(x = i)) +
            geom_bar()  + ggtheme_chosen

    }
  }
  named_ls_plots
}

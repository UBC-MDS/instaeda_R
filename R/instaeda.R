#' @import ggplot2
#' @import data.table
#' @importFrom stats reorder
#' @importFrom scales comma percent
#' @importFrom stats complete.cases
#' @importFrom utils object.size
#' @import dplyr

#' Plot summary metrics for input data.
#'
#' @param data input data
#' @param geom_labels a list of arguments to \link{geom_label}
#' @param title plot title
#' @param theme_config a list of configurations to be passed to \link{theme}.
#' @return invisibly return the ggplot object
#' @keywords plot_intro
#' @export
#' @examples
#' plot_intro(example_dataframe)
plot_intro <-
  function(data,
           geom_labels = list(),
           title = NULL,
           theme_config = list()) {
    NULL
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
#' plot_corr(example_dataframe)
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
  ggplot(corr_df, aes(
    x = variable_1,
    y = variable_2,
    fill = corr
  )) +
    geom_tile() +
    scale_fill_distiller(palette = colour_palette, limits = c(-1, 1)) +
    labs(
      title = "Correlations between variables",
      x = "Variable 1",
      y = "Variable 2",
      fill = "Correlation"
    ) +
    geom_text(aes(label = round(corr, 4)), size = 3) +
    theme(axis.text.x = element_text(angle = 50, vjust = 0.55))
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
#' divide_and_fill(example_dataframe)
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
#' plot_basic_distributions(example_dataframe)
plot_basic_distributions <- function(df,
                                     cols = NULL,
                                     include = NULL,
                                     colour_palette = "purpleorange") {
  NULL
}

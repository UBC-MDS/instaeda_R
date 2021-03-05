#' Plot summary metrics for input data.
#'
#' @param data input data
#' @param geom_labels a list of arguments to \link{geom_label}
#' @param title plot title
#' @param theme_config a list of configurations to be passed to \link{theme}.
#' @return invisibly return the ggplot object
#' @keywords plot_intro
#' @import ggplot2
#' @import data.table
#' @importFrom stats reorder
#' @importFrom scales comma percent
#' @importFrom stats complete.cases
#' @importFrom utils object.size
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
plot_corr <- function(df, cols=NULL, method="pearson", colour_palette="purpleorange"){
  NULL
}



#' Title
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
                            cols=None,
                            missing_values=np.nan,
                            strategy="mean",
                            fill_value=None,
                            random=False,
                            parts=1,
                            verbose=0){
  NULL
}


#' Title
#'
#' @return
#' @export
#'
#' @examples
plot_basic_distributions <- function(){
  NULL
}

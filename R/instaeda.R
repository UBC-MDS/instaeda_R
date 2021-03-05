#' Title
#'
#' @return
#' @export
#'
#' @examples
plot_intro <- function(){
  NULL
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
plot_corr <- function(){
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

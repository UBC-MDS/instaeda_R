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
    ## Declare variable 
    id <- dimension <- variable <- value <- NULL
    ## Get intro data
    get_intro <- function(data) {
      ## Check and set to data.table
      is_data_table <- is.data.table(data)
      data_class <- class(data)
      if (!is.data.table(data))
        data <- data.table(data)
      
      split_data <- split_columns(data)
      
      output <- data.table(
        "rows" = nrow(data),
        "columns" = ncol(data),
        "discrete_columns" = split_data[["num_discrete"]],
        "continuous_columns" = split_data[["num_continuous"]],
        "all_missing_columns" = split_data[["num_all_missing"]],
        "total_missing_values" = sum(is.na(data)),
        "complete_rows" = sum(complete.cases(data)),
        "total_observations" = nrow(data) * ncol(data),
        "memory_usage" = as.numeric(object.size(data))
      )
      
      ## Set data class back to original
      if (!is_data_table)
        class(output) <- data_class
      output
    }
    
    intro <- get_intro(data)
    memory_usage <- intro[["memory_usage"]]
    class(memory_usage) <- "object_size"
    memory_usage_string <- format(memory_usage, unit = "auto")
    intro2 <- data.table(
      "id" = seq.int(5L),
      "dimension" = c(rep("column", 3L), "row", "observation"),
      "variable" = c(
        "Discrete Columns",
        "Continuous Columns",
        "All Missing Columns",
        "Complete Rows",
        "Missing Observations"
      ),
      "value" = c(
        intro[["discrete_columns"]] / intro[["columns"]],
        intro[["continuous_columns"]] / intro[["columns"]],
        intro[["all_missing_columns"]] / intro[["columns"]],
        intro[["complete_rows"]] / intro[["rows"]],
        intro[["total_missing_values"]] / intro[["total_observations"]]
      )
    )
    ## Create ggplot object
    output <-
      ggplot(intro2, aes(
        x = reorder(variable,-id),
        y = value,
        fill = dimension
      )) +
      geom_bar(stat = "identity") +
      scale_y_continuous(labels = percent) +
      scale_fill_discrete("Dimension") +
      coord_flip() +
      labs(x = "Metrics", y = "Value") +
      guides(fill = guide_legend(override.aes = aes(label = "")))
    geom_label_args_list <-
      list("mapping" = aes(label = percent(value)))
    output <- output +
      do.call("geom_label", c(geom_label_args_list, geom_labels))
    ## Plot object
    class(output) <- c("single", class(output))
    plotDataExplorer(
      plot_obj = output,
      title = ifelse(
        is.null(title),
        paste("Memory Usage:", memory_usage_string),
        title
      ),
      theme_config = theme_config
    )
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

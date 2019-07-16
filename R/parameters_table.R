#' Parameters Table Formatting
#'
#' @param x A dataframe of model's parameters.
#' @param clean_names Clean parameters' names if possible.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' library(parameters)
#'
#' x <- model_parameters(lm(Sepal.Length ~ Species * Sepal.Width, data = iris))
#' parameters_table(x)
#'
#' @export
parameters_table <- function(x, clean_names = TRUE, ...){
  UseMethod("parameters_table")
}


#' @export
parameters_table.parameters_table <- function(x, clean_names = TRUE, ...){
  format_table(x)

  # .parameters_types_table(insight::find_parameters(model, flatten = TRUE), insight::get_data(model))
}
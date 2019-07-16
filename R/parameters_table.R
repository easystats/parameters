#' Parameters Table Formatting
#'
#' @param x A dataframe of model's parameters.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' library(parameters)
#'
#' model_parameters(lm(Sepal.Length ~ Species * Sepal.Width, data = iris))
#'
#' @export
parameters_table <- function(x, ...){
  UseMethod("parameters_table")
}


# parameters_table.parameters_table <- function(x, ...){
#
#   .parameters_types_table(insight::find_parameters(model, flatten = TRUE), insight::get_data(model))
# }
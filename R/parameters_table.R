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
#' as.data.frame(parameters_table(x))
#'
#' @export
parameters_table <- function(x, clean_names = TRUE, ...){
  UseMethod("parameters_table")
}


#' @export
parameters_table.parameters_table <- function(x, clean_names = TRUE, ...){

  # Format parameters names
  if(clean_names){
    x$Parameter <- attributes(x)$clean_names
  }

  # Format specific columns
  if("DoF" %in% names(x)) x$DoF <- format_value(x$DoF, protect_integers = TRUE)
  if("DoF_residual" %in% names(x)) x$DoF_residual <- format_value(x$DoF_residual, protect_integers = TRUE)
  if("p" %in% names(x)) x$p <- substring(format_value(x$p, digits = 3), 2)
  if(all(c("CI_low", "CI_high") %in% names(x))) {
    ci_colname <- sprintf("%i%% CI", attributes(x)$ci * 100)
    x[ci_colname] <- format_ci(x$CI_low, x$CI_high, ci = NULL)
    ci_position <- which(names(x) == "CI_low")
    x <- x[c(names(x)[1:(ci_position-1)], ci_colname, names(x)[ci_position:(length(names(x))-1)])]  # Replace at initial position
    x$CI_low <- x$CI_high <- NULL
  }

  # Format remaining columns
  other_cols <- c("Coefficient", "SE", "t", "Std_Coefficient")
  x[other_cols[other_cols %in% names(x)]] <- format_value(x[other_cols[other_cols %in% names(x)]])

  x
}



#' @export
print.parameters_table <- function(x, clean_names = TRUE, ...) {
  formatted_table <- parameters_table(x, clean_names = clean_names, ...)
  cat(format_table(formatted_table))
}
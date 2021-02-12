#' @export
print.compare_parameters <- function(x,
                                     digits = 2,
                                     ci_digits = 2,
                                     p_digits = 3,
                                     ...) {
  # save original input
  orig_x <- x

  # get attributes
  style <- attributes(x)$output_style

  formatted_table <- format(
    x,
    style,
    split_components = TRUE,
    digits = digits,
    ci_digits = ci_digits,
    p_digits = p_digits,
    ci_width = "auto",
    ci_brackets = TRUE,
    format = "text"
  )

  cat(insight::export_table(formatted_table, format = "text", footer = NULL))

  invisible(orig_x)
}





#' @inheritParams print.parameters_model
#' @export
format.compare_parameters <- function(x, style = NULL, split_components = TRUE, digits = 2, ci_digits = 2, p_digits = 3, ci_width = NULL, ci_brackets = NULL, format = NULL, ...) {
  x$Method <- NULL

  if (!is.null(x$Component) && .n_unique(x$Component) == 1) {
    x$Component <- NULL
  }

  # style / template
  ## TODO select columns

  # check whether to split table by certain factors/columns (like component, response...)
  split_by <- .prepare_splitby_for_print(x)

  # print everything now...
  if (split_components && !is.null(split_by) && length(split_by)) {
    formatted_table <- .print_model_parms_components(x, pretty_names, split_column = split_by, digits = digits, ci_digits = ci_digits, p_digits = p_digits, coef_column = coef_name, format = format, ci_width = ci_width, ci_brackets = ci_brackets, ...)
  } else {
    formatted_table <- insight::format_table(x, pretty_names = pretty_names, digits = digits, ci_width = ci_width, ci_brackets = ci_brackets, ci_digits = ci_digits, p_digits = p_digits, ...)
  }

  # remove unique columns
  if (.n_unique(formatted_table$Component) == 1) formatted_table$Component <- NULL
  if (.n_unique(formatted_table$Effects) == 1) formatted_table$Effects <- NULL

  # no column with CI-level in output
  formatted_table$CI <- NULL

  formatted_table
}

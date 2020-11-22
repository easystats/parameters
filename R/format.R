# usual models ---------------------------------

#' @export
format.parameters_model <- function(x, pretty_names = TRUE, split_components = TRUE, select = NULL, digits = 2, ci_digits = 2, p_digits = 3, ci_width = NULL, ci_brackets = NULL, format = NULL, ...) {
  # save attributes
  res <- attributes(x)$details
  coef_name <- attributes(x)$coefficient_name
  s_value <- attributes(x)$s_value

  # check if user supplied digits attributes
  if (missing(digits)) digits <- .additional_arguments(x, "digits", 2)
  if (missing(ci_digits)) ci_digits <- .additional_arguments(x, "ci_digits", 2)
  if (missing(p_digits)) p_digits <- .additional_arguments(x, "p_digits", 3)

  # prepare output, to have in shape for printing
  x <- .prepare_x_for_print(x, select, coef_name, s_value)

  # check whether to split table by certain factors/columns (like component, response...)
  split_by <- .prepare_splitby_for_print(x)

  # print everything now...
  if (split_components && !is.null(split_by) && length(split_by)) {
    formatted_table <- .print_model_parms_components(x, pretty_names, split_column = split_by, digits = digits, ci_digits = ci_digits, p_digits = p_digits, coef_column = coef_name, format = format, ci_width = ci_width, ci_brackets = ci_brackets, ...)
  } else {
    formatted_table <- insight::parameters_table(x, pretty_names = pretty_names, digits = digits, ci_width = ci_width, ci_brackets = ci_brackets, ci_digits = ci_digits, p_digits = p_digits, ...)
  }

  formatted_table
}




# stan models ----------------------------

#' @importFrom insight print_parameters parameters_table
#' @export
format.parameters_stan <- function(x, split_components = TRUE, select = NULL, ci_width = NULL, ci_brackets = NULL, format = NULL, ...) {
  cp <- attributes(x)$parameter_info
  final_table <- list()

  # round ESS
  if (!is.null(x$ESS)) {
    x$ESS <- round(x$ESS)
  }

  if (!split_components || is.null(cp)) {
    NextMethod()
  } else {
    if (!is.null(select)) {
      if (is.numeric(select)) select <- colnames(x)[select]
      select <- union(select, c("Parameter", "Component", "Effects", "Response", "Subgroup", "Function"))
      to_remove <- setdiff(colnames(x), select)
      x[to_remove] <- NULL
    }

    out <- insight::print_parameters(cp, x, keep_parameter_column = FALSE)
    add_attr <- attributes(out)$additional_attributes

    final_table <- lapply(out, function(i) {
      param_table <- insight::parameters_table(i, ci_width = ci_width, ci_brackets = ci_brackets, preserve_attributes = TRUE)
      param_table$Group <- NULL
      param_table
    })
  }

  .compact_list(final_table)
}




# sem-models ---------------------------------

#' @export
format.parameters_sem <- function(x, digits = 2, ci_digits = 2, p_digits = 3, format = NULL, ci_width = NULL, ci_brackets = TRUE, ...) {
  if (missing(digits)) digits <- .additional_arguments(x, "digits", 2)
  if (missing(ci_digits)) ci_digits <- .additional_arguments(x, "ci_digits", 2)
  if (missing(p_digits)) p_digits <- .additional_arguments(x, "p_digits", 3)

  .print_model_parms_components(x, pretty_names = TRUE, split_column = "Type", digits = digits, ci_digits = ci_digits, p_digits = p_digits, format = format, ci_width = ci_width, ci_brackets = ci_brackets, ...)
}


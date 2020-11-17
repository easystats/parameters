#' Export tables into different output formats
#'
#' Export tables (i.e. data frame) into different output formats
#'
#' @param x An object returned by \code{\link[=model_parameters]{model_parameters()}}.
#' @param format String, indicating the output format. By default, \code{"markdown"} is used.
#' @param split_components Logical, if \code{TRUE} (default), For models with
#'   multiple components (zero-inflation, smooth terms, ...), each component is
#'   printed in a separate table. If \code{FALSE}, model parameters are printed
#'   in a single table and a \code{Component} column is added to the output.
#' @param select Character vector (or numeric index) of column names that should
#'   be printed. If \code{NULL} (default), all columns are printed. The shortcut
#'   \code{select = "minimal"} prints coefficient, confidence intervals and p-values,
#'   while \code{select = "short"} prints coefficient, standard errors and p-values.
#' @param show_sigma Logical, if \code{TRUE}, adds information about the residual
#'   standard deviation.
#' @param show_formula Logical, if \code{TRUE}, adds the model formula to the output.
#' @inheritParams parameters_table
#'
#' @inheritSection format_parameters Interpretation of Interaction Terms
#' @return A character vector
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' mp <- model_parameters(model)
#' to_table(mp)
#' @export
to_table.parameters_model <- function(x, format = "markdown", pretty_names = TRUE, split_components = TRUE, select = NULL, digits = 2, ci_digits = 2, p_digits = 3, show_sigma = FALSE, show_formula = FALSE, ...) {
  # save original input
  orig_x <- x

  # save attributes
  res <- attributes(x)$details
  coef_name <- attributes(x)$coefficient_name
  sigma <- attributes(x)$sigma
  s_value <- attributes(x)$s_value
  p_adjust <- attributes(x)$p_adjust
  model_formula <- attributes(x)$model_formula
  ci_method <- .additional_arguments(x, "bayes_ci_method", NULL)

  # check if user supplied digits attributes
  if (missing(digits)) digits <- .additional_arguments(x, "digits", 2)
  if (missing(ci_digits)) ci_digits <- .additional_arguments(x, "ci_digits", 2)
  if (missing(p_digits)) p_digits <- .additional_arguments(x, "p_digits", 3)

  # table caption
  if (!is.null(attributes(x)$title)) {
    table_caption <- attributes(x)$title
  } else if (!is.null(res)) {
    table_caption <- "Fixed Effects"
  } else {
    table_caption <- NULL
  }

  # prepare output, to have in shape for printing
  x <- .prepare_x_for_print(x, select, coef_name, s_value)

  # check whether to split table by certain factors/columns (like component, response...)
  split_by <- .prepare_splitby_for_print(x)


  # generate final markdown table
  if (split_components && !is.null(split_by) && length(split_by)) {
    formatted_table <- .print_model_parms_components(x, pretty_names, split_column = split_by, digits = digits, ci_digits = ci_digits, p_digits = p_digits, coef_column = coef_name, format = "markdown", ci_width = NULL, ci_brackets = c("(", ")"), ...)
    attr(formatted_table, "format") <- "pipe"
    class(formatted_table) <- c("knitr_kable", "character")
    formatted_table
  } else {
    formatted_table <- parameters_table(x, pretty_names = pretty_names, digits = digits, ci_width = NULL, ci_brackets = c("(", ")"), ci_digits = ci_digits, p_digits = p_digits, ...)
    insight::format_table(formatted_table, format = format, caption = table_caption, align = "firstleft")
  }
}





#' @importFrom insight print_parameters
#' @export
to_table.parameters_stan <- function(x, split_components = TRUE, select = NULL, ...) {
  orig_x <- x
  cp <- attributes(x)$parameter_info

  # round ESS
  if (!is.null(x$ESS)) {
    x$ESS <- round(x$ESS)
  }

  # check if user supplied digits attributes
  ci <- .additional_arguments(x, "ci", .95)
  ci_method <- .additional_arguments(x, "bayes_ci_method", NULL)
  digits <- .additional_arguments(x, "digits", 2)
  ci_digits <- .additional_arguments(x, "ci_digits", 2)
  p_digits <- .additional_arguments(x, "p_digits", 3)

  if (!split_components || is.null(cp)) {
    NextMethod()
  } else {
    if (!is.null(select)) {
      if (is.numeric(select)) select <- colnames(x)[select]
      select <- union(select, c("Parameter", "Component", "Effects", "Response", "Subgroup", "Function"))
      to_remove <- setdiff(colnames(x), select)
      x[to_remove] <- NULL
    }

    final_table <- NULL
    out <- insight::print_parameters(cp, x)

    for (i in out) {
      table_caption <- paste0(attr(i, "main_title"), gsub("  ", " ", attr(i, "sub_title"), fixed = TRUE))
      rem <- which(colnames(i) %in% c("Parameter", "Component", "Effects", "Group", "Response", "Subgroup", "Function"))
      i <- i[, -rem]

      colnames(i)[1] <- "Parameter"
      attr(i, "ci") <- ci
      attr(i, "digits") <- digits
      attr(i, "ci_digits") <- ci_digits
      attr(i, "p_digits") <- p_digits

      formatted_table <- parameters_table(i, pretty_names = FALSE, ci_width = NULL, ci_brackets = c("(", ")"), ...)
      final_table <- c(
        final_table,
        insight::format_table(formatted_table, format = format, caption = table_caption, align = "firstleft")
      )
    }
    attr(final_table, "format") <- "pipe"
    class(final_table) <- c("knitr_kable", "character")
    final_table
  }
}



#' @export
to_table.parameters_simulate <- to_table.parameters_model

#' @export
to_table.parameters_brms_meta <- to_table.parameters_model



#' @importFrom insight to_table
#' @export
insight::to_table

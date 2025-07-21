#' @title Print comparisons of model parameters
#' @name print.compare_parameters
#'
#' @description A `print()`-method for objects from [`compare_parameters()`].
#'
#' @param x An object returned by [`compare_parameters()`].
#' @inheritParams print.parameters_model
#' @inheritSection print.parameters_model Global Options to Customize Messages and Tables when Printing
#'
#' @return Invisibly returns the original input object.
#'
#' @examplesIf require("gt", quietly = TRUE)
#' \donttest{
#' data(iris)
#' lm1 <- lm(Sepal.Length ~ Species, data = iris)
#' lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
#'
#' # custom style
#' result <- compare_parameters(lm1, lm2, select = "{estimate}{stars} ({se})")
#' print(result)
#'
#' # custom style, in HTML
#' result <- compare_parameters(lm1, lm2, select = "{estimate}<br>({se})|{p}")
#' print_html(result)
#' }
#' @export
print.compare_parameters <- function(x,
                                     split_components = TRUE,
                                     caption = NULL,
                                     subtitle = NULL,
                                     footer = NULL,
                                     digits = 2,
                                     ci_digits = digits,
                                     p_digits = 3,
                                     zap_small = FALSE,
                                     groups = NULL,
                                     column_width = NULL,
                                     ci_brackets = c("[", "]"),
                                     select = NULL,
                                     ...) {
  # save original input
  orig_x <- x

  # check if user supplied digits attributes
  if (missing(digits)) {
    digits <- .additional_arguments(x, "digits", digits)
  }
  if (missing(ci_digits)) {
    ci_digits <- .additional_arguments(x, "ci_digits", digits)
  }
  if (missing(p_digits)) {
    p_digits <- .additional_arguments(x, "p_digits", p_digits)
  }

  # get attributes
  if (missing(select)) {
    select <- attributes(x)$output_style
  }
  if (missing(groups)) {
    groups <- attributes(x)$parameter_groups
  }

  formatted_table <- format(
    x,
    select = select,
    split_components = split_components,
    digits = digits,
    ci_digits = ci_digits,
    p_digits = p_digits,
    ci_width = "auto",
    ci_brackets = ci_brackets,
    format = "text",
    groups = groups,
    zap_small = zap_small,
    ...
  )

  # if we have multiple components, we can align colum width across components here
  if (!is.null(column_width) && all(column_width == "fixed") && is.list(formatted_table)) {
    column_width <- .find_min_colwidth(formatted_table)
  }

  cat(insight::export_table(
    formatted_table,
    format = "text",
    caption = caption,
    subtitle = subtitle,
    footer = footer,
    empty_line = "-",
    width = column_width,
    ...
  ))

  invisible(orig_x)
}

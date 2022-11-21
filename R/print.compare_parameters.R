#' @export
print.compare_parameters <- function(x,
                                     split_components = TRUE,
                                     caption = NULL,
                                     subtitle = NULL,
                                     footer = NULL,
                                     digits = 2,
                                     ci_digits = 2,
                                     p_digits = 3,
                                     zap_small = FALSE,
                                     groups = NULL,
                                     column_width = NULL,
                                     ci_brackets = c("(", ")"),
                                     style = NULL,
                                     ...) {
  # save original input
  orig_x <- x

  # check if user supplied digits attributes
  if (missing(digits)) {
    digits <- .additional_arguments(x, "digits", digits)
  }
  if (missing(ci_digits)) {
    ci_digits <- .additional_arguments(x, "ci_digits", ci_digits)
  }
  if (missing(p_digits)) {
    p_digits <- .additional_arguments(x, "p_digits", p_digits)
  }

  # get attributes
  if (missing(style)) {
    style <- attributes(x)$output_style
  }

  formatted_table <- format(
    x,
    style = style,
    split_components = split_components,
    digits = digits,
    ci_digits = ci_digits,
    p_digits = p_digits,
    ci_width = "auto",
    ci_brackets = ci_brackets,
    format = "text",
    groups = groups,
    zap_small = zap_small
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

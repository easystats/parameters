#' @export
print.compare_parameters <- function(x,
                                     digits = 2,
                                     ci_digits = 2,
                                     p_digits = 3,
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
    style,
    split_components = TRUE,
    digits = digits,
    ci_digits = ci_digits,
    p_digits = p_digits,
    ci_width = "auto",
    ci_brackets = c("(", ")"),
    format = "text"
  )

  cat(insight::export_table(formatted_table, format = "text", footer = NULL, empty_line = "-"))

  invisible(orig_x)
}

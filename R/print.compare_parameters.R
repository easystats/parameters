#' @export
print.compare_parameters <- function(x,
                                     digits = 2,
                                     ci_digits = 2,
                                     p_digits = 3,
                                     style = NULL,
                                     ...) {
  # save original input
  orig_x <- x

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

  cat(insight::export_table(formatted_table, format = "text", footer = NULL))

  invisible(orig_x)
}

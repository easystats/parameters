#' Numeric Values Formatting
#'
#' @param x Numeric value.
#' @param digits Number of significant digits.
#' @param ... Arguments passed to or from other methods.
#'
#'
#'
#' @examples
#' format_value(1.20)
#' format_value(1.2)
#' format_value(1.2012313)
#' format_value(c(0.0045, 234, -23))
#' @export
format_value <- function(x, digits = 2, ...) {
  if (is.numeric(x)) {
    # x <- ifelse(is.na(x), NA, trimws(format(round(x, digits), nsmall = digits)))
    x <- ifelse(is.na(x), NA, sprintf(paste0("%.", digits, "f"), x))
  }
  return(x)
}


#' @rdname format_value
#' @importFrom stats na.omit
#' @export
format_value_unless_integer <- function(x, digits = 2, ...) {
  if (is.numeric(x) && !all(is.int(stats::na.omit(x)))) {
    format_value(x, digits = digits)
  } else {
    x
  }
}





#' Check if integer
#'
#' @param x Numeric value.
#'
#' @export
is.int <- function(x) {
  x %% 1 == 0
}

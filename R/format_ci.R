#' Confidence/Credible Interval (CI) Formatting
#'
#' @param CI_low Lower CI bound.
#' @param CI_high Upper CI bound.
#' @param ci CI level in percentage.
#' @param digits Number of significant digits.
#' @param brackets Logical, if \code{TRUE} (default), values are encompassed in square brackets.
#' @param width_low,width_high Like \code{width}, but only applies to the lower or higher confidence interval value. This can be used when the values for the lower and upper CI are of very different length.
#' @inheritParams insight::format_value
#'
#'
#' @return A formatted string.
#' @examples
#' format_ci(1.20, 3.57, ci = 0.90)
#' format_ci(1.20, 3.57, ci = NULL)
#' format_ci(1.20, 3.57, ci = NULL, brackets = FALSE)
#' format_ci(c(1.205645, 23.4), c(3.57, -1.35), ci = 0.90)
#' format_ci(c(1.20, NA, NA), c(3.57, -1.35, NA), ci = 0.90)
#' @export
format_ci <- function(CI_low, CI_high, ci = 0.95, digits = 2, brackets = TRUE, width = NULL, width_low = width, width_high = width) {
  if (!is.null(ci)) {
    ifelse(is.na(CI_low) & is.na(CI_high), "", paste0(ci * 100, "% CI ", .format_ci(CI_low, CI_high, digits = digits, brackets = brackets, width_low = width_low, width_high = width_high)))
  } else {
    ifelse(is.na(CI_low) & is.na(CI_high), "", .format_ci(CI_low, CI_high, digits = digits, brackets = brackets, width_low = width_low, width_high = width_high))
  }
}

#' @importFrom insight format_value
#' @keywords internal
.format_ci <- function(CI_low, CI_high, digits = 2, brackets = TRUE, width_low = NULL, width_high = NULL) {
  paste0(
    ifelse(isTRUE(brackets), "[", ""),
    insight::format_value(CI_low, digits = digits, missing = "missing", width = width_low),
    ", ",
    insight::format_value(CI_high, digits = digits, missing = "missing", width = width_high),
    ifelse(isTRUE(brackets), "]", "")
  )
}

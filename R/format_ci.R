#' Confidence/Credible Interval (CI) Formatting
#'
#' @param CI_low Lower CI bound.
#' @param CI_high Upper CI bound.
#' @param ci CI level in percentage.
#' @param digits Number of significant digits.
#'
#'
#' @return A formatted string.
#' @examples
#' format_ci(1.20, 3.57, ci = 0.90)
#' format_ci(1.20, 3.57, ci = NULL)
#' format_ci(c(1.205645, 23.4), c(3.57, -1.35), ci = 0.90)
#' format_ci(c(1.20, NA, NA), c(3.57, -1.35, NA), ci = 0.90)
#' @export
format_ci <- function(CI_low, CI_high, ci = 0.95, digits = 2) {
  if (!is.null(ci)) {
    ifelse(is.na(CI_low) & is.na(CI_high), "", paste0(ci * 100, "% CI ", .format_ci(CI_low, CI_high, digits = digits)))
  } else {
    ifelse(is.na(CI_low) & is.na(CI_high), "", .format_ci(CI_low, CI_high, digits = digits))
  }
}

#' @keywords internal
.format_ci <- function(CI_low, CI_high, digits = 2){
  paste0(
    "[",
    format_value(CI_low, digits = digits, missing = "missing"),
    ", ",
    format_value(CI_high, digits = digits, missing = "missing"),
    "]")
}
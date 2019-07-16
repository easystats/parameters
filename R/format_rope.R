#' Confidence/Credible Interval (CI) Formatting
#'
#' @param rope_percentage Value or vector of ROPE-percentages.
#' @param digits Number of significant digits.
#'
#'
#' @examples
#' format_rope(0.12, 3.57)
#' @export
format_rope <- function(rope_percentage, digits = 2) {
  text <- paste0(format_value(rope_percentage * 100, digits = digits), "% in ROPE")
  return(text)
}

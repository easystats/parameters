#' Probability of direction (pd) Formatting
#'
#' @param pd Probability of direction (pd).
#' @param digits Number of significant digits.
#'
#' @examples
#' format_pd(0.12)
#' @export
format_pd <- function(pd, digits = 2) {
  text <- ifelse(pd < 1, paste0("pd = ", format_value(pd * 100), "%"), paste0("pd = 100%"))
  return(text)
}

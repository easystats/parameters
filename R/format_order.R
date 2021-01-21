#' Order (first, second, ...) formatting
#'
#' Format order.
#'
#' @param order value or vector of orders.
#' @param textual Return number as words. If \code{FALSE}, will run \code{\link[insight:format_value]{format_value()}}.
#' @inherit insight::format_number
#'
#' @return A formatted string.
#' @examples
#' format_order(2)
#' format_order(8)
#' format_order(25, textual = FALSE)
#' @importFrom insight format_value format_number
#' @importFrom utils tail
#' @export
format_order <- function(order, textual = TRUE, ...) {
  if (textual) {
    order <- insight::format_number(order)
    parts <- unlist(strsplit(order, " ", fixed = TRUE))
    parts[length(parts)] <- switch(utils::tail(parts, 1),
      "one" = "first",
      "two" = "second",
      "three" = "third",
      "four" = "fourth",
      "five" = "fifth",
      "six" = "sixth",
      "seven" = "seventh",
      "eight" = "eigth",
      "nine" = "ninth"
    )
    out <- paste(parts, collapse = " ")
  } else {
    number <- insight::format_value(order, digits = 0, ...)
    last <- substr(number, nchar(number), nchar(number))
    last_two <- substr(number, nchar(number) - 1, nchar(number))
    # exceptions
    if (last_two %in% c(11, 12, 13)) {
      out <- paste0(number, "th")
    } else {
      out <- paste0(number, switch(
        last,
        "1" = "st",
        "2" = "nd",
        "3" = "rd",
        "4" = "th",
        "5" = "th",
        "6" = "th",
        "7" = "th",
        "8" = "th",
        "9" = "th",
        "0" = "th"
      ))
    }
  }

  out
}

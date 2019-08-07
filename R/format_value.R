#' Numeric Values Formatting
#'
#' @param x Numeric value.
#' @param digits Number of significant digits.
#' @param protect_integers Should integers be kept as integers (i.e., without decimals)?
#' @param na Value by which `NA` values are replaced. By default, an empty string (i.e. \code{""}) is returned for `NA`.
#' @param ... Arguments passed to or from other methods.
#'
#'
#' @return A formatted string.
#'
#' @examples
#' format_value(1.20)
#' format_value(1.2)
#' format_value(1.2012313)
#' format_value(c(0.0045, 234, -23))
#'
#' format_value(as.factor(c("A", "B", "A")))
#' format_value(iris$Species)
#'
#' format_value(3)
#' format_value(3, protect_integers = TRUE)
#'
#' format_value(iris)
#' @export
format_value <- function(x, digits = 2, protect_integers = FALSE, na = "", ...) {
  UseMethod("format_value")
}


#' @export
format_value.data.frame <- function(x, digits = 2, protect_integers = FALSE, na = "", ...) {
  as.data.frame(sapply(x, format_value, digits = digits, protect_integers = protect_integers, na = na, simplify = FALSE))
}


#' @export
format_value.numeric <- function(x, digits = 2, protect_integers = FALSE, na = "", ...) {
  if (protect_integers) {
    out <- .format_value_unless_integer(x, digits = digits, na = na, ...)
  } else {
    out <- .format_value(x, digits = digits, na = na, ...)
  }
  out[out == "-0"] <- "0"
  out
}

#' @export
format_value.double <- format_value.numeric

#' @export
format_value.character <- format_value.numeric

#' @export
format_value.factor <- format_value.numeric

#' @export
format_value.logical <- format_value.numeric




#' @importFrom stats na.omit
#' @keywords internal
.format_value_unless_integer <- function(x, digits = 2, na = "", ...) {
  if (is.numeric(x) && !all(is.int(stats::na.omit(x)))) {
    .format_value(x, digits = digits, na = na)
  } else if (is.na(x)) {
    as.character(na)
  } else {
    as.character(x)
  }
}

#' @keywords internal
.format_value <- function(x, digits = 2, na = "", ...) {
  if (is.numeric(x)) {
    x <- ifelse(is.na(x), na, sprintf(paste0("%.", digits, "f"), x))
  } else if (is.na(x)) {
    x <- as.character(na)
  }
  x
}


#' Check if integer
#'
#' @param x Numeric value.
#'
#' @export
is.int <- function(x) {
  x %% 1 == 0
}

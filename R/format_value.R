#' Numeric Values Formatting
#'
#' @param x Numeric value.
#' @param digits Number of significant digits.
#' @param protect_integers Should integers be kept as integers (i.e., without decimals)?
#' @param missing Value by which \code{NA} values are replaced. By default, an empty string (i.e. \code{""}) is returned for code{NA}.
#' @param width Minimum width of the returned string. If not \code{NULL} and \code{width} is larger than the string's length, leading whitespaces are added to the string.
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
#' format_value(1.20, width = 10)
#'
#' format_value(as.factor(c("A", "B", "A")))
#' format_value(iris$Species)
#'
#' format_value(3)
#' format_value(3, protect_integers = TRUE)
#'
#' format_value(iris)
#' @export
format_value <- function(x, digits = 2, protect_integers = FALSE, missing = "", width = NULL, ...) {
  UseMethod("format_value")
}


#' @export
format_value.data.frame <- function(x, digits = 2, protect_integers = FALSE, missing = "", width = NULL, ...) {
  as.data.frame(sapply(x, format_value, digits = digits, protect_integers = protect_integers, missing = missing, width = width, simplify = FALSE))
}


#' @export
format_value.numeric <- function(x, digits = 2, protect_integers = FALSE, missing = "", width = NULL, ...) {
  if (protect_integers) {
    out <- .format_value_unless_integer(x, digits = digits, .missing = missing, .width = width, ...)
  } else {
    out <- .format_value(x, digits = digits, .missing = missing, .width = width, ...)
  }
  # Deal with negative zeros
  whitespace <- ifelse(is.null(width), "", " ")
  out[out == "-0"] <- paste0(whitespace, "0")
  out[out == "-0.0"] <- paste0(whitespace, "0.0")
  out[out == "-0.00"] <- paste0(whitespace, "0.00")
  out[out == "-0.000"] <- paste0(whitespace, "0.000")
  out[out == "-0.0000"] <- paste0(whitespace, "0.0000")
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
.format_value_unless_integer <- function(x, digits = 2, .missing = "", .width = NULL, ...) {
  if (is.numeric(x) && !all(is.int(stats::na.omit(x)))) {
    .format_value(x, digits = digits, .missing = .missing, .width = .width)
  } else if (anyNA(x)) {
    .convert_missing(x, .missing)
  } else {
    as.character(x)
  }
}

#' @keywords internal
.format_value <- function(x, digits = 2, .missing = "", .width = NULL, ...) {
  if (is.numeric(x)) {
    x <- ifelse(is.na(x), .missing, sprintf("%.*f", digits, x))
  } else if (anyNA(x)) {
    x <- .convert_missing(x, .missing)
  }

  if (!is.null(.width)) {
    x <- format(x, justify = "right", width = .width)
  }

  x
}


.convert_missing <- function(x, .missing) {
  if (length(x) == 1) {
    return(as.character(.missing))
  }
  missings <- which(is.na(x))
  x[missings] <- as.character(.missing)
  x[!missings] <- as.character(x)
  x
}


#' Check if integer
#'
#' @param x Numeric value.
#'
#' @export
is.int <- function(x) {
  ifelse(is.infinite(x), FALSE, x %% 1 == 0)
}

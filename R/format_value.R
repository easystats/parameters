#' Numeric Values Formatting
#'
#' @param x Numeric value.
#' @param digits Number of significant digits.
#' @param protect_integers Should integers be kept as integers (i.e., without decimals)?
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
format_value <- function(x, digits = 2, protect_integers = FALSE, ...) {
  UseMethod("format_value")

}


#' @export
format_value.data.frame <- function(x, digits = 2, protect_integers = FALSE, ...) {
  as.data.frame(sapply(x, format_value, digits = digits, protect_integers = protect_integers, simplify = FALSE))
}


#' @export
format_value.numeric <- function(x, digits = 2, protect_integers = FALSE, ...) {
  if(protect_integers){
    .format_value_unless_integer(x, digits = digits, ...)
  } else{
    .format_value(x, digits = digits, ...)
  }
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
.format_value_unless_integer <- function(x, digits = 2, ...) {
  if (is.numeric(x) && !all(is.int(stats::na.omit(x)))) {
    .format_value(x, digits = digits)
  } else {
    as.character(x)
  }
}

#' @keywords internal
.format_value <- function(x, digits = 2, ...) {
  if (is.numeric(x)) {
    x <- ifelse(is.na(x), NA, sprintf(paste0("%.", digits, "f"), x))
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

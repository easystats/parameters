#' Compute Skewness and Kurtosis
#'
#' @param x A numeric vector or data.frame.
#' @param na.rm Remove missing values.
#'
#' @examples
#' skewness(rnorm(1000))
#' kurtosis(rnorm(1000))
#'
#' @export
skewness <- function(x, ...) {
  UseMethod("skewness")
}

#' @export
skewness.numeric <- function(x, na.rm = TRUE, ...) {
  if (na.rm) x <- x[!is.na(x)]
  n <- length(x)
  (sum((x - mean(x))^3) / n) / (sum((x - mean(x))^2) / n)^1.5
}

#' @export
skewness.matrix <- function(x, na.rm = TRUE, ...) {
  apply(x, 2, skewness, na.rm = na.rm)
}

#' @export
skewness.data.frame <- function(x, na.rm = TRUE, ...) {
  sapply(x, skewness, na.rm = na.rm)
}

#' @export
skewness.default <- function(x, na.rm = TRUE, ...) {
  skewness(as.vector(x), na.rm = na.rm)
}



#' @rdname skewness
#' @export
kurtosis <- function(x, ...) {
  UseMethod("kurtosis")
}

#' @export
kurtosis.numeric <- function(x, na.rm = TRUE, ...) {
  if (na.rm) x <- x[!is.na(x)]
  n <- length(x)
  n * sum((x - mean(x))^4) / (sum((x - mean(x))^2)^2) - 3
}

#' @export
kurtosis.matrix <- function(x, na.rm = TRUE, ...) {
  apply(x, 2, kurtosis, na.rm = na.rm)
}

#' @export
kurtosis.data.frame <- function(x, na.rm = TRUE, ...) {
  sapply(x, kurtosis, na.rm = na.rm)
}

#' @export
kurtosis.default <- function(x, na.rm = TRUE, ...) {
  kurtosis(as.vector(x), na.rm = na.rm)
}

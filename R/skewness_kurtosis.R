#' Compute Skewness and Kurtosis
#'
#'
#' @param x A numeric vector or data.frame.
#' @param na.rm Remove NaNs.
#'
#' @examples
#' skewness(rnorm(1000))
#' kurtosis(rnorm(1000))
#' @references All credits go to the \code{moments} package.
#' @export
skewness <- function(x, na.rm = TRUE) {
  if (is.matrix(x)) {
    apply(x, 2, skewness, na.rm = na.rm)
  } else if (is.vector(x)) {
    if (na.rm) x <- x[!is.na(x)]
    n <- length(x)
    (sum((x - mean(x))^3) / n) / (sum((x - mean(x))^2) / n)^(3 / 2)
  }
  else if (is.data.frame(x)) {
    sapply(x, skewness, na.rm = na.rm)
  } else {
    skewness(as.vector(x), na.rm = na.rm)
  }
}





#' @rdname skewness
#' @export
kurtosis <- function(x, na.rm = TRUE) {
  if (is.matrix(x)) {
    apply(x, 2, kurtosis, na.rm = na.rm)
  } else if (is.vector(x)) {
    if (na.rm) x <- x[!is.na(x)]
    n <- length(x)
    n * sum((x - mean(x))^4) / (sum((x - mean(x))^2)^2)
  }
  else if (is.data.frame(x)) {
    sapply(x, kurtosis, na.rm = na.rm)
  } else {
    kurtosis(as.vector(x), na.rm = na.rm)
  }
}

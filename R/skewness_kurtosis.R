#' Compute Skewness and Kurtosis
#'
#' @param x A numeric vector or data.frame.
#' @param na.rm Remove missing values.
#'
#' @details \strong{Skewness}
#' \cr \cr
#' Symmetric distributions have a \code{skewness} around zero, while
#' a negative skewness values indicates a "left-skewed" distribution, and a
#' positive skewness values indicates a "right-skewed" distribution. Examples
#' for the relationship of skewness and distributions are:
#' \itemize{
#'   \item Normal distribution (and other symmetric distribution) has a skewness of 0
#'   \item Half-normal distribution has a skewness just below 1
#'   \item Exponential distribution has a skewness of 2
#'   \item Lognormal distribution can have a skewness of any positive value, depending on its parameters
#' }
#' (\cite{https://en.wikipedia.org/wiki/Skewness})
#' \cr \cr
#' \strong{Kurtosis}
#' \cr \cr
#' The \code{kurtosis} is a measure of "tailedness" of a distribution. A distribution
#' with a kurtosis valus of about zero is called "mesokurtic". A kurtosis value
#' larger than zero indicates a "leptokurtic" distribution with \emph{fatter} tails.
#' A kurtosis value below zero indicates a "platykurtic" distribution with \emph{thinner}
#' tails (\cite{https://en.wikipedia.org/wiki/Kurtosis}).
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

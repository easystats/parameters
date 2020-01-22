#' Compute Skewness and Kurtosis
#'
#' @param x A numeric vector or data.frame.
#' @param na.rm Remove missing values.
#' @param type Type of algorithm for computing skewness. May be an integer from 1 to 3. See 'Details'.
#' @param ... Arguments passed to or from other methods.
#'
#' @details \subsection{Skewness}{
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
#' }
#' \subsection{Types of Skewness}{
#' \code{skewness()} supports three different methods for estimating skewness, as discussed in \cite{Joanes and Gill (1988)}:
#' \itemize{
#' \item Type 1 is the "classical" method, which is \code{(sum((x - mean(x))^3) / n) / (sum((x - mean(x))^2) / n)^1.5}
#' \item Type 2 first calculates the type-1 skewness, than adjusts the result: \code{type-1-skewness * sqrt(n * (n - 1)) / (n - 2)}. This is what SAS and SPSS usually return
#' \item Type 3 first calculates the type-1 skewness, than adjusts the result: \code{type-1-skewness * ((1 - 1 / n))^1.5}. This is what Minitab usually returns.
#' }
#' }
#' \subsection{Kurtosis}{
#' The \code{kurtosis} is a measure of "tailedness" of a distribution. A distribution
#' with a kurtosis values of about zero is called "mesokurtic". A kurtosis value
#' larger than zero indicates a "leptokurtic" distribution with \emph{fatter} tails.
#' A kurtosis value below zero indicates a "platykurtic" distribution with \emph{thinner}
#' tails (\cite{https://en.wikipedia.org/wiki/Kurtosis}).
#' }
#'
#' @references D. N. Joanes and C. A. Gill (1998). Comparing measures of sample skewness and kurtosis. The Statistician, 47, 183–189.
#'
#' @return Values of skewness or kurtosis.
#' @examples
#' skewness(rnorm(1000))
#' kurtosis(rnorm(1000))
#' @export
skewness <- function(x, na.rm = TRUE, type = 2, ...) {
  UseMethod("skewness")
}

#' @export
skewness.numeric <- function(x, na.rm = TRUE, type = 2, ...) {
  if (na.rm) x <- x[!is.na(x)]
  n <- length(x)
  out <- (sum((x - mean(x))^3) / n) / (sum((x - mean(x))^2) / n)^1.5

  if (type == 2) {
    if (n < 3) {
      warning("Need at least 3 complete observations for type-2-skewness. Using type-1 now.", call. = FALSE)
    } else {
      out <- out * sqrt(n * (n - 1)) / (n - 2)
    }
  } else if (type == 3) {
    out <- out * ((1 - 1 / n))^1.5
  }

  out
}

#' @export
skewness.matrix <- function(x, na.rm = TRUE, type = 2, ...) {
  apply(x, 2, skewness, na.rm = na.rm, type = type)
}

#' @export
skewness.data.frame <- function(x, na.rm = TRUE, type = 2, ...) {
  sapply(x, skewness, na.rm = na.rm, type = type)
}

#' @export
skewness.default <- function(x, na.rm = TRUE, type = 2, ...) {
  skewness(as.vector(x), na.rm = na.rm, type = type)
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

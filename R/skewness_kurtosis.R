#' Compute Skewness and Kurtosis
#'
#' @param x A numeric vector or data.frame.
#' @param na.rm Remove missing values.
#' @param type Type of algorithm for computing skewness. May be one of \code{1} (or \code{"1"}, \code{"I"} or \code{"classic"}), \code{2} (or \code{"2"}, \code{"II"} or \code{"SPSS"} or \code{"SAS"}) or \code{3} (or  \code{"3"}, \code{"III"} or \code{"Minitab"}). See 'Details'.
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
#' \item Type "1" is the "classical" method, which is \code{(sum((x - mean(x))^3) / n) / (sum((x - mean(x))^2) / n)^1.5}
#' \item Type "2" first calculates the type-1 skewness, than adjusts the result: \code{type-1-skewness * sqrt(n * (n - 1)) / (n - 2)}. This is what SAS and SPSS usually return
#' \item Type "3" first calculates the type-1 skewness, than adjusts the result: \code{type-1-skewness * ((1 - 1 / n))^1.5}. This is what Minitab usually returns.
#' }
#' }
#' \subsection{Kurtosis}{
#' The \code{kurtosis} is a measure of "tailedness" of a distribution. A distribution
#' with a kurtosis values of about zero is called "mesokurtic". A kurtosis value
#' larger than zero indicates a "leptokurtic" distribution with \emph{fatter} tails.
#' A kurtosis value below zero indicates a "platykurtic" distribution with \emph{thinner}
#' tails (\cite{https://en.wikipedia.org/wiki/Kurtosis}).
#' }
#' \subsection{Types of Kurtosis}{
#' \code{kurtosis()} supports three different methods for estimating kurtosis, as discussed in \cite{Joanes and Gill (1988)}:
#' \itemize{
#' \item Type "1" is the "classical" method, which is \code{n * sum((x - mean(x))^4) / (sum((x - mean(x))^2)^2) - 3}.
#' \item Type "2" first calculates the type-1 kurtosis, than adjusts the result: \code{((n + 1) * type-1-kurtosis + 6) * (n - 1)/((n - 2) * (n - 3))}. This is what SAS and SPSS usually return
#' \item Type "3" first calculates the type-1 kurtosis, than adjusts the result: \code{(type-1-kurtosis + 3) * (1 - 1 / n)^2 - 3}. This is what Minitab usually returns.
#' }
#' }
#'
#' @references D. N. Joanes and C. A. Gill (1998). Comparing measures of sample skewness and kurtosis. The Statistician, 47, 183–189.
#'
#' @return Values of skewness or kurtosis.
#' @examples
#' skewness(rnorm(1000))
#' kurtosis(rnorm(1000))
#' @export
skewness <- function(x, na.rm = TRUE, type = "2", ...) {
  UseMethod("skewness")
}

#' @export
skewness.numeric <- function(x, na.rm = TRUE, type = "2", ...) {
  if (na.rm) x <- x[!is.na(x)]
  n <- length(x)
  out <- (sum((x - mean(x))^3) / n) / (sum((x - mean(x))^2) / n)^1.5

  # convenience
  if (is.numeric(type)) type <- as.character(type)

  if (!.skewness_type_ok(type)) {
    warning("'type' must be a character value from \"1\" to \"3\". Using 'type=\"2\"' now.", call. = FALSE)
    type <- "2"
  }

  if (type %in% c("2", "II", "SPSS", "SAS") && n < 3) {
    warning("Need at least 3 complete observations for type-2-skewness. Using 'type=\"1\"' now.", call. = FALSE)
    type <- "1"
  }

  switch(
    type,
    "1" = ,
    "I" = ,
    "classic" = out,
    "2" = ,
    "II" = ,
    "SPSS" = ,
    "SAS" = out * sqrt(n * (n - 1)) / (n - 2),
    "3" = ,
    "III" = ,
    "Minitab" = out * ((1 - 1 / n))^1.5
  )
}

#' @export
skewness.matrix <- function(x, na.rm = TRUE, type = "2", ...) {
  apply(x, 2, skewness, na.rm = na.rm, type = type)
}

#' @export
skewness.data.frame <- function(x, na.rm = TRUE, type = "2", ...) {
  sapply(x, skewness, na.rm = na.rm, type = type)
}

#' @export
skewness.default <- function(x, na.rm = TRUE, type = "2", ...) {
  skewness(as.vector(x), na.rm = na.rm, type = type)
}



#' @rdname skewness
#' @export
kurtosis <- function(x, na.rm = TRUE, type = "2", ...) {
  UseMethod("kurtosis")
}

#' @export
kurtosis.numeric <- function(x, na.rm = TRUE, type = "2", ...) {
  if (na.rm) x <- x[!is.na(x)]
  n <- length(x)
  out <- n * sum((x - mean(x))^4) / (sum((x - mean(x))^2)^2)

  # convenience
  if (is.numeric(type)) type <- as.character(type)

  if (!.skewness_type_ok(type)) {
    warning("'type' must be a character value from \"1\" to \"3\". Using 'type=\"2\"' now.", call. = FALSE)
    type <- "2"
  }

  if (type %in% c("2", "II", "SPSS", "SAS") && n < 4) {
    warning("Need at least 4 complete observations for type-2-kurtosis Using 'type=\"1\"' now.", call. = FALSE)
    type <- "1"
  }

  switch(
    type,
    "1" = ,
    "I" = ,
    "classic" = out - 3,
    "2" = ,
    "II" = ,
    "SPSS" = ,
    "SAS" = ((n + 1) * (out - 3) + 6) * (n - 1)/((n - 2) * (n - 3)),
    "3" = ,
    "III" = ,
    "Minitab" = out * (1 - 1 / n)^2 - 3
  )
}

#' @export
kurtosis.matrix <- function(x, na.rm = TRUE, type = "2", ...) {
  apply(x, 2, kurtosis, na.rm = na.rm, type = type)
}

#' @export
kurtosis.data.frame <- function(x, na.rm = TRUE, type = "2", ...) {
  sapply(x, kurtosis, na.rm = na.rm, type = type)
}

#' @export
kurtosis.default <- function(x, na.rm = TRUE, type = "2", ...) {
  kurtosis(as.vector(x), na.rm = na.rm, type = type)
}




.skewness_type_ok <- function(type) {
  !is.null(type) && !is.na(type) && (type %in% c("1", "2", "3", "I", "II", "III", "classic", "SPSS", "SAS", "Minitab"))
}

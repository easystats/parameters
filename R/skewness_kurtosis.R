#' Compute Skewness and Kurtosis
#'
#' @param x A numeric vector or data.frame.
#' @param na.rm Remove missing values.
#' @param type Type of algorithm for computing skewness. May be one of \code{1} (or \code{"1"}, \code{"I"} or \code{"classic"}), \code{2} (or \code{"2"}, \code{"II"} or \code{"SPSS"} or \code{"SAS"}) or \code{3} (or  \code{"3"}, \code{"III"} or \code{"Minitab"}). See 'Details'.
#' @param iterations The number of bootstrap replicates for computing standard errors. If \code{NULL} (default), no standard errors are computed.
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
#' \item Type "1" is the "classical" method, which is \code{g1 = (sum((x - mean(x))^3) / n) / (sum((x - mean(x))^2) / n)^1.5}
#' \item Type "2" first calculates the type-1 skewness, than adjusts the result: \code{G1 = g1 * sqrt(n * (n - 1)) / (n - 2)}. This is what SAS and SPSS usually return
#' \item Type "3" first calculates the type-1 skewness, than adjusts the result: \code{b1 = g1 * ((1 - 1 / n))^1.5}. This is what Minitab usually returns.
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
#' \item Type "1" is the "classical" method, which is \code{g2 = n * sum((x - mean(x))^4) / (sum((x - mean(x))^2)^2) - 3}.
#' \item Type "2" first calculates the type-1 kurtosis, than adjusts the result: \code{G2 = ((n + 1) * g2 + 6) * (n - 1)/((n - 2) * (n - 3))}. This is what SAS and SPSS usually return
#' \item Type "3" first calculates the type-1 kurtosis, than adjusts the result: \code{b2 = (g2 + 3) * (1 - 1 / n)^2 - 3}. This is what Minitab usually returns.
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

#' @rdname skewness
#' @importFrom stats sd
#' @export
skewness.numeric <- function(x, na.rm = TRUE, type = "2", iterations = NULL, ...) {
  if (na.rm) x <- x[!is.na(x)]
  n <- length(x)
  out <- (sum((x - mean(x))^3) / n) / (sum((x - mean(x))^2) / n)^1.5

  type <- .check_skewness_type(type)

  if (type == "2" && n < 3) {
    warning("Need at least 3 complete observations for type-2-skewness. Using 'type=\"1\"' now.", call. = FALSE)
    type <- "1"
  }

  .skewness <- switch(
    type,
    "1" = out,
    "2" = out * sqrt(n * (n - 1)) / (n - 2),
    "3" = out * ((1 - 1 / n))^1.5
  )

  # out_se <- sqrt((6 * (n - 2)) / ((n + 1) * (n + 3)))
  #
  # .skewness_se <- switch(
  #   type,
  #   "1" = out_se,
  #   "2" = out_se * ((sqrt(n * (n - 1))) / (n - 2)),
  #   "3" = out_se * (((n - 1) / n)^1.5),
  # )

  if (!is.null(iterations )) {
    if (!requireNamespace("boot", quietly = TRUE)) {
      warning("Package 'boot' needed for bootstrapping SEs.", call. = FALSE)
    } else {
      results <- boot::boot(data = x, statistic = .boot_skewness, R = iterations, na.rm = na.rm, type = type)
      attr(.skewness, "SE") <- stats::sd(results$t, na.rm = TRUE)
      class(.skewness) <- unique(c("parameters_skewness", class(.skewness)))
    }
  }

  .skewness
}

#' @export
skewness.matrix <- function(x, na.rm = TRUE, type = "2", ...) {
  apply(x, 2, skewness, na.rm = na.rm, type = type)
}

#' @export
skewness.data.frame <- function(x, na.rm = TRUE, type = "2", ...) {
  sapply(x, skewness, na.rm = na.rm, type = type)
  # out <- lapply(x, skewness, na.rm = na.rm, type = type)
  # out <- data.frame(
  #   Parameter = names(out),
  #   Skewness = unname(sapply(out, as.vector)),
  #   SE = unname(sapply(out, function(i) attributes(i)$SE)),
  #   stringsAsFactors = FALSE
  # )
  #
  # class(out) <- c("parameters_skewness", "data.frame")
  # out
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


#' @rdname skewness
#' @export
kurtosis.numeric <- function(x, na.rm = TRUE, type = "2", iterations = NULL, ...) {
  if (na.rm) x <- x[!is.na(x)]
  n <- length(x)
  out <- n * sum((x - mean(x))^4) / (sum((x - mean(x))^2)^2)

  type <- .check_skewness_type(type)

  if (type == "2" && n < 4) {
    warning("Need at least 4 complete observations for type-2-kurtosis Using 'type=\"1\"' now.", call. = FALSE)
    type <- "1"
  }

  .kurtosis <- switch(
    type,
    "1" = out - 3,
    "2" = ((n + 1) * (out - 3) + 6) * (n - 1)/((n - 2) * (n - 3)),
    "3" = out * (1 - 1 / n)^2 - 3
  )

  # out_se <- sqrt((24 * n * (n - 2) * (n - 3)) / (((n + 1)^2) * (n + 3) * (n + 5)))
  #
  # .kurtosis_se <- switch(
  #   type,
  #   "1" = out_se,
  #   "2" = out_se * (((n - 1) * (n + 1)) / ((n - 2) * (n - 3))),
  #   "3" = out_se * ((n - 1) / n)^2
  # )
  #

  if (!is.null(iterations )) {
    if (!requireNamespace("boot", quietly = TRUE)) {
      warning("Package 'boot' needed for bootstrapping SEs.", call. = FALSE)
    } else {
      results <- boot::boot(data = x, statistic = .boot_skewness, R = iterations, na.rm = na.rm, type = type)
      attr(.kurtosis, "SE") <- stats::sd(results$t, na.rm = TRUE)
      class(.kurtosis) <- unique(c("parameters_kurtosis", class(.kurtosis)))
    }
  }

  .kurtosis
}

#' @export
kurtosis.matrix <- function(x, na.rm = TRUE, type = "2", ...) {
  apply(x, 2, kurtosis, na.rm = na.rm, type = type)
}

#' @export
kurtosis.data.frame <- function(x, na.rm = TRUE, type = "2", ...) {
  sapply(x, kurtosis, na.rm = na.rm, type = type)

  # out <- lapply(x, kurtosis, na.rm = na.rm, type = type)
  # out <- data.frame(
  #   Parameter = names(out),
  #   Kurtosis = unname(sapply(out, as.vector)),
  #   SE = unname(sapply(out, function(i) attributes(i)$SE)),
  #   stringsAsFactors = FALSE
  # )
  #
  # class(out) <- c("parameters_kurtosis", "data.frame")
  # out
}

#' @export
kurtosis.default <- function(x, na.rm = TRUE, type = "2", ...) {
  kurtosis(as.vector(x), na.rm = na.rm, type = type)
}




.check_skewness_type <- function(type) {
  # convenience
  if (is.numeric(type)) type <- as.character(type)

  if (is.null(type) || is.na(type) || !(type %in% c("1", "2", "3", "I", "II", "III", "classic", "SPSS", "SAS", "Minitab"))) {
    warning("'type' must be a character value from \"1\" to \"3\". Using 'type=\"2\"' now.", call. = FALSE)
    type <- "2"
  }

  switch(
    type,
    "1" = ,
    "I" = ,
    "classic" = "1",
    "2" = ,
    "II" = ,
    "SPSS" = ,
    "SAS" = "2",
    "3" = ,
    "III" = ,
    "Minitab" = "3"
  )
}



#' @export
print.parameters_skewness <- function(x, digits = 3, ...) {
  if (is.numeric(x)) {
    cat(sprintf("Skewness (SE): %.*f (%.*f)\n", digits, as.vector(x), digits, attributes(x)$SE))
  } else if (is.data.frame(x)) {
    cat(insight::format_table(x, digits = digits))
  } else {
    NextMethod()
  }
}


#' @export
print.parameters_kurtosis <- function(x, digits = 3, ...) {
  if (is.numeric(x)) {
    cat(sprintf("Kurtosis (SE): %.*f (%.*f)\n", digits, as.vector(x), digits, attributes(x)$SE))
  } else if (is.data.frame(x)) {
    cat(insight::format_table(x, digits = digits))
  } else {
    NextMethod()
  }
}






# bootstrapping -----------------------------------

.boot_skewness <- function(data, indices, na.rm, type) {
  parameters::skewness(data[indices], na.rm = na.rm, type = type, iterations = NULL)
}


.boot_kurtosis <- function(data, indices, na.rm, type) {
  parameters::kurtosis(data[indices], na.rm = na.rm, type = type, iterations = NULL)
}

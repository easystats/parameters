#' Compute Skewness and (Excess) Kurtosis
#'
#' @param x A numeric vector or data.frame.
#' @param na.rm Remove missing values.
#' @param type Type of algorithm for computing skewness. May be one of \code{1}
#'   (or \code{"1"}, \code{"I"} or \code{"classic"}), \code{2} (or \code{"2"},
#'   \code{"II"} or \code{"SPSS"} or \code{"SAS"}) or \code{3} (or  \code{"3"},
#'   \code{"III"} or \code{"Minitab"}). See 'Details'.
#' @param iterations The number of bootstrap replicates for computing standard
#'   errors. If \code{NULL} (default), parametric standard errors are computed.
#'   See 'Details'.
#' @param test Logical, if \code{TRUE}, tests if skewness or kurtosis is
#'   significantly different from zero.
#' @param digits Number of decimal places.
#' @param object An object returned by \code{skewness()} or \code{kurtosis()}.
#' @param ... Arguments passed to or from other methods.
#'
#' @details \subsection{Skewness}{
#' Symmetric distributions have a \code{skewness} around zero, while
#' a negative skewness values indicates a "left-skewed" distribution, and a
#' positive skewness values indicates a "right-skewed" distribution. Examples
#' for the relationship of skewness and distributions are:
#' \itemize{
#'   \item Normal distribution (and other symmetric distribution) has a skewness
#'   of 0
#'   \item Half-normal distribution has a skewness just below 1
#'   \item Exponential distribution has a skewness of 2
#'   \item Lognormal distribution can have a skewness of any positive value,
#'   depending on its parameters
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
#' \subsection{Standard Errors}{
#' It is recommended to compute empirical (bootstrapped) standard errors (via the \code{iterations} argument) than relying on analytic standard errors (\cite{Wright & Herrington, 2011}).
#' }
#'
#' @references
#' \itemize{
#'   \item D. N. Joanes and C. A. Gill (1998). Comparing measures of sample skewness and kurtosis. The Statistician, 47, 183–189.
#'   \item Wright, D. B., & Herrington, J. A. (2011). Problematic standard errors and confidence intervals for skewness and kurtosis. Behavior research methods, 43(1), 8-17.
#' }
#'
#'
#' @return Values of skewness or kurtosis.
#'
#' @examples
#' skewness(rnorm(1000))
#' kurtosis(rnorm(1000))
#' @export
skewness <- function(x, na.rm = TRUE, type = "2", iterations = NULL, ...) {
  UseMethod("skewness")
}


# skewness -----------------------------------------


#' @importFrom stats sd pnorm
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

  .skewness <- switch(type,
    "1" = out,
    "2" = out * sqrt(n * (n - 1)) / (n - 2),
    "3" = out * ((1 - 1 / n))^1.5
  )

  out_se <- sqrt((6 * (n - 2)) / ((n + 1) * (n + 3)))

  .skewness_se <- switch(type,
    "1" = out_se,
    "2" = out_se * ((sqrt(n * (n - 1))) / (n - 2)),
    "3" = out_se * (((n - 1) / n)^1.5),
  )

  if (!is.null(iterations)) {
    if (!requireNamespace("boot", quietly = TRUE)) {
      warning("Package 'boot' needed for bootstrapping SEs.", call. = FALSE)
    } else {
      results <- boot::boot(
        data = x,
        statistic = .boot_skewness,
        R = iterations,
        na.rm = na.rm,
        type = type
      )
      out_se <- stats::sd(results$t, na.rm = TRUE)
    }
  }

  .skewness <- data.frame(
    Skewness = .skewness,
    SE = out_se
  )
  class(.skewness) <- unique(c("parameters_skewness", class(.skewness)))
  .skewness
}



#' @export
skewness.matrix <- function(x, na.rm = TRUE, type = "2", iterations = NULL, ...) {
  .skewness <- apply(x, 2, skewness, na.rm = na.rm, type = type, iterations = iterations)
  .names <- colnames(x)
  if (length(.names) == 0) {
    .names <- paste0("X", seq_len(ncol(x)))
  }
  .skewness <- cbind(Parameter = .names, do.call(rbind, .skewness))
  class(.skewness) <- unique(c("parameters_skewness", class(.skewness)))
  .skewness
}



#' @export
skewness.data.frame <- function(x, na.rm = TRUE, type = "2", iterations = NULL, ...) {
  .skewness <- lapply(x, skewness, na.rm = na.rm, type = type, iterations = iterations)
  .skewness <- cbind(Parameter = names(.skewness), do.call(rbind, .skewness))
  class(.skewness) <- unique(c("parameters_skewness", class(.skewness)))
  .skewness
}



#' @export
skewness.default <- function(x, na.rm = TRUE, type = "2", iterations = NULL, ...) {
  skewness(.factor_to_numeric(x), na.rm = na.rm, type = type, iterations = iterations)
}








# Kurtosis -----------------------------------


#' @rdname skewness
#' @export
kurtosis <- function(x, na.rm = TRUE, type = "2", iterations = NULL, ...) {
  UseMethod("kurtosis")
}



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

  .kurtosis <- switch(type,
    "1" = out - 3,
    "2" = ((n + 1) * (out - 3) + 6) * (n - 1) / ((n - 2) * (n - 3)),
    "3" = out * (1 - 1 / n)^2 - 3
  )

  out_se <- sqrt((24 * n * (n - 2) * (n - 3)) / (((n + 1)^2) * (n + 3) * (n + 5)))

  .kurtosis_se <- switch(type,
    "1" = out_se,
    "2" = out_se * (((n - 1) * (n + 1)) / ((n - 2) * (n - 3))),
    "3" = out_se * ((n - 1) / n)^2
  )

  if (!is.null(iterations)) {
    if (!requireNamespace("boot", quietly = TRUE)) {
      warning("Package 'boot' needed for bootstrapping SEs.", call. = FALSE)
    } else {
      results <- boot::boot(
        data = x,
        statistic = .boot_kurtosis,
        R = iterations,
        na.rm = na.rm,
        type = type
      )
      out_se <- stats::sd(results$t, na.rm = TRUE)
    }
  }

  .kurtosis <- data.frame(
    Kurtosis = .kurtosis,
    SE = out_se
  )
  class(.kurtosis) <- unique(c("parameters_kurtosis", class(.kurtosis)))
  .kurtosis
}



#' @export
kurtosis.matrix <- function(x, na.rm = TRUE, type = "2", iterations = NULL, ...) {
  .kurtosis <- apply(x, 2, kurtosis, na.rm = na.rm, type = type, iterations = iterations)
  .names <- colnames(x)
  if (length(.names) == 0) {
    .names <- paste0("X", seq_len(ncol(x)))
  }
  .kurtosis <- cbind(Parameter = .names, do.call(rbind, .kurtosis))
  class(.kurtosis) <- unique(c("parameters_kurtosis", class(.kurtosis)))
  .kurtosis
}



#' @export
kurtosis.data.frame <- function(x, na.rm = TRUE, type = "2", iterations = NULL, ...) {
  .kurtosis <- lapply(x, kurtosis, na.rm = na.rm, type = type, iterations = iterations)
  .kurtosis <- cbind(Parameter = names(.kurtosis), do.call(rbind, .kurtosis))
  class(.kurtosis) <- unique(c("parameters_kurtosis", class(.kurtosis)))
  .kurtosis
}



#' @export
kurtosis.default <- function(x, na.rm = TRUE, type = "2", iterations = NULL, ...) {
  kurtosis(.factor_to_numeric(x), na.rm = na.rm, type = type, iterations = iterations)
}





# methods -----------------------------------------

#' @export
as.numeric.parameters_kurtosis <- function(x, ...) {
  x$Kurtosis
}

#' @export
as.numeric.parameters_skewness <- function(x, ...) {
  x$Skewness
}

#' @export
as.double.parameters_kurtosis <- as.numeric.parameters_kurtosis

#' @export
as.double.parameters_skewness <- as.numeric.parameters_skewness

#' @rdname skewness
#' @export
print.parameters_kurtosis <- function(x, digits = 3, test = FALSE, ...) {
  out <- summary(x, test = test)
  cat(insight::export_table(out, digits = digits))
  invisible(x)
}

#' @rdname skewness
#' @export
print.parameters_skewness <- print.parameters_kurtosis

#' @importFrom stats pnorm
#' @rdname skewness
#' @export
summary.parameters_skewness <- function(object, test = FALSE, ...) {
  if (test) {
    object$z <- object$Skewness / object$SE
    object$p <- 2 * (1 - stats::pnorm(abs(object$z)))
  }
  object
}

#' @rdname skewness
#' @export
summary.parameters_kurtosis <- function(object, test = FALSE, ...) {
  if (test) {
    object$z <- object$Kurtosis / object$SE
    object$p <- 2 * (1 - stats::pnorm(abs(object$z)))
  }
  object
}





# helper ------------------------------------------


.check_skewness_type <- function(type) {
  # convenience
  if (is.numeric(type)) type <- as.character(type)

  if (is.null(type) || is.na(type) || !(type %in% c("1", "2", "3", "I", "II", "III", "classic", "SPSS", "SAS", "Minitab"))) {
    warning("'type' must be a character value from \"1\" to \"3\". Using 'type=\"2\"' now.", call. = FALSE)
    type <- "2"
  }

  switch(type,
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



# bootstrapping -----------------------------------

.boot_skewness <- function(data, indices, na.rm, type) {
  parameters::skewness(data[indices],
    na.rm = na.rm,
    type = type,
    iterations = NULL
  )$Skewness
}


.boot_kurtosis <- function(data, indices, na.rm, type) {
  parameters::kurtosis(data[indices],
    na.rm = na.rm,
    type = type,
    iterations = NULL
  )$Kurtosis
}

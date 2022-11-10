#' @title Calculate calibrated p-values.
#' @name p_calibrate
#'
#' @description Compute calibrated p-values that can be interpreted
#' probabilistically, i.e. as posterior probability of H0 (given that H0
#' and H1 have equal prior probabilities).
#'
#' @param x A numeric vector of p-values, or a regression model object.
#' @param type Type of calibration. Can be `"frequentist"` or `"bayesian"`.
#' See 'Details'.
#' @param ... Currently not used.
#'
#' @return A data frame with p-values and calibrated p-values.
#'
#' @details
#' The Bayesian calibration, i.e. when `type = "bayesian"`, can be interpreted
#' as the lower bound of the Bayes factor for H0 to H1, based on the data.
#' The full Bayes factor would then require multiplying by the prior odds of
#' H0 to H1. The frequentist calibration also has a Bayesian interpretation; it
#' is the posterior probability of H0, assuming that H0 and H1 have equal
#' prior probabilities of 0.5 each (_Sellke et al. 2001_).
#'
#' The calibration only works for p-values lower than or equal to `1/e`.
#'
#' @references
#' Thomas Sellke, M. J Bayarri and James O Berger (2001) Calibration of p Values
#' for Testing Precise Null Hypotheses, The American Statistician, 55:1, 62-71,
#' \doi{10.1198/000313001300339950}
#'
#' @examples
#' model <- lm(mpg ~ wt + as.factor(gear) + am, data = mtcars)
#' p_calibrate(model, verbose = FALSE)
#' @export
p_calibrate <- function(x, ...) {
  UseMethod("p_calibrate")
}

#' @export
p_calibrate.numeric <- function(x, type = "frequentist", verbose = TRUE, ...) {
  type <- match.arg(tolower(type), choices = c("frequentist", "bayesian"))

  # fill p-values larger than calibration cut-off with `NA`
  x[x > (1 / exp(1))] <- NA

  if (type == "bayesian") {
    calibrated <- (-exp(1) * x * log(x))
  } else {
    calibrated <- 1 / (1 + (1 / (-exp(1) * x * log(x))))
  }

  if (verbose && anyNA(calibrated)) {
    insight::format_warning(
      "Some p-values were larger than the calibration cut-off.",
      "Returning `NA` for p-values that cannot be calibrated."
    )
  }

  calibrated
}

#' @export
p_calibrate.default <- function(x, type = "frequentist", verbose = TRUE, ...) {
  if (!insight::is_model(x)) {
    insight::format_error("`p_calibrate()` requires a valid model object.")
  }
  out <- p_value(x)
  out$p_calibrated <- p_calibrate(out$p, type = type, verbose = FALSE, ...)

  if (verbose && anyNA(out$p_calibrated)) {
    insight::format_warning(
      "Some p-values were larger than the calibration cut-off.",
      "Returning `NA` for p-values that cannot be calibrated."
    )
  }

  class(out) <- c("p_calibrate", "data.frame")
  attr(out, "type") <- type
  out
}


# methods -----------------

#' @export
format.p_calibrate <- function(x, ...) {
  insight::format_table(x, ...)
}

#' @export
print.p_calibrate <- function(x, ...) {
  formatted <- format(x, ...)
  footer <- switch(attributes(x)$type,
    "frequentist" = "Calibrated p-values indicate the posterior probability of H0.\n",
    "Calibrated p-values indicate the Bayes Factor (evidence) in favor of H0 over H1.\n"
  )
  cat(insight::export_table(formatted, footer = c(footer, "blue"), ...))
}

#' p-values for Models with Zero-Inflation
#'
#' This function attempts to return, or compute, p-values of hurdle and zero-inflated models.
#'
#' @param model A statistical model.
#' @param component Should all parameters, parameters for the conditional model, precision-component or marginal effects be returned? \code{component} may be one of \code{"conditional"}, \code{"precision"}, \code{"marginal"} or \code{"all"} (default).
#' @inheritParams p_value
#' @inheritParams simulate_model
#' @inheritParams standard_error
#' @inheritParams ci.merMod
#'
#' @return The p-values.
#'
#' @examples
#' if (require("mfx")) {
#'   set.seed(12345)
#'   n <- 1000
#'   x <- rnorm(n)
#'   y <- rnegbin(n, mu = exp(1 + 0.5 * x), theta = 0.5)
#'   d <- data.frame(y, x)
#'   model <- poissonmfx(y ~ x, data = d)
#'
#'   p_value(model)
#'   p_value(model, component = "marginal")
#' }
#' @importFrom stats coef
#' @importFrom insight get_parameters
#' @export
p_value.poissonmfx <- function(model, component = c("all", "conditional", "marginal"), ...) {
  parms <- insight::get_parameters(model, component = "all")
  cs <- stats::coef(summary(model$fit))
  p <- c(as.vector(model$mfxest[, 4]), as.vector(cs[, 4]))

  out <- .data_frame(
    Parameter = parms$Parameter,
    p = p,
    Component = parms$Component
  )

  component <- match.arg(component)
  if (component != "all") {
    out <- out[out$Component == component, ]
  }

  out
}



#' @export
p_value.logitor <- function(model, method = NULL, ...) {
  p_value.default(model$fit, method = method, ...)
}

#' @export
p_value.poissonirr <- p_value.logitor

#' @export
p_value.negbinirr <- p_value.logitor

#' @export
p_value.logitmfx <- p_value.poissonmfx

#' @export
p_value.probitmfx <- p_value.poissonmfx

#' @export
p_value.negbinmfx <- p_value.poissonmfx



#' @rdname p_value.poissonmfx
#' @export
p_value.betaor <- function(model, component = c("all", "conditional", "precision"), ...) {
  component = match.arg(component)
  p_value.betareg(model$fit, component = component, ...)
}



#' @importFrom stats coef
#' @importFrom insight get_parameters
#' @rdname p_value.poissonmfx
#' @export
p_value.betamfx <- function(model, component = c("all", "conditional", "precision", "marginal"), ...) {
  parms <- insight::get_parameters(model, component = "all")
  cs <- do.call(rbind, stats::coef(summary(model$fit)))
  p <- c(as.vector(model$mfxest[, 4]), as.vector(cs[, 4]))

  out <- .data_frame(
    Parameter = parms$Parameter,
    p = p,
    Component = parms$Component
  )

  component <- match.arg(component)
  if (component != "all") {
    out <- out[out$Component == component, ]
  }

  out
}

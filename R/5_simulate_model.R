#' Simulated draws from model coefficients
#'
#' Simulate draws from a statistical model to return a data frame of estimates.
#'
#' @param model Statistical model (no Bayesian models).
#' @param component Should all parameters, parameters for the conditional model,
#'   or for the zero-inflated part of the model be returned? Applies to models
#'   with zero-inflated component. \code{component} may be one of \code{"conditional"},
#'   \code{"zi"}, \code{"zero-inflated"}, \code{"dispersion"} or \code{"all"}
#'   (default). May be abbreviated.
#' @inheritParams bootstrap_model
#' @inheritParams p_value
#'
#' @return A data frame.
#'
#' @seealso \code{\link[=simulate_parameters]{simulate_parameters()}},
#' \code{\link[=bootstrap_model]{bootstrap_model()}},
#' \code{\link[=bootstrap_parameters]{bootstrap_parameters()}}
#'
#' @details
#'   \subsection{Technical Details}{
#'     \code{simulate_model()} is a computationally faster alternative
#'     to \code{bootstrap_model()}. Simulated draws for coefficients are based
#'     on a multivariate normal distribution (\code{MASS::mvrnorm()}) with mean
#'     \code{mu = coef(model)} and variance \code{Sigma = vcov(model)}.
#'   }
#'   \subsection{Models with Zero-Inflation Component}{
#'     For models from packages \pkg{glmmTMB}, \pkg{pscl}, \pkg{GLMMadaptive} and
#'     \pkg{countreg}, the \code{component} argument can be used to specify
#'     which parameters should be simulated. For all other models, parameters
#'     from the conditional component (fixed effects) are simulated. This may
#'     include smooth terms, but not random effects.
#'   }
#'
#' @examples
#' library(parameters)
#' model <- lm(Sepal.Length ~ Species * Petal.Width + Petal.Length, data = iris)
#' head(simulate_model(model))
#' \donttest{
#' if (require("glmmTMB", quietly = TRUE)) {
#'   model <- glmmTMB(
#'     count ~ spp + mined + (1 | site),
#'     ziformula = ~mined,
#'     family = poisson(),
#'     data = Salamanders
#'   )
#'   head(simulate_model(model))
#'   head(simulate_model(model, component = "zero_inflated"))
#' }
#' }
#' @export
simulate_model <- function(model, iterations = 1000, ...) {
  UseMethod("simulate_model")
}



# Models with single component only -----------------------------------------


#' @export
simulate_model.default <- function(model, iterations = 1000, ...) {
  out <- .simulate_model(model, iterations, component = "conditional", effects = "fixed")

  class(out) <- c("parameters_simulate_model", class(out))
  attr(out, "object_name") <- .safe_deparse(substitute(model))
  out
}


#' @export
simulate_model.lm <- simulate_model.default

#' @export
simulate_model.glmmadmb <- simulate_model.default

#' @export
simulate_model.cglm <- simulate_model.default

#' @export
simulate_model.cpglm <- simulate_model.default

#' @export
simulate_model.cpglmm <- simulate_model.default

#' @export
simulate_model.feglm <- simulate_model.default

#' @export
simulate_model.fixest <- simulate_model.default

#' @export
simulate_model.iv_robust <- simulate_model.default

#' @export
simulate_model.rq <- simulate_model.default

#' @export
simulate_model.crq <- simulate_model.default

#' @export
simulate_model.nlrq <- simulate_model.default

#' @export
simulate_model.speedglm <- simulate_model.default

#' @export
simulate_model.speedlm <- simulate_model.default

#' @export
simulate_model.glm <- simulate_model.default

#' @export
simulate_model.glmRob <- simulate_model.default

#' @export
simulate_model.lmRob <- simulate_model.default

#' @export
simulate_model.gls <- simulate_model.default

#' @export
simulate_model.lme <- simulate_model.default

#' @export
simulate_model.crch <- simulate_model.default

#' @export
simulate_model.biglm <- simulate_model.default

#' @export
simulate_model.plm <- simulate_model.default

#' @export
simulate_model.flexsurvreg <- simulate_model.default

#' @export
simulate_model.LORgee <- simulate_model.default

#' @export
simulate_model.feis <- simulate_model.default

#' @export
simulate_model.lmrob <- simulate_model.default

#' @export
simulate_model.glmrob <- simulate_model.default

#' @export
simulate_model.merMod <- simulate_model.default

#' @export
simulate_model.gamlss <- simulate_model.default

#' @export
simulate_model.lm_robust <- simulate_model.default

#' @export
simulate_model.coxme <- simulate_model.default

#' @export
simulate_model.geeglm <- simulate_model.default

#' @export
simulate_model.gee <- simulate_model.default

#' @export
simulate_model.clm <- simulate_model.default

#' @export
simulate_model.polr <- simulate_model.default

#' @export
simulate_model.coxph <- simulate_model.default

#' @export
simulate_model.logistf <- simulate_model.default

#' @export
simulate_model.truncreg <- simulate_model.default

#' @export
simulate_model.glimML <- simulate_model.default

#' @export
simulate_model.lrm <- simulate_model.default

#' @export
simulate_model.psm <- simulate_model.default

#' @export
simulate_model.ols <- simulate_model.default

#' @export
simulate_model.rms <- simulate_model.default

#' @export
simulate_model.vglm <- simulate_model.default

#' @export
simulate_model.censReg <- simulate_model.default

#' @export
simulate_model.survreg <- simulate_model.default

#' @export
simulate_model.multinom <- simulate_model.default

#' @export
simulate_model.brmultinom <- simulate_model.default

#' @export
simulate_model.bracl <- simulate_model.default






# helper -----------------------------------------


.simulate_model <- function(model, iterations, component = "conditional", effects = "fixed") {
  insight::check_if_installed("MASS")
  if (is.null(iterations)) iterations <- 1000

  params <- insight::get_parameters(model, effects = effects, component = component, verbose = FALSE)
  beta <- stats::setNames(params$Estimate, params$Parameter) # Transform to named vector

  varcov <- insight::get_varcov(model, component = component, effects = effects)
  as.data.frame(MASS::mvrnorm(n = iterations, mu = beta, Sigma = varcov))

  ## Alternative approach, similar to arm::sim()

  # k <- length(insight::find_parameters(model, effects = "fixed", component = "conditional", flatten = TRUE))
  # n <- insight::n_obs(model)
  # beta.cov <- stats::vcov(model) / stats::sigma(model)
  # s <- vector("double", iterations)
  # b <- array(NA, c(100, k))
  # for (i in 1:iterations) {
  #   s[i] <- stats::sigma(model) * sqrt((n - k) / rchisq(1, n - k))
  #   b[i,] <- MASS::mvrnorm(n = 1, mu = beta, Sigma = beta.cov * s[i] ^ 2)
  # }
}

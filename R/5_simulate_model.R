#' @title Simulated draws from model coefficients
#' @name simulate_model
#'
#' @description Simulate draws from a statistical model to return a data frame
#' of estimates.
#'
#' @param model Statistical model (no Bayesian models).
#' @param component Should all parameters, parameters for the conditional model,
#'   for the zero-inflation part of the model, or the dispersion model be returned?
#'   Applies to models with zero-inflation and/or dispersion component. `component`
#'   may be one of `"conditional"`, `"zi"`, `"zero-inflated"`, `"dispersion"` or
#'    `"all"` (default). May be abbreviated.
#' @param ... Arguments passed to [`insight::get_varcov()`], e.g. to allow simulated
#' draws to be based on heteroscedasticity consistent variance covariance matrices.
#' @inheritParams bootstrap_model
#' @inheritParams p_value
#'
#' @inheritSection model_parameters.zcpglm Model components
#'
#' @return A data frame.
#'
#' @seealso [`simulate_parameters()`], [`bootstrap_model()`], [`bootstrap_parameters()`]
#'
#' @details
#' ## Technical Details
#' `simulate_model()` is a computationally faster alternative
#' to `bootstrap_model()`. Simulated draws for coefficients are based
#' on a multivariate normal distribution (`MASS::mvrnorm()`) with mean
#' `mu = coef(model)` and variance `Sigma = vcov(model)`.
#'
#' ## Models with Zero-Inflation Component
#' For models from packages **glmmTMB**, **pscl**, **GLMMadaptive** and
#' **countreg**, the `component` argument can be used to specify
#' which parameters should be simulated. For all other models, parameters
#' from the conditional component (fixed effects) are simulated. This may
#' include smooth terms, but not random effects.
#'
#' @examples
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

#' @rdname simulate_model
#' @export
simulate_model.default <- function(model, iterations = 1000, component = "all", ...) {
  # check for valid input
  .is_model_valid(model)

  out <- .simulate_model(model, iterations, component = "conditional", effects = "fixed", ...)

  class(out) <- c("parameters_simulate_model", class(out))
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
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
simulate_model.flic <- simulate_model.default

#' @export
simulate_model.flac <- simulate_model.default

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


.simulate_model <- function(model,
                            iterations,
                            component = "conditional",
                            effects = "fixed",
                            ...) {
  if (is.null(iterations)) iterations <- 1000

  params <- insight::get_parameters(model, effects = effects, component = component, verbose = FALSE)
  beta_mu <- stats::setNames(params$Estimate, params$Parameter) # Transform to named vector

  # "..." allow specification of vcov-args (#784)
  varcov <- insight::get_varcov(model, component = component, effects = effects, ...)
  as.data.frame(.mvrnorm(n = iterations, mu = beta_mu, Sigma = varcov))

  ## Alternative approach, similar to arm::sim()

  # k <- length(insight::find_parameters(model, effects = "fixed", component = "conditional", flatten = TRUE))
  # n <- insight::n_obs(model)
  # beta.cov <- stats::vcov(model) / stats::sigma(model)
  # s <- vector("double", iterations)
  # b <- array(NA, c(100, k))
  # for (i in 1:iterations) {
  #   s[i] <- stats::sigma(model) * sqrt((n - k) / rchisq(1, n - k))
  #   b[i,] <- .mvrnorm(n = 1, mu = beta_mu, Sigma = beta.cov * s[i] ^ 2)
  # }
}

.mvrnorm <- function(n = 1, mu, Sigma, tol = 1e-06) {
  p <- length(mu)
  if (!all(dim(Sigma) == c(p, p))) {
    insight::format_error(
      "Incompatible arguments to calculate multivariate normal distribution."
    )
  }

  eS <- eigen(Sigma, symmetric = TRUE)
  ev <- eS$values

  if (!all(ev >= -tol * abs(ev[1L]))) {
    insight::format_error("`Sigma` is not positive definite.")
  }

  X <- drop(mu) + eS$vectors %*% diag(sqrt(pmax(ev, 0)), p) %*% t(matrix(stats::rnorm(p * n), n))
  nm <- names(mu)

  dn <- dimnames(Sigma)
  if (is.null(nm) && !is.null(dn)) {
    nm <- dn[[1L]]
  }

  dimnames(X) <- list(nm, NULL)
  if (n == 1) {
    drop(X)
  } else {
    t(X)
  }
}

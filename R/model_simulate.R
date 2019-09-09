#' Simulated draws from model coefficients
#'
#' Simulate draws from a statistical model to return a data.frame of estimates.
#'
#' @param model Statistical model (no Bayesian models).
#' @param component Should all parameters, parameters for the conditional model,
#'   or for the zero-inflated part of the model be returned? Applies to models
#'   with zero-inflated component. \code{component} may be one of \code{"conditional"},
#'   \code{"zi"}, \code{"zero-inflated"} or \code{"all"} (default). May be abbreviated.
#' @inheritParams model_bootstrap
#'
#' @return A data frame.
#'
#' @seealso \code{\link[=parameters_simulate]{parameters_simulate()}},
#' \code{\link[=model_bootstrap]{model_bootstrap()}},
#' \code{\link[=parameters_bootstrap]{parameters_bootstrap()}}
#'
#' @details
#'   \subsection{Technical Details}{
#'     \code{model_simulate()} is a computationally faster alternative
#'     to \code{model_bootstrap()}. Simulated draws for coefficients are based
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
#' library(glmmTMB)
#'
#' model <- lm(Sepal.Length ~ Species * Petal.Width + Petal.Length, data = iris)
#' head(model_simulate(model))
#'
#' model <- glmmTMB(
#'   count ~ spp + mined + (1 | site),
#'   ziformula =  ~ mined,
#'   family = poisson(),
#'   data = Salamanders
#' )
#' head(model_simulate(model))
#' head(model_simulate(model, component = "zero_inflated"))
#' @export
model_simulate <- function(model, iterations = 1000, ...) {
  UseMethod("model_simulate")
}


# Models with single component only -----------------------------------------


#' @importFrom stats vcov setNames
#' @importFrom insight get_parameters
#' @export
model_simulate.lm <- function(model, iterations = 1000, ...) {
  .model_simulate(model, iterations, component = "conditional")
}


#' @export
model_simulate.glm <- model_simulate.lm


#' @export
model_simulate.gls <- model_simulate.lm


#' @export
model_simulate.lme <- model_simulate.lm


#' @export
model_simulate.plm <- model_simulate.lm


#' @export
model_simulate.feis <- model_simulate.lm


#' @export
model_simulate.merMod <- model_simulate.lm


#' @export
model_simulate.gamlss <- model_simulate.lm


#' @export
model_simulate.coxme <- model_simulate.lm


#' @export
model_simulate.geeglm <- model_simulate.lm


#' @export
model_simulate.gee <- model_simulate.lm


#' @export
model_simulate.clm <- model_simulate.lm


#' @export
model_simulate.clm2 <- model_simulate.lm


#' @export
model_simulate.polr <- model_simulate.lm


#' @export
model_simulate.coxph <- model_simulate.lm


#' @export
model_simulate.svyglm.nb <- model_simulate.lm


#' @export
model_simulate.svyglm.zip <- model_simulate.lm


#' @export
model_simulate.logistf <- model_simulate.lm


#' @export
model_simulate.betareg <- model_simulate.lm


#' @export
model_simulate.truncreg <- model_simulate.lm


#' @export
model_simulate.glimML <- model_simulate.lm


#' @export
model_simulate.ivreg <- model_simulate.lm


#' @export
model_simulate.lrm <- model_simulate.lm


#' @export
model_simulate.psm <- model_simulate.lm


#' @export
model_simulate.ols <- model_simulate.lm


#' @export
model_simulate.rms <- model_simulate.lm


#' @export
model_simulate.vglm <- model_simulate.lm


#' @export
model_simulate.censReg <- model_simulate.lm


#' @export
model_simulate.tobit <- model_simulate.lm


#' @export
model_simulate.survreg <- model_simulate.lm







# gam models  -----------------------------------------


#' @export
model_simulate.gam <- function(model, iterations = 1000, ...) {
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Package 'MASS' needed for this function to work. Please install it.", call. = FALSE)
  }

  if (is.null(iterations)) iterations <- 1000

  beta <- stats::coef(model)
  varcov <- .get_varcov(model, "all")
  as.data.frame(MASS::mvrnorm(n = iterations, mu = beta, Sigma = varcov))
}



#' @export
model_simulate.gamm <- function(model, iterations = 1000, ...) {
  model <- model$gam
  class(model) <- c("gam", "lm", "glm")
  model_simulate(model, iterations = iterations, ...)
}



#' @export
model_simulate.list <- function(model, iterations = 1000, ...) {
  if ("gam" %in% names(model)) {
    model <- model$gam
    class(model) <- c("gam", "lm", "glm")
    model_simulate(model, iterations = iterations, ...)
  }
}



#' @export
model_simulate.vgam <- function(model, iterations = 1000, ...) {
  .model_simulate(model, iterations, component = "all")
}







# Models with zero-inflation components ---------------------------------------


#' @importFrom stats vcov setNames
#' @importFrom insight get_parameters
#' @rdname model_simulate
#' @export
model_simulate.glmmTMB <- function(model, iterations = 1000, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)

  if (component %in% c("zi", "zero_inflated", "all") && !insight::model_info(model)$is_zero_inflated) {
    insight::print_color("Model has no zero-inflation component. Simulating from conditional parameters.\n", "red")
    component <- "conditional"
  }

  if (is.null(iterations)) iterations <- 1000

  if (component == "all") {
    d1 <- .model_simulate(model, iterations, component = "conditional")
    d2 <- .model_simulate(model, iterations, component = "zero_inflated")
    colnames(d2) <- paste0(colnames(d2), "_zi")
    d <- cbind(d1, d2)
  } else if (component == "conditional") {
    d <- .model_simulate(model, iterations, component = "conditional")
  } else {
    d <- .model_simulate(model, iterations, component = "zero_inflated")
  }

  d
}



#' @export
model_simulate.MixMod <- model_simulate.glmmTMB

#' @export
model_simulate.zeroinfl <- model_simulate.glmmTMB

#' @export
model_simulate.hurdle <- model_simulate.zeroinfl

#' @export
model_simulate.zerocount <- model_simulate.zeroinfl






# helper -----------------------------------------


.model_simulate <- function(model, iterations, component = "conditional") {
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Package 'MASS' needed for this function to work. Please install it.", call. = FALSE)
  }

  if (is.null(iterations)) iterations <- 1000

  parms <- insight::get_parameters(model, effects = "fixed", component = component)
  beta <- stats::setNames(parms$estimate, parms$parameter)

  varcov <- .get_varcov(model, component)
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



#' @importFrom insight find_parameters
#' @importFrom stats vcov
.get_varcov <- function(model, component) {
  if (inherits(model, c("hurdle", "zeroinfl", "zerocount"))) {
    vc <- switch(
      component,
      "conditional" = stats::vcov(object = model, model = "count"),
      "zero_inflated" = stats::vcov(object = model, model = "zero"),
      stats::vcov(object = model)
    )
  } else if (inherits(model, "MixMod")) {
    vc <- switch(
      component,
      "conditional" = stats::vcov(model, parm = "fixed-effects"),
      "zero_inflated" = stats::vcov(model, parm = "zero_part"),
      stats::vcov(model)
    )
  } else if (inherits(model, "feis")) {
    vc <- model$vcov
  } else if (inherits(model, "glimML")) {
    if (!requireNamespace("aod", quietly = TRUE)) {
      stop("Package 'aod' required for this function to work. Please install it.")
    }
    vc <- aod::vcov(model)
  } else if (inherits(model, c("vglm", "vgam"))) {
    if (!requireNamespace("VGAM", quietly = TRUE)) {
      stop("Package 'VGAM' required for this function to work. Please install it.")
    }
    vc <- VGAM::vcov(model)
  } else if (inherits(model, "tobit")) {
    coef_names <- insight::find_parameters(model, flatten = TRUE)
    vc <- stats::vcov(model)[coef_names, coef_names]
  } else if (inherits(model, "geeglm")) {
    vc <- summary(model)$cov.unscaled
  } else if (inherits(model, "gee")) {
    vc <- model$naive.variance
  } else {
    vc <- suppressWarnings(stats::vcov(model))
    if (is.list(vc)) {
      vc <- switch(
        component,
        "conditional" = vc[["cond"]],
        "zero_inflated" = vc[["zi"]],
        vc[[1]]
      )
    }
  }

  vc
}

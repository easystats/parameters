#' Simulated draws from model coefficients
#'
#' Simulate draws a statistical model n times to return a data.frame of estimates.
#'
#' @param model Statistical model.
#' @param n_sims The number of simulation draws to create.
#' @param component Should all parameters, parameters for the conditional model,
#'   or for the zero-inflated part of the model be returned? Applies to models
#'   with zero-inflated component. \code{component} may be one of \code{"conditional"},
#'   \code{"zi"}, \code{"zero-inflated"} or \code{"all"} (default). May be abbreviated.
#' @param ... Arguments passed to or from other methods.
#'
#' @return A data.frame.
#'
#' @seealso \code{\link[=parameters_simulate]{parameters_simulate()}},
#' \code{\link[=model_bootstrap]{model_bootstrap()}},
#' \code{\link[=parameters_bootstrap]{parameters_bootstrap()}}
#'
#' @details \code{model_simulate()} is a computationally faster alternative
#'   to \code{model_bootstrap()}. Simulated draws for coefficients are based
#'   on a multivariate normal distribution (\code{MASS::mvrnorm()} with mean
#'   \code{coef(model)} and variance \code{vcov(model)}.
#'
#' @examples
#' library(parameters)
#'
#' model <- lm(Sepal.Length ~ Species * Petal.Width + Petal.Length, data = iris)
#' head(model_simulate(model))
#' @export
model_simulate <- function(model, n_sims = 1000, ...) {
  UseMethod("model_simulate")
}



#' @importFrom stats vcov setNames
#' @importFrom insight get_parameters
#' @export
model_simulate.lm <- function(model, n_sims = 1000, ...) {
  .model_simulate(model, n_sims, component = "conditional")
}


#' @export
model_simulate.glm <- model_simulate.lm


#' @export
model_simulate.gls <- model_simulate.lm


#' @export
model_simulate.lme <- model_simulate.lm


#' @export
model_simulate.merMod <- model_simulate.lm


#' @export
model_simulate.gamlss <- model_simulate.lm


#' @export
model_simulate.gam <- function(model, n_sims = 1000, ...) {
  if (!requireNamespace("MASS", qquietly = TRUE)) {
    stop("Package 'MASS' needed for this function to work. Please install it.", call. = FALSE)
  }

  beta <- stats::coef(model)
  varcov <- .get_varcov(model, "all")
  as.data.frame(MASS::mvrnorm(n = n_sims, mu = beta, Sigma = varcov))
}



#' @export
model_simulate.gamm <- function(model, n_sims = 1000, ...) {
  model <- model$gam
  class(model) <- c("gam", "lm", "glm")
  model_simulate(model, n_sims = n_sims, ...)
}



#' @rdname model_simulate
#' @export
model_simulate.glmmTMB <- function(model, n_sims = 1000, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)

  if (component %in% c("zi", "zero_inflated", "all") && !insight::model_info(model)$is_zero_inflated) {
    insight::print_color("Model has no zero-inflation component. Simulating from conditional parameters.\n", "red")
    component <- "conditional"
  }

  if (component == "all") {
    d1 <- .model_simulate(model, n_sims, component = "conditional")
    d2 <- .model_simulate(model, n_sims, component = "zero_inflated")
    colnames(d2) <- paste0(colnames(d2), "_zi")
    d <- cbind(d1, d2)
  } else if (component == "conditional") {
    d <- .model_simulate(model, n_sims, component = "conditional")
  } else {
    d <- .model_simulate(model, n_sims, component = "zero_inflated")
  }

  d
}



#' @rdname model_simulate
#' @export
model_simulate.MixMod <- model_simulate.glmmTMB

#' @rdname model_simulate
#' @export
model_simulate.zeroinfl <- model_simulate.glmmTMB

#' @export
model_simulate.hurdle <- model_simulate.zeroinfl

#' @export
model_simulate.zerocount <- model_simulate.zeroinfl



.model_simulate <- function(model, n_sims, component = "conditional") {
  if (!requireNamespace("MASS", qquietly = TRUE)) {
    stop("Package 'MASS' needed for this function to work. Please install it.", call. = FALSE)
  }

  parms <- insight::get_parameters(model, effects = "fixed", component = component)
  beta <- stats::setNames(parms$estimate, parms$parameter)

  varcov <- .get_varcov(model, component)

  as.data.frame(MASS::mvrnorm(n = n_sims, mu = beta, Sigma = varcov))

  ## Alternative approach, similar to arm::sim()

  # k <- length(insight::find_parameters(model, effects = "fixed", component = "conditional", flatten = TRUE))
  # n <- insight::n_obs(model)
  # beta.cov <- stats::vcov(model) / stats::sigma(model)
  # s <- vector("double", n_sims)
  # b <- array(NA, c(100, k))
  # for (i in 1:n_sims) {
  #   s[i] <- stats::sigma(model) * sqrt((n - k) / rchisq(1, n - k))
  #   b[i,] <- MASS::mvrnorm(n = 1, mu = beta, Sigma = beta.cov * s[i] ^ 2)
  # }
}


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

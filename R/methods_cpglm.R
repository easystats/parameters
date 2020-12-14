
#' @export
model_parameters.bcplm <- function(model,
                                   centrality = "median",
                                   dispersion = FALSE,
                                   ci = .89,
                                   ci_method = "hdi",
                                   test = c("pd", "rope"),
                                   rope_range = "default",
                                   rope_ci = 1.0,
                                   bf_prior = NULL,
                                   diagnostic = c("ESS", "Rhat"),
                                   priors = TRUE,
                                   verbose = TRUE,
                                   ...) {


  # Processing
  params <- .extract_parameters_bayesian(
    model,
    centrality = centrality,
    dispersion = dispersion,
    ci = ci,
    ci_method = ci_method,
    test = test,
    rope_range = rope_range,
    rope_ci = rope_ci,
    bf_prior = bf_prior,
    diagnostic = diagnostic,
    priors = priors,
    ...
  )

  attr(params, "ci") <- ci
  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}


#' @importFrom utils capture.output
#' @export
standard_error.cpglm <- function(model, ...) {
  if (!requireNamespace("cplm", quietly = TRUE)) {
    stop("To use this function, please install package 'cplm'.")
  }

  junk <- utils::capture.output(stats <- cplm::summary(model)$coefficients)
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(stats[, "Std. Error"])
  )
}


#' @importFrom utils capture.output
#' @export
standard_error.zcpglm <- function(model, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  if (!requireNamespace("cplm", quietly = TRUE)) {
    stop("To use this function, please install package 'cplm'.")
  }

  component <- match.arg(component)
  junk <- utils::capture.output(stats <- cplm::summary(model)$coefficients)
  params <- insight::get_parameters(model)

  tweedie <- .data_frame(
    Parameter = params$Parameter[params$Component == "conditional"],
    SE = as.vector(stats$tweedie[, "Std. Error"]),
    Component = "conditional"
  )

  zero <- .data_frame(
    Parameter = params$Parameter[params$Component == "zero_inflated"],
    SE = as.vector(stats$zero[, "Std. Error"]),
    Component = "zero_inflated"
  )

  out <- .filter_component(rbind(tweedie, zero), component)
  out
}


#' @export
standard_error.cpglmm <- function(model, ...) {
  if (!requireNamespace("cplm", quietly = TRUE)) {
    stop("To use this function, please install package 'cplm'.")
  }

  stats <- cplm::summary(model)$coefs
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(stats[, "Std. Error"])
  )
}



#' @importFrom utils capture.output
#' @export
p_value.cpglm <- function(model, ...) {
  if (!requireNamespace("cplm", quietly = TRUE)) {
    stop("To use this function, please install package 'cplm'.")
  }

  junk <- utils::capture.output(stats <- cplm::summary(model)$coefficients)
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    p = as.vector(stats[, "Pr(>|t|)"])
  )
}


#' @include methods_pscl.R
#' @export
model_parameters.zcpglm <- model_parameters.zeroinfl


#' @include methods_ordinal.R
#' @export
model_parameters.cpglmm <- model_parameters.clmm

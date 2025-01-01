# model_parameters --------------------

#' @export
model_parameters.mmrm <- function(model,
                                  ci = 0.95,
                                  ci_method = NULL,
                                  bootstrap = FALSE,
                                  iterations = 1000,
                                  standardize = NULL,
                                  exponentiate = FALSE,
                                  p_adjust = NULL,
                                  summary = getOption("parameters_summary", FALSE),
                                  include_info = getOption("parameters_info", FALSE),
                                  keep = NULL,
                                  drop = NULL,
                                  verbose = TRUE,
                                  ...) {
  ci_method <- switch(model$method,
    Satterthwaite = "satterthwaite",
    "kenward"
  )

  ## TODO remove deprecated later
  if (!missing(summary)) {
    .deprecated_warning("summary", "include_info", verbose)
    include_info <- summary
  }

  # extract model parameters table, as data frame
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    ci_method = ci_method,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = "Parameter",
    standardize = standardize,
    exponentiate = exponentiate,
    p_adjust = p_adjust,
    include_info = include_info,
    keep_parameters = keep,
    drop_parameters = drop,
    vcov = NULL,
    vcov_args = NULL,
    verbose = verbose,
    ...
  )

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}

#' @export
model_parameters.mmrm_fit <- model_parameters.mmrm

#' @export
model_parameters.mmrm_tmb <- model_parameters.mmrm


# ci --------------------

#' @export
ci.mmrm <- function(x, ci = 0.95, method = "residual", ...) {
  .ci_generic(model = x, ci = ci, method = "residual", ...)
}

#' @export
ci.mmrm_fit <- ci.mmrm

#' @export
ci.mmrm_tmb <- ci.mmrm


# p --------------------

#' @export
p_value.mmrm <- function(model,
                         dof = NULL,
                         method = NULL,
                         component = "all",
                         vcov = NULL,
                         vcov_args = NULL,
                         verbose = TRUE,
                         ...) {
  p_value.default(
    model,
    dof = NULL,
    method = NULL,
    component = "all",
    vcov = NULL,
    vcov_args = NULL,
    verbose = verbose,
    ...
  )
}

#' @export
p_value.mmrm_fit <- p_value.mmrm

#' @export
p_value.mmrm_tmb <- p_value.mmrm


# SE --------------------

#' @export
standard_error.mmrm <- function(model, ...) {
  se <- .get_se_from_summary(model)
  .data_frame(Parameter = names(se), SE = as.vector(se))
}

#' @export
standard_error.mmrm_fit <- standard_error.mmrm

#' @export
standard_error.mmrm_tmb <- standard_error.mmrm

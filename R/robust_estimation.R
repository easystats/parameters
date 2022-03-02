# The functions in this file are kept for backward compatibility. They are
# superseded by the `vcov` and `vcov_args` arguments.


#' Robust standard errors. Superseded by the `vcov*` arguments in `standard_error()`
#'
#' @inheritParams standard_error
#' @export
standard_error_robust <- function(model,
                                  vcov = "HC3",
                                  vcov_args = NULL,
                                  component = "conditional",
                                  ...) {

  # exceptions
  if (inherits(model, "gee")) {
    return(standard_error(model, ...))
  }

  if (inherits(model, "MixMod")) {
    return(standard_error(model, ...))
  }

  standard_error(model,
                 vcov = vcov,
                 vcov_args = vcov_args,
                 ...)
}



#' Robust p values. Superseded by the `vcov*` arguments in `p_value()`
#'
#' @inheritParams p_value
#' @keywords internal
#' @export
p_value_robust <- function(model,
                           vcov = "HC",
                           vcov_args = NULL,
                           component = "conditional",
                           method = NULL,
                           ...) {

  # exceptions
  if (inherits(model, "gee")) {
    return(p_value(model, ...))
  }

  if (inherits(model, "MixMod")) {
    return(p_value(model, ...))
  }

  p_value(
    model,
    vcov = vcov,
    vcov_args = vcov_args,
    component = component,
    ...)
}


#' Robust confidence intervals. Superseded by the `vcov*` arguments in `ci()`
#'
#' @inheritParams ci.default
#' @inheritParams standard_error
#' @keywords internal
#' @export
ci_robust <- function(model,
                      ci = 0.95,
                      method = NULL,
                      vcov = "HC",
                      vcov_args = NULL,
                      component = "conditional",
                      ...) {
  out <- .ci_generic(
    model = model,
    ci = ci,
    method = method,
    component = component,
    vcov = vcov,
    vcov_args = vcov_args,
    ...
  )

  if ("Component" %in% colnames(out) && .n_unique(out$Component) == 1) {
    out$Component <- NULL
  }
  out
}

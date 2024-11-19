#' @title Confidence Intervals (CI)
#' @name ci.default
#'
#' @description `ci()` attempts to return confidence intervals of model parameters.
#'
#' @param x A statistical model.
#' @param ci Confidence Interval (CI) level. Default to `0.95` (`95%`).
#' @param dof Number of degrees of freedom to be used when calculating
#' confidence intervals. If `NULL` (default), the degrees of freedom are
#' retrieved by calling [`insight::get_df()`] with approximation method defined
#' in `method`. If not `NULL`, use this argument to override the default degrees
#' of freedom used to compute confidence intervals.
#' @param method Method for computing degrees of freedom for confidence
#' intervals (CI) and the related p-values. Allowed are following options (which
#' vary depending on the model class): `"residual"`, `"normal"`, `"likelihood"`,
#' `"satterthwaite"`, `"kenward"`, `"wald"`, `"profile"`, `"boot"`, `"uniroot"`,
#' `"ml1"`, `"betwithin"`, `"hdi"`, `"quantile"`, `"ci"`, `"eti"`, `"si"`,
#' `"bci"`, or `"bcai"`. See section _Confidence intervals and approximation of
#' degrees of freedom_ in [`model_parameters()`] for further details.
#' @param component Model component for which parameters should be shown. See
#' the documentation for your object's class in [`model_parameters()`] or
#' [`p_value()`] for further details, or see section _Model components_.
#' @param iterations The number of bootstrap replicates. Only applies to models
#' of class `merMod` when `method=boot`.
#' @param verbose Toggle warnings and messages.
#' @param ... Additional arguments passed down to the underlying functions.
#' E.g., arguments like `vcov` or `vcov_args` can be used to compute confidence
#' intervals using a specific variance-covariance matrix for the standard
#' errors.
#' @inheritParams standard_error
#'
#' @return A data frame containing the CI bounds.
#'
#' @inheritSection model_parameters Confidence intervals and approximation of degrees of freedom
#'
#' @inheritSection model_parameters.zcpglm Model components
#'
#' @examplesIf require("glmmTMB") && requireNamespace("sandwich")
#' data(qol_cancer)
#' model <- lm(QoL ~ time + age + education, data = qol_cancer)
#'
#' # regular confidence intervals
#' ci(model)
#'
#' # using heteroscedasticity-robust standard errors
#' ci(model, vcov = "HC3")
#'
#' \donttest{
#' library(parameters)
#' data(Salamanders, package = "glmmTMB")
#' model <- glmmTMB::glmmTMB(
#'   count ~ spp + mined + (1 | site),
#'   ziformula = ~mined,
#'   family = poisson(),
#'   data = Salamanders
#' )
#'
#' ci(model)
#' ci(model, component = "zi")
#' }
#' @export
ci.default <- function(x,
                       ci = 0.95,
                       dof = NULL,
                       method = NULL,
                       iterations = 500,
                       component = "all",
                       vcov = NULL,
                       vcov_args = NULL,
                       verbose = TRUE,
                       ...) {
  # check for valid input
  .is_model_valid(x)
  .ci_generic(
    model = x,
    ci = ci,
    dof = dof,
    method = method,
    component = component,
    vcov = vcov,
    vcov_args = vcov_args,
  ...)
}


#' @export
ci.glm <- function(x,
                   ci = 0.95,
                   dof = NULL,
                   method = "profile",
                   vcov = NULL,
                   vcov_args = NULL,
                   verbose = TRUE,
                   ...) {
  method <- insight::validate_argument(
    method,
    c("profile", "wald", "normal", "residual")
  )

  # No robust vcov for profile method
  if (method == "profile") {
    if ((!is.null(vcov) || !is.null(vcov_args)) && isTRUE(verbose)) {
      insight::format_alert(
        "The `vcov` and `vcov_args` are not available with `method=\"profile\"`."
      )
    }
    out <- lapply(ci, function(i) .ci_profiled(model = x, ci = i))
    out <- do.call(rbind, out)
  } else {
    out <- .ci_generic(
      model = x,
      ci = ci,
      dof = dof,
      method = method,
      vcov = vcov,
      vcov_args = vcov_args,
      verbose = verbose,
      ...
    )
  }

  # Return the CI bounds as a data frame.
  row.names(out) <- NULL
  out
}


# helper -----------------------------------------


#' @keywords internal
.check_component <- function(m, x, verbose = TRUE) {
  if (x %in% c("zi", "zero_inflated")) {
    minfo <- insight::model_info(m, verbose = FALSE)
    if (!isTRUE(minfo$is_zero_inflated)) {
      if (isTRUE(verbose)) {
        message("Model has no zero-inflation component!")
      }
      x <- NULL
    }
  }
  x
}

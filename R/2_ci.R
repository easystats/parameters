#' @title Confidence Intervals (CI)
#' @name ci.default
#'
#' @description Compute confidence intervals (CI) for frequentist models.
#'
#' @param x A statistical model.
#' @param ci Confidence Interval (CI) level. Default to `0.95` (`95%`).
#' @param dof Number of degrees of freedom to be used when calculating
#'   confidence intervals. If `NULL` (default), the degrees of freedom are
#'   retrieved by calling [`degrees_of_freedom()`][degrees_of_freedom] with
#'   approximation method defined in `method`. If not `NULL`, use this argument
#'   to override the default degrees of freedom used to compute confidence
#'   intervals.
#' @param method For mixed models, can be [`"wald"()`][p_value_wald]
#'   (default), [`"ml1"()`][p_value_ml1] or
#'   [`"betwithin"()`][p_value_betwithin]. For linear mixed model, can
#'   also be [`"satterthwaite"()`][p_value_satterthwaite],
#'   [`"kenward"()`][p_value_kenward] or `"boot"` (see
#'   `lme4::confint.merMod`). For (generalized) linear models, can be
#'   `"robust"` to compute confidence intervals based on robust covariance
#'   matrix estimation, and for generalized linear models and models from
#'   packages \pkg{lme4} or \pkg{glmmTMB}, may also be `"profile"`,
#'   `"uniroot"` or `"wald"` (default).
#' @param robust Logical, if `TRUE`, computes confidence intervals based on
#'   robust standard errors. See [`standard_error_robust()`][standard_error_robust].
#' @param ... Arguments passed down to [`standard_error_robust()`][standard_error_robust]
#'   when confidence intervals or p-values based on robust standard errors
#'   should be computed.
#'
#' @return A data frame containing the CI bounds.
#'
#' @note `ci_robust()` resp. `ci(robust = TRUE)` rely on the \pkg{sandwich}
#'   or \pkg{clubSandwich} package (the latter if `vcov_estimation = "CR"` for
#'   cluster-robust standard errors) and will thus only work for those models
#'   supported by those packages.
#'
#' @examples
#' \donttest{
#' library(parameters)
#' if (require("glmmTMB")) {
#'   model <- glmmTMB(
#'     count ~ spp + mined + (1 | site),
#'     ziformula = ~mined,
#'     family = poisson(),
#'     data = Salamanders
#'   )
#'
#'   ci(model)
#'   ci(model, component = "zi")
#' }
#' }
#' @export
ci.default <- function(x, ci = .95, dof = NULL, method = NULL, robust = FALSE, ...) {
  .ci_generic(model = x, ci = ci, dof = dof, method = method, robust = robust, ...)
}


#' @export
ci.glm <- function(x,
                   ci = .95,
                   dof = NULL,
                   method = "profile",
                   robust = FALSE,
                   ...) {

  method <- match.arg(method, choices = c("profile", "wald", "normal"))
  if (method == "profile") {
    out <- lapply(ci, function(i) .ci_profiled(model = x, ci = i))
    out <- do.call(rbind, out)
  } else {
    out <- .ci_generic(model = x, ci = ci, dof = dof, method = method, robust = robust, ...)
  }

  row.names(out) <- NULL
  out
}


# helper -----------------------------------------


#' @keywords internal
.check_component <- function(m, x, verbose = TRUE) {
  if (!insight::model_info(m)$is_zero_inflated && x %in% c("zi", "zero_inflated")) {
    if (isTRUE(verbose)) {
      message("Model has no zero-inflation component!")
    }
    x <- NULL
  }
  x
}

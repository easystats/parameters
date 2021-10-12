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
#' @param method Method for computing degrees of freedom for confidence
#'   intervals (CI) or p-values. May be one of `"analytical"`, `"any"`, `"ml1"`,
#'   `"betwithin"`, `"satterthwaite"`, `"kenward"`, `"wald"`, `"profile"`,
#'   `"boot"`, `"uniroot"`, `"residual"`, `"normal"`, or `"likelihood"`.
#'   If `bootstrap = TRUE`, may be one of `"hdi"`, `"quantile"`, `"ci"`, `"eti"`,
#'   `"si"`, `"bci"`, or `"bcai"`. See argument `ci_method` in
#'   [bayestestR::describe_posterior()].
#' @param robust Logical, if `TRUE`, computes confidence intervals based on
#'   robust standard errors. See [`standard_error_robust()`][standard_error_robust].
#' @param component TODO.
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

#' @export
standard_error.glmgee <- function(model,
                                  vcov = c("robust", "df-adjusted", "model", "bias-corrected", "jackknife"),
                                  verbose = TRUE,
                                  ...) {
  vcov <- match.arg(vcov)
  se <- NULL

  .vcov <- insight::get_varcov(
    model,
    vcov = vcov,
    verbose = verbose,
    ...
  )
  se <- sqrt(diag(.vcov))
  .data_frame(Parameter = names(se), SE = as.vector(se))
}


#' @export
p_value.glmgee <- function(model,
                           method = NULL,
                           vcov = c("robust", "df-adjusted", "model", "bias-corrected", "jackknife"),
                           ...) {
  vcov <- match.arg(vcov)
  est <- insight::get_parameters(model, component = "conditional")
  se <- standard_error(model, vcov = vcov, verbose = FALSE)

  if (is.null(method)) {
    method <- "wald"
  }

  p <- 2 * stats::pt(
    abs(est$Estimate / se$SE),
    df = insight::get_df(x = model, type = method),
    lower.tail = FALSE
  )

  .data_frame(
    Parameter = est$Parameter,
    p = as.vector(p)
  )
}


#' @export
ci.glmgee <- function(x,
                      ci = 0.95,
                      dof = NULL,
                      method = NULL,
                      vcov = c("robust", "df-adjusted", "model", "bias-corrected", "jackknife"),
                      verbose = TRUE,
                      ...) {
  vcov <- match.arg(vcov)
  out <- .ci_generic(
    model = x,
    ci = ci,
    dof = dof,
    method = method,
    vcov = vcov,
    vcov_args = NULL,
    component = "conditional",
    verbose = verbose
  )
  # Return the CI bounds as a data frame.
  row.names(out) <- NULL
  out
}

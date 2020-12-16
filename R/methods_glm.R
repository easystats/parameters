# classes: .glm


#################### .glm


#' @export
standard_error.glm <- function(model, method = NULL, ...) {
  robust <- !is.null(method) && method == "robust"

  if (isTRUE(robust)) {
    standard_error_robust(model, ...)
  } else {
    se <- .get_se_from_summary(model)
    .data_frame(
      Parameter = names(se),
      SE = as.vector(se)
    )
  }
}


#' @rdname ci.merMod
#' @method ci glm
#' @export
ci.glm <- function(x, ci = .95, method = c("profile", "wald", "robust"), ...) {
  method <- match.arg(method)
  if (method == "profile") {
    out <- lapply(ci, function(i) .ci_profiled(model = x, ci = i))
    out <- do.call(rbind, out)
  } else if (method == "robust") {
    out <- ci_wald(model = x, ci = ci, robust = TRUE, ...)
  } else {
    out <- ci_wald(model = x, ci = ci)
  }

  row.names(out) <- NULL
  out
}


#' @importFrom insight n_obs
#' @rdname model_parameters.default
#' @export
model_parameters.glm <- function(model,
                                 ci = .95,
                                 df_method = "profile",
                                 bootstrap = FALSE,
                                 iterations = 1000,
                                 standardize = NULL,
                                 exponentiate = FALSE,
                                 robust = FALSE,
                                 p_adjust = NULL,
                                 verbose = TRUE,
                                 ...) {
  if (insight::n_obs(model) > 1e4 && df_method == "profile") {
    message("Profiled confidence intervals may take longer time to compute. Use 'df_method=\"wald\"' for faster computation of CIs.")
  }

  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    df_method = df_method,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = "Parameter",
    standardize = standardize,
    exponentiate = exponentiate,
    robust = robust,
    p_adjust = p_adjust,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}

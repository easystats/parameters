# ci -----------------

#' @export
ci.phylolm <- function(x, ci = 0.95, dof = NULL, method = "wald", verbose = TRUE, ...) {
  method <- match.arg(method, choices = c("wald", "residual", "normal", "boot"))

  if (method == "boot" && (is.null(x$boot) || x$boot <= 0)) {
    insight::format_warning(
      "Bootstrapped confidence intervals are not available",
      "Try re-fitting your model, using `boot = <n>`, where `n` is the number of bootstrap replicates."
    )
    method <- "wald"
  }

  if (method == "boot") {
    s <- stats::coef(summary(x))
    out <- .data_frame(
      Parameter = row.names(s),
      CI_low = as.vector(s[, "lowerbootCI"]),
      CI_high = as.vector(s[, "upperbootCI"])
    )
  } else {
    out <- ci.default(x = x, ci = ci, dof = dof, method = method, verbose = verbose, ...)
  }

  row.names(out) <- NULL
  out
}

#' @export
ci.phyloglm <- ci.phylolm

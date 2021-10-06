#' @rdname ci.merMod
#' @export
ci.default <- function(x, ci = .95, dof = Inf, method = NULL, robust = FALSE, ...) {
  if (!is.null(method)) {
    method <- tolower(method)
  } else {
    method <- "wald"
  }

  if (isTRUE(robust)) {
    ci_wald(model = x, ci = ci, dof = dof, method = method, robust = TRUE, ...)
  } else if (method == "ml1") {
    ci_ml1(model = x, ci = ci)
  } else if (method == "betwithin") {
    ci_betwithin(model = x, ci = ci)
  } else if (method == "normal") {
    ci_wald(model = x, ci = ci, dof = Inf, method = "normal", robust = FALSE, ...)
  } else {
    ci_wald(model = x, ci = ci, dof = NULL, method = method, robust = FALSE, ...)
  }
}


#' @rdname ci.merMod
#' @export
ci.glm <- function(x, ci = .95, method = c("profile", "wald"), robust = FALSE, ...) {
  method <- match.arg(method)
  if (method == "profile") {
    out <- lapply(ci, function(i) .ci_profiled(model = x, ci = i))
    out <- do.call(rbind, out)
  } else {
    out <- ci_wald(model = x, ci = ci, robust = robust, ...)
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

#' @rdname ci.merMod
#' @export
ci.default <- function(x, ci = .95, method = NULL, ...) {
  if (!is.null(method)) {
    method <- tolower(method)
  } else {
    method <- "wald"
  }

  if (method == "robust") {
    ci_wald(model = x, ci = ci, dof = Inf, robust = TRUE)
  } else if (method == "ml1") {
    ci_ml1(model = x, ci = ci)
  } else if (method == "betwithin") {
    ci_betwithin(model = x, ci = ci)
  } else {
    ci_wald(model = x, ci = ci, dof = Inf, robust = FALSE)
  }
}


#' @rdname ci.merMod
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


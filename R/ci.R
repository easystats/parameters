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


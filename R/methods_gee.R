#' @export
ci.gee <- ci.tobit


#' @export
ci.geeglm <- ci.tobit


#' @export
ci.survreg <- ci.tobit


#' @export
standard_error.geeglm <- standard_error.default


#' @export
standard_error.gee <- function(model, method = NULL, ...) {
  cs <- stats::coef(summary(model))
  robust <- !is.null(method) && method == "robust"

  if (isTRUE(robust)) {
    se <- as.vector(cs[, "Robust S.E."])
  } else {
    se <- as.vector(cs[, "Naive S.E."])
  }

  .data_frame(Parameter = .remove_backticks_from_string(rownames(cs)), SE = se)
}


#' @export
p_value.gee <- function(model, method = NULL, ...) {
  cs <- stats::coef(summary(model))

  if (!is.null(method) && method == "robust") {
    p <- 2 * stats::pt(abs(cs[, "Estimate"] / cs[, "Robust S.E."]), df = degrees_of_freedom(model, method = "any"), lower.tail = FALSE)
  } else {
    p <- 2 * stats::pt(abs(cs[, "Estimate"] / cs[, "Naive S.E."]), df = degrees_of_freedom(model, method = "any"), lower.tail = FALSE)
  }

  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(cs)),
    p = as.vector(p)
  )
}


#' @export
p_value.geeglm <- p_value.default

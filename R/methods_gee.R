
#' @export
standard_error.geeglm <- standard_error.default


#' @export
standard_error.gee <- function(model, method = NULL, ...) {
  cs <- stats::coef(summary(model))

  if (isTRUE(list(...)$robust) || "vcov" %in% names(list(...))) {
    se <- as.vector(cs[, "Robust S.E."])
  } else {
    se <- as.vector(cs[, "Naive S.E."])
  }

  .data_frame(Parameter = .remove_backticks_from_string(rownames(cs)), SE = se)
}


#' @export
p_value.gee <- function(model, method = NULL, ...) {
  cs <- stats::coef(summary(model))
  if (is.null(method)) {
    method <- "any"
  }

  if (isTRUE(list(...)$robust) || "vcov" %in% names(list(...))) {
    p <- 2 * stats::pt(abs(cs[, "Estimate"] / cs[, "Robust S.E."]), df = degrees_of_freedom(model, method = method), lower.tail = FALSE)
  } else {
    p <- 2 * stats::pt(abs(cs[, "Estimate"] / cs[, "Naive S.E."]), df = degrees_of_freedom(model, method = method), lower.tail = FALSE)
  }

  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(cs)),
    p = as.vector(p)
  )
}


#' @export
ci.geeglm <- function(x, ci = .95, method = "wald", ...) {
  .ci_generic(x, ci = ci, method = method, ...)
}


#' @export
p_value.geeglm <- function(model, method = "wald", ...) {
  stat <- insight::get_statistic(model)

  if (!is.null(stat)) {
    if (identical(method, "residual")) {
      dof <- degrees_of_freedom(model, method = "residual")
      p <- as.vector(2 * stats::pt(sqrt(abs(stat$Statistic)), df = dof, lower.tail = FALSE))
    } else {
      p <- as.vector(1 - stats::pchisq(stat$Statistic, df = 1))
    }
    .data_frame(
      Parameter = stat$Parameter,
      p = p
    )
  }
}

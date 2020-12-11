
#' @export
ci.lrm <- ci.gamlss


#' @export
ci.psm <- ci.gamlss


#' @export
ci.ols <- ci.gamlss


#' @export
ci.rms <- ci.gamlss


#' @export
standard_error.lrm <- function(model, ...) {
  se <- sqrt(diag(stats::vcov(model)))

  # psm-models returns vcov-matrix w/o dimnames
  if (is.null(names(se))) names(se) <- names(stats::coef(model))

  .data_frame(
    Parameter = .remove_backticks_from_string(names(se)),
    SE = as.vector(se)
  )
}


#' @export
standard_error.ols <- standard_error.lrm


#' @export
standard_error.rms <- standard_error.lrm


#' @export
standard_error.psm <- standard_error.lrm


#' @export
p_value.lrm <- function(model, ...) {
  stat <- insight::get_statistic(model)
  p <- 2 * stats::pt(abs(stat$Statistic), df = degrees_of_freedom(model, method = "any"), lower.tail = FALSE)

  .data_frame(
    Parameter = .remove_backticks_from_string(stat$Parameter),
    p = as.vector(p)
  )
}


#' @export
p_value.ols <- p_value.lrm


#' @export
p_value.rms <- p_value.lrm


#' @export
p_value.psm <- p_value.lrm


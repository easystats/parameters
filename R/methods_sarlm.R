#' @export
p_value.Sarlm <- function(model, ...) {
  stat <- insight::get_statistic(model)
  .data_frame(
    Parameter = stat$Parameter,
    p = 2 * stats::pnorm(abs(stat$Statistic), lower.tail = FALSE)
  )
}


#' @export
ci.Sarlm <- function(x, ci = 0.95, ...) {
  .ci_generic(model = x, ci = ci, ...)
}


#' @export
standard_error.Sarlm <- function(model, ...) {
  params <- insight::get_parameters(model)
  s <- summary(model)
  # add rho, if present
  if (!is.null(s$rho)) {
    rho <- as.numeric(s$rho.se)
  } else {
    rho <- NULL
  }
  .data_frame(
    Parameter = params$Parameter,
    SE = c(rho, as.vector(s$Coef[, 2]))
  )
}

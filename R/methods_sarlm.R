#' @importFrom insight get_statistic
#' @importFrom stats pnorm
#' @export
p_value.sarlm <- function(model, ...) {
  stat <- insight::get_statistic(model)
  .data_frame(
    Parameter = stat$Parameter,
    p = 2 * stats::pnorm(abs(stat$Statistic), lower.tail = FALSE)
  )
}


#' @export
ci.sarlm <- function(x, ci = .95, ...) {
  ci_wald(model = x, ci = ci, ...)
}


#' @export
standard_error.sarlm <- function(model, ...) {
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

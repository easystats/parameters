#' @export
standard_error.coxme <- function(model, ...) {
  beta_coef <- model$coefficients

  if (length(beta_coef) > 0) {
    .data_frame(
      Parameter = .remove_backticks_from_string(names(beta_coef)),
      SE = sqrt(diag(stats::vcov(model)))
    )
  }
}

## TODO add ci_method later?

#' @export
p_value.coxme <- function(model, ...) {
  stat <- insight::get_statistic(model)

  if (!is.null(stat)) {
    .data_frame(
      Parameter = stat$Parameter,
      p = as.vector(1 - stats::pchisq(stat$Statistic^2, df = 1))
    )
  }
}

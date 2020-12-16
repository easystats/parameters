
#' @export
ci.coxme <- ci.tobit


#' @export
standard_error.coxme <- function(model, ...) {
  beta <- model$coefficients

  if (length(beta) > 0) {
    .data_frame(
      Parameter = .remove_backticks_from_string(names(beta)),
      SE = sqrt(diag(stats::vcov(model)))
    )
  }
}


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



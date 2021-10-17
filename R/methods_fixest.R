# .fixest -----------------------

#' @export
standard_error.fixest <- function(model, ...) {
  stats <- summary(model)
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(stats$se)
  )
}

## TODO add ci_method later?

#' @export
p_value.fixest <- function(model, ...) {
  stats <- summary(model)$coeftable
  params <- insight::get_parameters(model)
  stat_col <- which(colnames(stats) %in% c("Pr(>|t|)", "Pr(>|z|)"))

  .data_frame(
    Parameter = params$Parameter,
    p = as.vector(stats[[stat_col]])
  )
}




# .feglm -----------------------

#' @export
standard_error.feglm <- function(model, ...) {
  stats <- stats::coef(summary(model))
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(stats[, "Std. error"])
  )
}

## TODO add ci_method later?

#' @export
p_value.feglm <- function(model, ...) {
  stats <- stats::coef(summary(model))
  params <- insight::get_parameters(model)
  .data_frame(
    Parameter = params$Parameter,
    p = as.vector(stats[, 4])
  )
}

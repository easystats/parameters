#' @export
ci.BBmm <- ci.default


#' @export
ci.BBreg <- ci.default


#' @export
standard_error.BBmm <- function(model, ...) {
  .data_frame(
    Parameter = insight::find_parameters(model,
      effects = "fixed",
      component = "conditional",
      flatten = TRUE
    ),
    SE = as.data.frame(summary(model)$fixed.coefficients)$StdErr
  )
}


#' @export
standard_error.BBreg <- function(model, ...) {
  .data_frame(
    Parameter = insight::find_parameters(model,
      effects = "fixed",
      component = "conditional",
      flatten = TRUE
    ),
    SE = as.data.frame(summary(model)$coefficients)$StdErr
  )
}


## TODO add ci_method later?
## TODO BBmm only has p based on normal distribution assumptions?


#' @export
p_value.BBmm <- function(model, ...) {
  .data_frame(
    Parameter = insight::find_parameters(model,
      effects = "fixed",
      component = "conditional",
      flatten = TRUE
    ),
    p = as.data.frame(summary(model)$fixed.coefficients)$p.value
  )
}


## TODO add ci_method later?
## TODO BBreg only has p based on normal distribution assumptions?


#' @export
p_value.BBreg <- function(model, ...) {
  .data_frame(
    Parameter = insight::find_parameters(model,
      effects = "fixed",
      component = "conditional",
      flatten = TRUE
    ),
    p = as.data.frame(summary(model)$coefficients)$p.value
  )
}

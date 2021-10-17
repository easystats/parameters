
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

## TODO BBreg only ha p based on normal distribution assumptions?


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



#' @export
degrees_of_freedom.BBmm <- function(model, method = "residual", ...) {
  if (method %in% c("residual", "wald")) {
    return(model$df)
  } else {
    return(degrees_of_freedom.default(model = model, method = method, ...))
  }
}


#' @export
degrees_of_freedom.BBreg <- degrees_of_freedom.BBmm

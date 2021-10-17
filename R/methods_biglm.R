
#' @export
standard_error.biglm <- function(model, ...) {
  cs <- summary(model)$mat
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(cs[, 4])
  )
}


#' @export
degrees_of_freedom.biglm <- function(model, method = NULL, ...) {
  if (identical(method, "normal")) {
    Inf
  } else {
    .degrees_of_freedom_analytical(model, kenward = FALSE)
  }
}

#' @export
degrees_of_freedom.bigglm <- degrees_of_freedom.biglm

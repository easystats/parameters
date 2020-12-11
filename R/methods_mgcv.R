
#' @export
ci.gamm <- function(x, ci = .95, ...) {
  x <- x$gam
  class(x) <- c("gam", "lm", "glm")
  ci(x, ci = ci, ...)
}


#' @export
standard_error.gamm <- function(model, ...) {
  model <- model$gam
  class(model) <- c("gam", "lm", "glm")
  standard_error(model)
}


#' @export
p_value.gamm <- function(model, ...) {
  model <- model$gam
  class(model) <- c("gam", "lm", "glm")
  p_value(model)
}


#' @export
model_parameters.gamm <- function(model,
                                  ci = .95,
                                  bootstrap = FALSE,
                                  iterations = 1000,
                                  verbose = TRUE,
                                  ...) {
  model <- model$gam
  class(model) <- c("gam", "lm", "glm")
  model_parameters(
    model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    robust = FALSE,
    ...
  )
}




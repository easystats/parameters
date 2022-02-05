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
    ...
  )
}


#' @export
ci.gamm <- ci.gamm4


#' @export
standard_error.gamm <- standard_error.gamm4


#' @export
p_value.gamm <- p_value.gamm4


#' @export
simulate_model.gamm <- function(model, iterations = 1000, ...) {
  model <- model$gam
  class(model) <- c("gam", "lm", "glm")
  simulate_model(model, iterations = iterations, ...)
}

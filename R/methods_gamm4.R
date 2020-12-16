
#' @export
ci.gamm4 <- function(x, ci = .95, ...) {
  x <- x$gam
  class(x) <- c("gam", "lm", "glm")
  ci(x, ci = ci, ...)
}


#' @export
standard_error.gamm4 <- function(model, ...) {
  model <- model$gam
  class(model) <- c("gam", "lm", "glm")
  standard_error(model)
}


#' @export
p_value.gamm4 <- function(model, ...) {
  model <- model$gam
  class(model) <- c("gam", "lm", "glm")
  p_value(model)
}

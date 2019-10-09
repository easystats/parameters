#' Correlations and t-test Parameters
#'
#' Parameters of h-tests (correlations, t-tests).
#'
#' @param model Object of class \code{htest}.
#' @param bootstrap Should estimates be bootstrapped?
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' model <- cor.test(mtcars$mpg, mtcars$cyl, method = "pearson")
#' model_parameters(model)
#'
#' model <- t.test(iris$Sepal.Width, iris$Sepal.Length)
#' model_parameters(model)
#'
#' model <- t.test(mtcars$mpg ~ mtcars$vs)
#' model_parameters(model)
#'
#' model <- t.test(iris$Sepal.Width, mu = 1)
#' model_parameters(model)
#' @return A data.frame of indices related to the model's parameters.
#' @export
model_parameters.htest <- function(model, bootstrap = FALSE, ...) {
  if (bootstrap) {
    stop("Bootstrapped h-tests are not yet implemented.")
  } else {
    parameters <- .extract_parameters_htest(model)
  }

  attr(parameters, "ci") <- attributes(model$conf.int)$conf.level
  class(parameters) <- c("parameters_model", class(parameters))
  parameters
}

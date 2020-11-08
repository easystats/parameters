#' Parameters from Correlations and t-tests
#'
#' Parameters of h-tests (correlations, t-tests).
#'
#' @param model Object of class \code{htest} or \code{pairwise.htest}.
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
#'
#' data(airquality)
#' airquality$Month <- factor(airquality$Month, labels = month.abb[5:9])
#' model <- pairwise.t.test(airquality$Ozone, airquality$Month)
#' model_parameters(model)
#'
#' smokers <- c(83, 90, 129, 70)
#' patients <- c(86, 93, 136, 82)
#' model <- pairwise.prop.test(smokers, patients)
#' model_parameters(model)
#' @return A data frame of indices related to the model's parameters.
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

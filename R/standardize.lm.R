#' Model Standardization
#'
#' Standardize the model's parameters.
#'
#' @inheritParams standardize
#' @param method The method used for standardizing the parameters. Can be "refit" (default).
#'
#' @details Methods:
#' \itemize{
#'  \item \emph{refit}: This method is based on a complete model re-fit using the standardized data. It is the most accurate, especially for parameters related to interactions, but it is also the most computationnaly costly.
#'  }
#'
#' @examples
#' model <- lm(Sepal.Length ~ Species * Petal.Width, data = iris)
#' coef(standardize(model))
#' @importFrom stats update
#' @importFrom insight get_data
#' @export
standardize.lm <- function(x, robust = FALSE, method = "refit", ...) {
  data <- insight::get_data(x)
  model_std <- update(x, data = standardize(data, robust = robust))
  return(model_std)
}

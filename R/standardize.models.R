#' Model Standardization
#'
#' Refit the model after standardizing the data.
#'
#' @inheritParams standardize
#'
#'
#' @examples
#' model <- lm(Sepal.Length ~ Species * Petal.Width, data = iris)
#' coef(standardize(model))
#' @importFrom stats update
#' @importFrom insight get_data model_info find_response
#' @importFrom utils capture.output
#' @export
standardize.lm <- function(x, robust = FALSE, ...) {
  data <- insight::get_data(x)
  if (insight::model_info(x)$is_binomial) {
    data[insight::find_response(x)] <- as.factor(insight::get_response(x))
  }
  if (inherits(x, c("brmsfit"))) {
    text <- capture.output(model_std <- update(x, newdata = standardize(data, robust = robust)))
  } else {
    text <- capture.output(model_std <- update(x, data = standardize(data, robust = robust)))
  }
  return(model_std)
}

#' @export
standardize.merMod <- standardize.lm

#' @export
standardize.stanreg <- standardize.lm

#' @export
standardize.brmsfit <- standardize.lm




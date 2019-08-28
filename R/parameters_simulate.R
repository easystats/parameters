#' Parameters simulation
#'
#' Compute simulated draws of parameters and their related indices such as Confidence Intervals (CI) and p-values.
#'
#' @inheritParams model_simulate
#' @inheritParams bayestestR::describe_posterior
#'
#' @details \code{parameters_simulate()} is a computationally faster alternative
#'   to \code{parameters_bootstrap()}. Simulated draws for coefficients are based
#'   on a multivariate normal distribution (\code{MASS::mvrnorm()} with mean
#'   \code{coef(model)} and variance \code{vcov(model)}.
#'
#' @return A data frame with simulated parameters.
#'
#' @references Gelman A, Hill J. Data analysis using regression and multilevel/hierarchical models. Cambridge; New York: Cambridge University Press 2007: 140-143
#'
#' @seealso \code{\link{model_bootstrap}}, \code{\link{parameters_bootstrap}}, \code{\link{model_simulate}}
#'
#' @examples
#' library(parameters)
#'
#' model <- lm(Sepal.Length ~ Species * Petal.Width + Petal.Length, data = iris)
#' parameters_simulate(model)
#' @importFrom bayestestR describe_posterior
#' @importFrom tools toTitleCase
#' @export
parameters_simulate <- function(model, n_sims = 1000, centrality = "median", ci = .95, ci_method = "quantile", test = "p-value", ...) {
  data <- model_simulate(model, n_sims = n_sims, ...)
  .summary_bootstrap(data = data, test = test, centrality = centrality, ci_method = ci_method, ...)
}

#' Parameters simulation
#'
#' Compute simulated draws of parameters and their related indices such as Confidence Intervals (CI) and p-values.
#'
#' @inheritParams model_simulate
#' @inheritParams bayestestR::describe_posterior
#'
#' @return A data frame with simulated parameters.
#'
#' @references Gelman A, Hill J. Data analysis using regression and multilevel/hierarchical models. Cambridge; New York: Cambridge University Press 2007: 140-143
#'
#' @seealso \code{\link{model_bootstrap}}, \code{\link{parameters_bootstrap}}, \code{\link{model_simulate}}
#'
#' @details
#'   \subsection{Technical Details}{
#'     \code{model_simulate()} is a computationally faster alternative
#'     to \code{model_bootstrap()}. Simulated draws for coefficients are based
#'     on a multivariate normal distribution (\code{MASS::mvrnorm()}) with mean
#'     \code{mu = coef(model)} and variance \code{Sigma = vcov(model)}.
#'   }
#'   \subsection{Models with Zero-Inflation Component}{
#'     For models from packages \pkg{glmmTMB}, \pkg{pscl}, \pkg{GLMMadaptive} and
#'     \pkg{countreg}, the \code{component} argument can be used to specify
#'     which parameters should be simulated. For all other models, parameters
#'     from the conditional component (fixed effects) are simulated. This may
#'     include smooth terms, but not random effects.
#'   }
#'
#' @examples
#' library(parameters)
#' library(glmmTMB)
#'
#' model <- lm(Sepal.Length ~ Species * Petal.Width + Petal.Length, data = iris)
#' parameters_simulate
#'
#' model <- glmmTMB(
#'   count ~ spp + mined + (1 | site),
#'   ziformula =  ~ mined,
#'   family = poisson(),
#'   data = Salamanders
#' )
#' parameters_simulate(model, centrality = "mean")
#' parameters_simulate(model, ci = c(.8, .95), component = "zero_inflated")
#' @importFrom bayestestR describe_posterior
#' @importFrom tools toTitleCase
#' @export
parameters_simulate <- function(model, n_sims = 1000, centrality = "median", ci = .95, ci_method = "quantile", test = "p-value", ...) {
  data <- model_simulate(model, n_sims = n_sims, ...)
  .summary_bootstrap(data = data, test = test, centrality = centrality, ci = ci, ci_method = ci_method, ...)
}

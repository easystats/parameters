#' Parameters simulation
#'
#' Compute simulated draws of parameters and their related indices such as Confidence Intervals (CI) and p-values. Simulating parameter draws can be seen as a (computationally faster) alternative to bootstrapping.
#'
#' @inheritParams simulate_model
#' @inheritParams bayestestR::describe_posterior
#'
#' @return A data frame with simulated parameters.
#'
#' @references Gelman A, Hill J. Data analysis using regression and multilevel/hierarchical models. Cambridge; New York: Cambridge University Press 2007: 140-143
#'
#' @seealso \code{\link{bootstrap_model}}, \code{\link{bootstrap_parameters}}, \code{\link{simulate_model}}
#'
#' @details
#'   \subsection{Technical Details}{
#'     \code{simulate_model()} is a computationally faster alternative
#'     to \code{bootstrap_model()}. Simulated draws for coefficients are based
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
#' simulate_parameters(model)
#'
#' model <- glmmTMB(
#'   count ~ spp + mined + (1 | site),
#'   ziformula = ~mined,
#'   family = poisson(),
#'   data = Salamanders
#' )
#' simulate_parameters(model, centrality = "mean")
#' simulate_parameters(model, ci = c(.8, .95), component = "zero_inflated")
#' @importFrom bayestestR describe_posterior
#' @importFrom tools toTitleCase
#' @export
simulate_parameters <- function(model, ...) {
  UseMethod("simulate_parameters")
}

#' @rdname simulate_parameters
#' @export
parameters_simulate <- simulate_parameters




#' @rdname simulate_parameters
#' @export
simulate_parameters.default <- function(model, iterations = 1000, centrality = "median", ci = .95, ci_method = "quantile", test = "p-value", ...) {
  data <- simulate_model(model, iterations = iterations, ...)
  out <- .summary_bootstrap(data = data, test = test, centrality = centrality, ci = ci, ci_method = ci_method, ...)

  params <- insight::get_parameters(model)
  if ("Effects" %in% colnames(params) && length(unique(params$Effects)) > 1) {
    out$Effects <- params$Effects
  }

  class(out) <- c("parameters_simulate", "see_parameters_simulate", class(out))
  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  attr(out, "iterations") <- iterations

  out
}



#' @export
simulate_parameters.multinom <- function(model, iterations = 1000, centrality = "median", ci = .95, ci_method = "quantile", test = "p-value", ...) {
  data <- simulate_model(model, iterations = iterations, ...)
  out <- .summary_bootstrap(data = data, test = test, centrality = centrality, ci = ci, ci_method = ci_method, ...)

  params <- insight::get_parameters(model)
  out$Parameter <- params$Parameter
  if ("Response" %in% colnames(params)) {
    out$Response <- params$Response
  }

  class(out) <- c("parameters_simulate", "see_parameters_simulate", class(out))
  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  attr(out, "iterations") <- iterations

  out
}

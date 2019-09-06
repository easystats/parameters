#' Parameters bootstrapping
#'
#' Compute bootstrapped parameters and their related indices such as Confidence Intervals (CI) and p-values.
#'
#'
#' @inheritParams model_bootstrap
#' @inheritParams bayestestR::describe_posterior
#'
#' @return Bootstrapped parameters.
#'
#' @references Davison, A. C., & Hinkley, D. V. (1997). Bootstrap methods and their application (Vol. 1). Cambridge university press.
#'
#' @seealso \code{\link{model_bootstrap}}, \code{\link{parameters_simulate}}, \code{\link{model_simulate}}
#'
#' @examples
#' library(parameters)
#'
#' model <- lm(Sepal.Length ~ Species * Petal.Width, data = iris)
#' parameters_bootstrap(model)
#' @importFrom tools toTitleCase
#' @export
parameters_bootstrap <- function(model, iterations = 1000, centrality = "median", ci = .95, ci_method = "quantile", test = "p-value", ...) {
  data <- model_bootstrap(model, iterations = iterations, ...)
  .summary_bootstrap(data = data, test = test, centrality = centrality, ci = ci, ci_method = ci_method, ...)
}



.summary_bootstrap <- function(data, test, centrality, ci, ci_method, ...) {
  # Is the p-value requested?
  if (any(test %in% c("p-value", "p", "pval"))) {
    p_value <- TRUE
    test <- setdiff(test, c("p-value", "p", "pval"))
    if (length(test) == 0) test <- NULL
  } else {
    p_value <- FALSE
  }

  parameters <- bayestestR::describe_posterior(data, centrality = centrality, ci = ci, ci_method = ci_method, test = test, ...)

  # Remove unecessary columns
  if ("CI" %in% names(parameters) && length(unique(parameters$CI)) == 1) {
    parameters$CI <- NULL
  }

  # Coef
  if (length(c(centrality)) == 1) {
    names(parameters)[names(parameters) == tools::toTitleCase(centrality)] <- "Coefficient"
  }

  # p-value
  if (p_value) {
    parameters$.col_order <- 1:nrow(parameters)
    p <- p_value(data, ...)
    parameters <- merge(parameters, p, all = TRUE)
    parameters <- parameters[parameters$.col_order, ]
    parameters$.col_order <- NULL
  }

  parameters
}
#' Parameters bootstrapping
#'
#' Compute bootstrapped parameters and their related indices such as Confidence Interval (CI) and p-value.
#'
#'
#' @inheritParams model_bootstrap
#' @inheritParams bayestestR::describe_posterior
#'
#' @examples
#' model <- lm(Sepal.Length ~ Species * Petal.Width, data = iris)
#' parameters_bootstrap(model, test = c("p", "pd"))
#'
#' @references Davison, A. C., & Hinkley, D. V. (1997). Bootstrap methods and their application (Vol. 1). Cambridge university press.
#'
#' @importFrom tools toTitleCase
#' @export
parameters_bootstrap <- function(model, iterations = 1000, centrality = "median", ci = .95, ci_method = "quantile", test = "p-value", ...){
  data <- model_bootstrap(model, iterations = iterations, ...)

  # Is the p-value requested?
  if("p-value" %in% c(test) | "p" %in% c(test) | "pval" %in% c(test)){
    p_value <- TRUE
    test <- c(test)[!c(test) %in% c("p-value", "p", "pval")]
    if(length(test) == 0) test <- NULL
  } else{
    p_value <- FALSE
  }

  parameters <- bayestestR::describe_posterior(data, centrality = centrality, ci = ci, ci_method = ci_method, test = test, ...)

  # Remove unecessary columns
  if("CI" %in% names(parameters) && length(unique(parameters$CI)) == 1){
    parameters$CI <- NULL
  }

  # Coef
  if(length(c(centrality)) == 1){
    names(parameters)[names(parameters) == tools::toTitleCase(centrality)] <- "Coefficient"
  }

  # p-value
  if(p_value){
    parameters$p <- sapply(data, .p_value_bootstrapped)
  }

  parameters
}


#' @seealso https://blogs.sas.com/content/iml/2011/11/02/how-to-compute-p-values-for-a-bootstrap-distribution.html
#' @keywords internal
.p_value_bootstrapped <- function(x){
  2 * (1 - max(
    c(
      (1 + length(x[x > 0])) / (1 + length(x)),
      (1 + length(x[x < 0])) / (1 + length(x))
    )
  ))
}


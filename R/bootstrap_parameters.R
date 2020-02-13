#' Parameters bootstrapping
#'
#' Compute bootstrapped parameters and their related indices such as Confidence Intervals (CI) and p-values.
#'
#'
#' @param test The indices to compute. Character (vector) with one or more of these options: \code{"p-value"} (or \code{"p"}), \code{"p_direction"} (or \code{"pd"}), \code{"rope"}, \code{"p_map"}, \code{"equivalence_test"} (or \code{"equitest"}), \code{"bayesfactor"} (or \code{"bf"}) or \code{"all"} to compute all tests. For each "test", the corresponding \pkg{bayestestR} function is called (e.g. \code{\link[bayestestR]{rope}} or \code{\link[bayestestR]{p_direction}}) and its results included in the summary output.
#' @inheritParams bootstrap_model
#' @inheritParams bayestestR::describe_posterior
#'
#' @return Bootstrapped parameters.
#'
#' @references Davison, A. C., & Hinkley, D. V. (1997). Bootstrap methods and their application (Vol. 1). Cambridge university press.
#'
#' @seealso \code{\link{bootstrap_model}}, \code{\link{simulate_parameters}}, \code{\link{simulate_model}}
#'
#' @details This function first calls \code{\link{bootstrap_model}} to generate
#'   bootstrapped coefficients. The resulting replicated for each coefficient
#'   are treated as "distribution", and is passed to \code{\link[bayestestR]{describe_posterior}}
#'   to calculate the related indices defined in the \code{"test"} argument.
#'
#' @examples
#' library(parameters)
#'
#' model <- lm(Sepal.Length ~ Species * Petal.Width, data = iris)
#' bootstrap_parameters(model)
#' @importFrom tools toTitleCase
#' @export
bootstrap_parameters <- function(model, iterations = 1000, centrality = "median", ci = .95, ci_method = "quantile", test = "p-value", ...) {
  data <- bootstrap_model(model, iterations = iterations, ...)
  .summary_bootstrap(data = data, test = test, centrality = centrality, ci = ci, ci_method = ci_method, ...)
}





#' @keywords internal
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
    parameters <- parameters[order(parameters$.col_order), ]
    parameters$.col_order <- NULL
  }

  rownames(parameters) <- NULL
  parameters
}

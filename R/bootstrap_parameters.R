#' Parameters bootstrapping
#'
#' Compute bootstrapped parameters and their related indices such as Confidence Intervals (CI) and p-values.
#'
#'
#' @param test The indices to compute. Character (vector) with one or more of these options: `"p-value"` (or `"p"`), `"p_direction"` (or `"pd"`), `"rope"`, `"p_map"`, `"equivalence_test"` (or `"equitest"`), `"bayesfactor"` (or `"bf"`) or `"all"` to compute all tests. For each "test", the corresponding \pkg{bayestestR} function is called (e.g. [bayestestR::rope()] or [bayestestR::p_direction()]) and its results included in the summary output.
#' @inheritParams bootstrap_model
#' @inheritParams bayestestR::describe_posterior
#'
#' @return A data frame summarizing the bootstrapped parameters.
#'
#' @inheritSection bootstrap_model Using with `emmeans`
#'
#' @references Davison, A. C., & Hinkley, D. V. (1997). Bootstrap methods and their application (Vol. 1). Cambridge university press.
#'
#' @seealso [bootstrap_model()], [simulate_parameters()], [simulate_model()]
#'
#' @details This function first calls [bootstrap_model()] to generate
#'   bootstrapped coefficients. The resulting replicated for each coefficient
#'   are treated as "distribution", and is passed to [bayestestR::describe_posterior()]
#'   to calculate the related indices defined in the `"test"` argument.
#'   \cr\cr
#'   Note that that p-values returned here are estimated under the assumption of
#'   *translation equivariance*: that shape of the sampling distribution is
#'   unaffected by the null being true or not. If this assumption does not hold,
#'   p-values can be biased, and it is suggested to use proper permutation tests
#'   to obtain non-parametric p-values.
#'
#' @examples
#' \dontrun{
#' if (require("boot", quietly = TRUE)) {
#'   set.seed(2)
#'   model <- lm(Sepal.Length ~ Species * Petal.Width, data = iris)
#'   b <- bootstrap_parameters(model)
#'   print(b)
#'
#'   if (require("emmeans")) {
#'     est <- emmeans(b, trt.vs.ctrl ~ Species)
#'     print(model_parameters(est))
#'   }
#' }
#' }
#' @export
bootstrap_parameters <- function(model,
                                 iterations = 1000,
                                 centrality = "median",
                                 ci = .95,
                                 ci_method = "quantile",
                                 test = "p-value",
                                 ...) {
  data <- bootstrap_model(model, iterations = iterations, ...)
  out <- .summary_bootstrap(
    data = data,
    test = test,
    centrality = centrality,
    ci = ci,
    ci_method = ci_method,
    ...
  )

  class(out) <- c("bootstrap_parameters", "parameters_model", class(out))
  attr(out, "boot_samples") <- data
  out
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

  parameters <- bayestestR::describe_posterior(
    data,
    centrality = centrality,
    ci = ci,
    ci_method = ci_method,
    test = test,
    ...
  )

  # Remove unnecessary columns
  if ("CI" %in% names(parameters) && .n_unique(parameters$CI) == 1) {
    parameters$CI <- NULL
  } else if ("CI" %in% names(parameters) && .n_unique(parameters$CI) > 1) {
    parameters <- datawizard::reshape_ci(parameters)
  }

  # Coef
  if (length(c(centrality)) == 1) {
    names(parameters)[names(parameters) == .capitalize(centrality)] <- "Coefficient"
  }

  # p-value
  if (p_value) {
    parameters$.row_order <- 1:nrow(parameters)
    # calculate probability of direction, then convert to p.
    p <- bayestestR::p_direction(data, null = 0, ...)
    p$p <- bayestestR::pd_to_p(p$pd)
    p$pd <- NULL
    parameters <- merge(parameters, p, all = TRUE)
    parameters <- parameters[order(parameters$.row_order), ]
    parameters$.row_order <- NULL
  }

  rownames(parameters) <- NULL
  attr(parameters, "ci") <- ci
  parameters
}

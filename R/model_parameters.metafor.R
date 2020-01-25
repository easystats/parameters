#' Parameters from Meta-Analysis
#'
#' Extract and compute indices and measures to describe parameters of meta-analysis models.
#'
#' @inheritParams model_parameters.default
#'
#' @examples
#' library(parameters)
#' data <- data.frame(
#'   estimate = c(0.111, 0.245, 0.8, 1.1, 0.03),
#'   std.error = c(0.05, 0.111, 0.001, 0.2, 0.01)
#' )
#' if (require("metafor")) {
#'   model <- rma(yi = estimate, sei = std.error, data = data)
#'   model_parameters(model)
#' }
#' @return A data frame of indices related to the model's parameters.
#' @importFrom stats qt pt setNames
#' @export
model_parameters.rma <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, standardize = NULL, exponentiate = FALSE, ...) {
  meta_analysis_overall <- .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = "Parameter",
    standardize = standardize,
    exponentiate = exponentiate,
    ...
  )

  alpha <- (1 + ci) / 2

  rma_parameters <- if (!is.null(model$slab)) {
    sprintf("Study %s", model$slab)
  } else {
    sprintf("Study %i", 1:model[["k"]])
  }

  rma_coeffients <- as.vector(model$yi)
  rma_se <- as.vector(sqrt(model$vi))
  rma_ci_low <- rma_coeffients - rma_se * stats::qt(alpha, df = Inf)
  rma_ci_high <- rma_coeffients + rma_se * stats::qt(alpha, df = Inf)
  rma_statistic <- rma_coeffients / rma_se
  rma_ci_p <- 2 * stats::pt(abs(rma_statistic), df = Inf, lower.tail = FALSE)

  meta_analysis_studies <- data.frame(
    Parameter = rma_parameters,
    Coefficient = rma_coeffients,
    SE = rma_se,
    CI_low = rma_ci_low,
    CI_high = rma_ci_high,
    z = rma_statistic,
    df_error = NA,
    p = rma_ci_p,
    Weight = 1 / as.vector(model$vi),
    stringsAsFactors = FALSE
  )

  original_attributes <- attributes(meta_analysis_overall)
  out <- merge(meta_analysis_studies, meta_analysis_overall, all = TRUE, sort = FALSE)

  # fix intercept name
  out$Parameter[out$Parameter == "(Intercept)"] <- "Overall"

  original_attributes$names <- names(out)
  original_attributes$row.names <- 1:nrow(out)
  original_attributes$pretty_names <- stats::setNames(out$Parameter, out$Parameter)
  attributes(out) <- original_attributes

  # no df
  out$df_error <- NULL
  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)

  out
}

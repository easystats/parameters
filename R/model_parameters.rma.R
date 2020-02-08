#' Parameters from Meta-Analysis
#'
#' Extract and compute indices and measures to describe parameters of meta-analysis models.
#'
#' @inheritParams model_parameters.default
#'
#' @examples
#' library(parameters)
#' mydat <- data.frame(
#'   effectsize = c(-0.393, 0.675, 0.282, -1.398),
#'   stderr = c(0.317, 0.317, 0.13, 0.36)
#' )
#' if (require("metafor")) {
#'   model <- rma(yi = effectsize, sei = stderr, method = "REML", data = mydat)
#'   model_parameters(model)
#' }
#'
#' # with subgroups
#' if (require("metafor")) {
#'   data(dat.bcg)
#'   dat <- escalc(
#'     measure = "RR",
#'     ai = tpos,
#'     bi = tneg,
#'     ci = cpos,
#'     di = cneg,
#'     data = dat.bcg
#'   )
#'   dat$alloc <- ifelse(dat$alloc == "random", "random", "other")
#'   model <- rma(yi, vi, mods = ~ alloc, data = dat, digits = 3, slab = author)
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

  # remove specific parameters
  remove <- which(meta_analysis_overall$Parameter != "(Intercept)")
  if (length(remove) > 0) {
    meta_analysis_overall <- meta_analysis_overall[-remove, ]
  }

  alpha <- (1 + ci) / 2

  rma_parameters <- if (!is.null(model$slab)) {
    sprintf("%s", model$slab)
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

  # subgroup analyses?
  if (!is.null(model$formula.mods)) {
    subgroups <- deparse(model$formula.mods[[2]])[1]
    model_data <- insight::get_data(model)
    if (subgroups %in% colnames(model_data)) {
      out$Subgroup <- NA
      out$Subgroup[out$Parameter != "(Intercept)"] <- insight::get_data(model)[[subgroups]]
      out$Subgroup[out$Parameter == "(Intercept)"] <- "Overall"
    }
  }

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

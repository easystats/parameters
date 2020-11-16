#' @export
model_parameters.lqmm <- function(model,
                                  ci = .95,
                                  bootstrap = FALSE,
                                  iterations = 1000,
                                  p_adjust = NULL,
                                  verbose = TRUE,
                                  ...) {

  # Processing
  if (bootstrap) {
    parameters <- bootstrap_parameters(model, iterations = iterations, ci = ci, ...)
  } else {
    parameters <- .extract_parameters_lqmm(model, ci = ci, p_adjust = p_adjust, ...)
  }

  parameters <- .add_model_parameters_attributes(parameters, model, ci, exponentiate = FALSE, ...)
  attr(parameters, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))

  parameters
}

#' @export
model_parameters.lqm <- model_parameters.lqmm



.extract_parameters_lqmm <- function(model, ci, p_adjust, ...) {
  cs <- summary(model, ...)
  parameters <- insight::get_parameters(model)

  if (is.list(cs$tTable)) {
    summary_table <- do.call(rbind, cs$tTable)
  } else {
    summary_table <- cs$tTable
  }


  # ==== Coefficient, SE and test statistic

  parameters$Coefficient <- parameters$Estimate
  parameters$SE <- summary_table[, 2]
  parameters$t <- parameters$Estimate / parameters$SE

  # ==== DF and Conf Int

  parameters$df <- tryCatch(
    {
      if (!is.null(cs$rdf)) {
        cs$rdf
      } else {
        attr(cs$B, "R") - 1
      }
    },
    error = function(e) {
      Inf
    }
  )
  parameters$CI_low <- parameters$Coefficient - stats::qt((1 + ci) / 2, df = parameters$df) * parameters$SE
  parameters$CI_high <- parameters$Coefficient + stats::qt((1 + ci) / 2, df = parameters$df) * parameters$SE

  # ==== p-value

  parameters$p <- summary_table[, 5]

  if (!is.null(p_adjust) && tolower(p_adjust) %in% stats::p.adjust.methods && "p" %in% colnames(parameters)) {
    parameters$p <- stats::p.adjust(parameters$p, method = p_adjust)
  }

  # ==== Reorder

  col_order <- c("Parameter", "Coefficient", "SE", "CI_low", "CI_high", "t", "df", "p", "Component")
  parameters[col_order[col_order %in% names(parameters)]]
}

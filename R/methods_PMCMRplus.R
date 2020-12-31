#' @rdname model_parameters.glht
#' @export
model_parameters.PMCMR <- function(model, ...) {
  if (!requireNamespace("PMCMRplus", quietly = TRUE)) {
    stop("Package 'PMCMRplus' needed. Please install it by running `install.packages('PMCMRplus')`.")
  }

  parameters <- PMCMRplus::toTidy(model)
  names(parameters) <- c(
    "Group1", "Group2", "Statistic", "p", "alternative", "Method",
    "Distribution", "p.adjust.method"
  )

  parameters <- .add_htest_parameters_attributes(parameters, model)

  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}


#' @export
model_parameters.osrt <- model_parameters.PMCMR


#' @export
model_parameters.trendPMCMR <- model_parameters.PMCMR

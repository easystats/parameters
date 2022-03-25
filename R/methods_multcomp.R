#' Parameters from Hypothesis Testing
#'
#' Parameters from Hypothesis Testing.
#'
#' @param model Object of class [multcomp::glht()] (**multcomp**)
#'   or of class `PMCMR`, `trendPMCMR` or `osrt` (**PMCMRplus**).
#' @inheritParams model_parameters.default
#'
#' @return A data frame of indices related to the model's parameters.
#'
#' @examples
#' \donttest{
#' if (require("multcomp", quietly = TRUE)) {
#'   # multiple linear model, swiss data
#'   lmod <- lm(Fertility ~ ., data = swiss)
#'   mod <- glht(
#'     model = lmod,
#'     linfct = c(
#'       "Agriculture = 0",
#'       "Examination = 0",
#'       "Education = 0",
#'       "Catholic = 0",
#'       "Infant.Mortality = 0"
#'     )
#'   )
#'   model_parameters(mod)
#' }
#' if (require("PMCMRplus", quietly = TRUE)) {
#'   model <- kwAllPairsConoverTest(count ~ spray, data = InsectSprays)
#'   model_parameters(model)
#' }
#' }
#' @export
model_parameters.glht <- function(model,
                                  ci = .95,
                                  exponentiate = FALSE,
                                  verbose = TRUE,
                                  ...) {

  # p-adjustment method
  s <- summary(model)
  p_adjust <- s$test$type

  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = FALSE,
    iterations = 10,
    merge_by = "Parameter",
    standardize = NULL,
    exponentiate = exponentiate,
    p_adjust = NULL,
    verbose = verbose,
    ...
  )

  attr(out, "p_adjust") <- p_adjust
  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}


#' @export
ci.glht <- function(x, ci = .95, ...) {

  # backward compatibility with `robust` argument
  dots <- list(...)
  if ("robust" %in% names(dots) && !"vcov" %in% names(dots)) {
    robust <- dots[["robust"]]
  } else if (any(c("vcov", "vcov_args") %in% names(dots))) {
    robust <- TRUE
  } else {
    robust <- FALSE
  }

  s <- summary(x)
  if (robust) {
    adjusted_ci <- 2 * stats::pnorm(s$test$qfunction(ci)) - 1
    dof <- Inf
  } else {
    adjusted_ci <- ci
    dof <- x$df
  }
  out <- .ci_generic(model = x, ci = adjusted_ci, dof = dof, ...)
  if (robust) {
    out$CI <- ci
  }
  out
}


#' @export
standard_error.glht <- function(model, ...) {
  s <- summary(model)
  .data_frame(
    Parameter = insight::find_parameters(model, flatten = TRUE),
    SE = unname(s$test$sigma)
  )
}


#' @export
degrees_of_freedom.glht <- function(model, ...) {
  model$df
}


#' @export
p_value.glht <- function(model, ...) {
  s <- summary(model)
  .data_frame(
    Parameter = insight::find_parameters(model, flatten = TRUE),
    p = unname(s$test$pvalues)
  )
}

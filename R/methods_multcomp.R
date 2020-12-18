#' Parameters from Hypothesis Testing
#'
#' Parameters from Hypothesis Testing.
#'
#' @param model Object of class \code{\link[multcomp:glht]{glht}} (\pkg{multcomp}).
#' @inheritParams model_parameters.default
#'
#' @return A data frame of indices related to the model's parameters.
#'
#' @examples
#' \donttest{
#' if (require("multcomp")) {
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
#' }
#' @export
model_parameters.glht <- function(model,
                                  ci = .95,
                                  exponentiate = FALSE,
                                  verbose = TRUE,
                                  ...) {
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = FALSE,
    iterations = 10,
    merge_by = "Parameter",
    standardize = NULL,
    exponentiate = exponentiate,
    robust = FALSE,
    p_adjust = NULL,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}


#' @importFrom stats pnorm
#' @export
ci.glht <- function(x, ci = .95, method = "robust", ...) {
  s <- summary(x)
  robust <- !is.null(method) && method == "robust"
  if (robust) {
    adjusted_ci <- 2 * stats::pnorm(s$test$qfunction(ci)) - 1
    dof <- Inf
  } else {
    adjusted_ci <- ci
    dof <- x$df
  }
  out <- ci_wald(model = x, ci = adjusted_ci, dof = dof, ...)
  if (robust) {
    out$CI <- 100 * ci
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


#' @importFrom insight find_parameters
#' @export
p_value.glht <- function(model, ...) {
  s <- summary(model)
  .data_frame(
    Parameter = insight::find_parameters(model, flatten = TRUE),
    p = unname(s$test$pvalues)
  )
}

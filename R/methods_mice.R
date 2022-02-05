# confidence intervals --------------------------

#' @export
ci.mipo <- ci.gam


#' @export
ci.mira <- function(x, ci = .95, ...) {
  insight::check_if_installed("mice")
  ci(mice::pool(x), ci = ci, ...)
}


# degrees of freedom ----------------------------

#' @export
degrees_of_freedom.mira <- function(model, ...) {
  insight::check_if_installed("mice")
  degrees_of_freedom(mice::pool(model), ...)
}


#' @export
degrees_of_freedom.mipo <- function(model, ...) {
  as.vector(summary(model)$df)
}


# p values ---------------------------------------

#' @export
p_value.mipo <- function(model, ...) {
  .data_frame(
    Parameter = as.vector(summary(model)$term),
    p = as.vector(summary(model)$p.value)
  )
}


#' @export
p_value.mira <- function(model, ...) {
  insight::check_if_installed("mice")
  p_value(mice::pool(model), ...)
}


# standard errors --------------------------------

#' @export
standard_error.mipo <- function(model, ...) {
  .data_frame(
    Parameter = as.vector(summary(model)$term),
    SE = as.vector(summary(model)$std.error)
  )
}


#' @export
standard_error.mira <- function(model, ...) {
  insight::check_if_installed("mice")
  standard_error(mice::pool(model), ...)
}


# format -------------------------------------------

#' @export
format_parameters.mira <- format_parameters.rma


# model_parameters ---------------------------------

#' @export
model_parameters.mipo <- model_parameters.default


#' Parameters from multiply imputed repeated analyses
#'
#' Format models of class `mira`, obtained from `mice::width.mids()`.
#'
#' @param model An object of class `mira`.
#' @inheritParams model_parameters.default
#' @param ... Arguments passed to or from other methods.
#'
#' @details `model_parameters()` for objects of class `mira` works
#'   similar to `summary(mice::pool())`, i.e. it generates the pooled summary
#'   of multiple imputed repeated regression analyses.
#'
#' @examples
#' library(parameters)
#' if (require("mice", quietly = TRUE)) {
#'   data(nhanes2)
#'   imp <- mice(nhanes2)
#'   fit <- with(data = imp, exp = lm(bmi ~ age + hyp + chl))
#'   model_parameters(fit)
#' }
#' \dontrun{
#' # model_parameters() also works for models that have no "tidy"-method in mice
#' if (require("mice", quietly = TRUE) && require("gee", quietly = TRUE)) {
#'   data(warpbreaks)
#'   set.seed(1234)
#'   warpbreaks$tension[sample(1:nrow(warpbreaks), size = 10)] <- NA
#'   imp <- mice(warpbreaks)
#'   fit <- with(data = imp, expr = gee(breaks ~ tension, id = wool))
#'
#'   # does not work:
#'   # summary(pool(fit))
#'
#'   model_parameters(fit)
#' }
#' }
#'
#'
#'
#' # and it works with pooled results
#' if (require("mice")) {
#'   data("nhanes2")
#'   imp <- mice(nhanes2)
#'   fit <- with(data = imp, exp = lm(bmi ~ age + hyp + chl))
#'   pooled <- pool(fit)
#'
#'   model_parameters(pooled)
#' }
#' @export
model_parameters.mira <- function(model,
                                  ci = .95,
                                  exponentiate = FALSE,
                                  p_adjust = NULL,
                                  verbose = TRUE,
                                  ...) {
  insight::check_if_installed("mice")
  out <- .model_parameters_generic(
    model = mice::pool(model),
    ci = ci,
    bootstrap = FALSE,
    iterations = 10,
    merge_by = "Parameter",
    standardize = NULL,
    exponentiate = exponentiate,
    p_adjust = p_adjust,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}

# confidence intervals --------------------------

#' @export
ci.mipo <- ci.gam


#' @export
ci.mira <- function(x, ci = .95, ...) {
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop("Package 'mice' needed for this function to work. Please install it.")
  }
  ci(mice::pool(x), ci = ci, ...)
}


# degrees of freedom ----------------------------

#' @export
degrees_of_freedom.mira <- function(model, ...) {
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop("Package 'mice' needed for this function to work. Please install it.")
  }
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
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop("Package 'mice' needed for this function to work. Please install it.")
  }
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
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop("Package 'mice' needed for this function to work. Please install it.")
  }
  standard_error(mice::pool(model), ...)
}


# format -------------------------------------------

#' @export
format_parameters.mira <- format_parameters.rma


# model_parameters ---------------------------------

#' @include model_parameters.R
#' @export
model_parameters.mipo <- model_parameters.default


#' Parameters from multiply imputed repeated analyses
#'
#' Format models of class \code{mira}, obtained from \code{mice::width.mids()}.
#'
#' @param model An object of class \code{mira}.
#' @inheritParams model_parameters.default
#' @param ... Arguments passed to or from other methods.
#'
#' @details \code{model_parameters()} for objects of class \code{mira} works
#'   similar to \code{summary(mice::pool())}, i.e. it generates the pooled summary
#'   of multiple imputed repeated regression analyses.
#'
#' @examples
#' library(parameters)
#' if (require("mice")) {
#'   data(nhanes2)
#'   imp <- mice(nhanes2)
#'   fit <- with(data = imp, exp = lm(bmi ~ age + hyp + chl))
#'   model_parameters(fit)
#' }
#' \dontrun{
#' # model_parameters() also works for models that have no "tidy"-method in mice
#' if (require("mice") && require("gee")) {
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
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop("Package 'mice' needed for this function to work. Please install it.")
  }
  out <- .model_parameters_generic(
    model = mice::pool(model),
    ci = ci,
    bootstrap = FALSE,
    iterations = 10,
    merge_by = "Parameter",
    standardize = NULL,
    exponentiate = exponentiate,
    robust = FALSE,
    p_adjust = p_adjust,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}

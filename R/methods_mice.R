# confidence intervals --------------------------

#' @export
ci.mipo <- ci.gam


#' @export
ci.mira <- function(x, ci = 0.95, ...) {
  insight::check_if_installed("mice")
  ci(mice::pool(x), ci = ci, ...)
}


# p values ---------------------------------------

#' @export
p_value.mipo <- function(model, ...) {
  s <- summary(model)
  out <- .data_frame(
    Parameter = as.vector(s$term),
    p = as.vector(s$p.value)
  )
  # check for ordinal-alike models
  if ("y.level" %in% colnames(s)) {
    out$Response <- as.vector(s$y.level)
  }
  out
}


#' @export
p_value.mira <- function(model, ...) {
  insight::check_if_installed("mice")
  p_value(mice::pool(model), ...)
}


# standard errors --------------------------------

#' @export
standard_error.mipo <- function(model, ...) {
  s <- summary(model)
  out <- .data_frame(
    Parameter = as.vector(s$term),
    SE = as.vector(s$std.error)
  )
  # check for ordinal-alike models
  if ("y.level" %in% colnames(s)) {
    out$Response <- as.vector(s$y.level)
  }
  out
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
model_parameters.mipo <- function(model,
                                  ci = 0.95,
                                  exponentiate = FALSE,
                                  p_adjust = NULL,
                                  keep = NULL,
                                  drop = NULL,
                                  verbose = TRUE,
                                  ...) {
  # validation check, warn if unsupported argument is used.
  dot_args <- .check_dots(
    dots = list(...),
    not_allowed = c("vcov", "vcov_args"),
    class(model)[1],
    verbose = verbose
  )

  # check if we have ordinal/categorical response
  s <- summary(model)
  if ("y.level" %in% colnames(s)) {
    merge_by <- c("Parameter", "Response")
  } else {
    merge_by <- "Parameter"
  }

  fun_args <- list(
    model,
    ci = ci,
    merge_by = merge_by,
    exponentiate = exponentiate,
    p_adjust = p_adjust,
    keep_parameters = keep,
    drop_parameters = drop,
    vcov = NULL,
    vcov_args = NULL
  )
  fun_args <- c(fun_args, dot_args)

  out <- do.call(".model_parameters_generic", fun_args)
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}


#' Parameters from multiply imputed repeated analyses
#'
#' Format models of class `mira`, obtained from `mice::width.mids()`, or of
#' class `mipo`.
#'
#' @param model An object of class `mira` or `mipo`.
#' @inheritParams model_parameters.default
#' @param ... Arguments passed to or from other methods.
#'
#' @details `model_parameters()` for objects of class `mira` works
#'   similar to `summary(mice::pool())`, i.e. it generates the pooled summary
#'   of multiple imputed repeated regression analyses.
#'
#' @examplesIf require("mice", quietly = TRUE) && require("gee", quietly = TRUE)
#' library(parameters)
#' data(nhanes2, package = "mice")
#' imp <- mice::mice(nhanes2)
#' fit <- with(data = imp, exp = lm(bmi ~ age + hyp + chl))
#' model_parameters(fit)
#' \donttest{
#' # model_parameters() also works for models that have no "tidy"-method in mice
#' data(warpbreaks)
#' set.seed(1234)
#' warpbreaks$tension[sample(1:nrow(warpbreaks), size = 10)] <- NA
#' imp <- mice::mice(warpbreaks)
#' fit <- with(data = imp, expr = gee::gee(breaks ~ tension, id = wool))
#'
#' # does not work:
#' # summary(mice::pool(fit))
#'
#' model_parameters(fit)
#' }
#'
#' # and it works with pooled results
#' data("nhanes2", package = "mice")
#' imp <- mice::mice(nhanes2)
#' fit <- with(data = imp, exp = lm(bmi ~ age + hyp + chl))
#' pooled <- mice::pool(fit)
#'
#' model_parameters(pooled)
#' @export
model_parameters.mira <- function(model,
                                  ci = 0.95,
                                  exponentiate = FALSE,
                                  p_adjust = NULL,
                                  keep = NULL,
                                  drop = NULL,
                                  verbose = TRUE,
                                  ...) {
  insight::check_if_installed("mice")
  micemodel <- suppressWarnings(mice::pool(model))

  out <- .model_parameters_generic(
    model = micemodel,
    ci = ci,
    bootstrap = FALSE,
    iterations = 10,
    merge_by = "Parameter",
    standardize = NULL,
    exponentiate = exponentiate,
    p_adjust = p_adjust,
    keep_parameters = keep,
    drop_parameters = drop,
    verbose = verbose,
    ...
  )

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}

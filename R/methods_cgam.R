#' @title Parameters from Generalized Additive (Mixed) Models
#' @name model_parameters.cgam
#'
#' @description Extract and compute indices and measures to describe parameters
#'   of generalized additive models (GAM(M)s).
#'
#' @param model A gam/gamm model.
#' @inheritParams model_parameters.default
#'
#' @seealso [insight::standardize_names()] to rename
#'   columns into a consistent, standardized naming scheme.
#'
#' @details The reporting of degrees of freedom *for the spline terms*
#' slightly differs from the output of `summary(model)`, for example in the
#' case of `mgcv::gam()`. The *estimated degrees of freedom*, column
#' `edf` in the summary-output, is named `df` in the returned data
#' frame, while the column `df_error` in the returned data frame refers to
#' the residual degrees of freedom that are returned by `df.residual()`.
#' Hence, the values in the the column `df_error` differ from the column
#' `Ref.df` from the summary, which is intentional, as these reference
#' degrees of freedom \dQuote{is not very interpretable}
#' ([web](https://stat.ethz.ch/pipermail/r-help/2019-March/462135.html)).
#'
#' @return A data frame of indices related to the model's parameters.
#'
#' @examples
#' library(parameters)
#' if (require("mgcv")) {
#'   dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
#'   model <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)
#'   model_parameters(model)
#' }
#' @export
model_parameters.cgam <- function(model,
                                  ci = 0.95,
                                  ci_method = "residual",
                                  bootstrap = FALSE,
                                  iterations = 1000,
                                  standardize = NULL,
                                  exponentiate = FALSE,
                                  p_adjust = NULL,
                                  keep = NULL,
                                  drop = NULL,
                                  verbose = TRUE,
                                  ...) {
  # validation check, warn if unsupported argument is used.
  dot_args <- .check_dots(
    dots = list(...),
    not_allowed = c("vcov", "vcov_args", "component"),
    class(model)[1],
    verbose = verbose
  )

  # Processing
  if (bootstrap) {
    params <- bootstrap_parameters(
      model,
      iterations = iterations,
      ci = ci,
      ...
    )
  } else {
    fun_args <- list(
      model,
      ci = ci,
      ci_method = ci_method,
      component = "all",
      merge_by = c("Parameter", "Component"),
      standardize = standardize,
      p_adjust = p_adjust,
      keep_parameters = keep,
      drop_parameters = drop,
      verbose = verbose,
      vcov = NULL,
      vcov_args = NULL
    )
    fun_args <- c(fun_args, dot_args)
    params <- do.call(".extract_parameters_generic", fun_args)
  }

  # fix statistic column
  if ("t" %in% names(params) && !is.null(params$Component) && "smooth_terms" %in% params$Component) {
    names(params)[names(params) == "t"] <- "t / F"
  }

  # fix estimated df column
  if (inherits(model, c("gam", "cgam", "scam", "rqss")) && "smooth_terms" %in% params$Component && !("df" %in% names(params))) { # nolint
    params$df <- params$Coefficient
    params$df[params$Component != "smooth_terms"] <- NA
    params$df_error[params$Component == "smooth_terms"] <- NA
    params$Coefficient[params$Component == "smooth_terms"] <- NA
    # reorder
    insert_column <- which(names(params) == "df_error")
    if (!length(insert_column)) {
      insert_column <- which(names(params) == "p")
    }
    if (length(insert_column)) {
      n_col <- ncol(params)
      params <- params[c(1:(insert_column - 1), n_col, insert_column:(n_col - 1))]
    }
  } else if (all(c("df", "df_error") %in% names(params)) && "smooth_terms" %in% params$Component) {
    params$df_error[params$Component == "smooth_terms"] <- NA
  }

  # exponentiate coefficients and SE/CI, if requested
  params <- .exponentiate_parameters(params, model, exponentiate)

  params <- .add_model_parameters_attributes(
    params,
    model,
    ci,
    exponentiate,
    p_adjust = p_adjust,
    verbose = verbose,
    ...
  )

  if ("CI" %in% colnames(params)) {
    params$CI[is.na(params$CI_low)] <- NA
  }

  attr(params, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}


#' @export
p_value.cgam <- function(model, component = "all", ...) {
  component <- insight::validate_argument(
    component,
    c("all", "conditional", "smooth_terms")
  )

  params <- insight::get_parameters(model, component = "all")
  cs <- summary(model)
  p <- as.vector(cs$coefficients[, 4])
  if (!is.null(cs$coefficients2)) p <- c(p, as.vector(cs$coefficients2[, "p.value"]))

  out <- .data_frame(
    Parameter = params$Parameter,
    Component = params$Component,
    p = as.vector(p)
  )

  if (component != "all") {
    out <- out[out$Component == component, ]
  }

  out
}


#' @export
standard_error.cgam <- function(model, ...) {
  sc <- summary(model)
  se <- as.vector(sc$coefficients[, "StdErr"])

  params <- insight::get_parameters(model, component = "all")

  if (!is.null(sc$coefficients2)) se <- c(se, rep(NA, nrow(sc$coefficients2)))

  .data_frame(
    Parameter = params$Parameter,
    SE = se,
    Component = params$Component
  )
}

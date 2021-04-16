#' @title Parameters from Generalized Additive (Mixed) Models
#' @name model_parameters.cgam
#'
#' @description Extract and compute indices and measures to describe parameters
#'   of generalized additive models (GAM(M)s).
#'
#' @param model A gam/gamm model.
#' @inheritParams model_parameters.default
#'
#' @seealso \code{\link[insight:standardize_names]{standardize_names()}} to rename
#'   columns into a consistent, standardized naming scheme.
#'
#' @details The reporting of degrees of freedom \emph{for the spline terms}
#' slightly differs from the output of \code{summary(model)}, for example in the
#' case of \code{mgcv::gam()}. The \emph{estimated degrees of freedom}, column
#' \code{edf} in the summary-output, is named \code{df} in the returned data
#' frame, while the column \code{df_error} in the returned data frame refers to
#' the residual degrees of freedom that are returned by \code{df.residual()}.
#' Hence, the values in the the column \code{df_error} differ from the column
#' \code{Ref.df} from the summary, which is intentional, as these reference
#' degrees of freedom \dQuote{is not very interpretable}
#' (\href{https://r.789695.n4.nabble.com/ref-df-in-mgcv-gam-tp4756194p4756195.html}{web}).
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
                                  ci = .95,
                                  bootstrap = FALSE,
                                  iterations = 1000,
                                  standardize = NULL,
                                  exponentiate = FALSE,
                                  robust = FALSE,
                                  p_adjust = NULL,
                                  parameters = NULL,
                                  verbose = TRUE,
                                  ...) {
  # Processing
  if (bootstrap) {
    params <- bootstrap_parameters(
      model,
      iterations = iterations,
      ci = ci,
      ...
    )
  } else {
    params <-
      .extract_parameters_generic(
        model,
        ci = ci,
        component = "all",
        merge_by = c("Parameter", "Component"),
        standardize = standardize,
        robust = robust,
        p_adjust = p_adjust,
        filter_parameters = parameters
      )
  }

  # fix statistic column
  if ("t" %in% names(params) && !is.null(params$Component) && "smooth_terms" %in% params$Component) {
    names(params)[names(params) == "t"] <- "t / F"
  }

  # fix estimated df column
  if (inherits(model, c("gam", "cgam", "scam", "rqss")) && "smooth_terms" %in% params$Component && !("df" %in% names(params))) {
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

  if (isTRUE(exponentiate) || identical(exponentiate, "nongaussian")) {
    params <- .exponentiate_parameters(params, model, exponentiate)
  }
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

  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}



#' @rdname p_value.DirichletRegModel
#' @export
p_value.cgam <- function(model, component = c("all", "conditional", "smooth_terms"), ...) {
  component <- match.arg(component)

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


#' @export
degrees_of_freedom.cgam <- function(model, ...) {
  model$resid_df_obs
}

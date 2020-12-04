#' Parameters from Generalized Additive (Mixed) Models
#'
#' Extract and compute indices and measures to describe parameters of generalized additive models (GAM(M)s).
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
model_parameters.gam <- function(model,
                                 ci = .95,
                                 bootstrap = FALSE,
                                 iterations = 1000,
                                 standardize = NULL,
                                 exponentiate = FALSE,
                                 robust = FALSE,
                                 p_adjust = NULL,
                                 verbose = TRUE,
                                 ...) {

  # Processing
  if (bootstrap) {
    parameters <- bootstrap_parameters(model, iterations = iterations, ci = ci, ...)
  } else {
    parameters <-
      .extract_parameters_generic(
        model,
        ci = ci,
        component = "all",
        merge_by = c("Parameter", "Component"),
        standardize = standardize,
        robust = robust,
        p_adjust = p_adjust
      )
  }

  # fix statistic column
  if ("t" %in% names(parameters) && !is.null(parameters$Component) && "smooth_terms" %in% parameters$Component) {
    names(parameters)[names(parameters) == "t"] <- "t / F"
  }

  # fix estimated df column
  if (inherits(model, c("gam", "scam")) && "smooth_terms" %in% parameters$Component && !("df" %in% names(parameters))) {
    parameters$df <- parameters$Coefficient
    parameters$df[parameters$Component != "smooth_terms"] <- NA
    parameters$Coefficient[parameters$Component == "smooth_terms"] <- NA
    # reorder
    insert_column <- which(names(parameters) == "df_error")
    if (!length(insert_column)) {
      insert_column <- which(names(parameters) == "p")
    }
    if (length(insert_column)) {
      n_col <- ncol(parameters)
      parameters <- parameters[c(1:(insert_column - 1), n_col, insert_column:(n_col - 1))]
    }
  }

  if (exponentiate) parameters <- .exponentiate_parameters(parameters)
  parameters <- .add_model_parameters_attributes(
    parameters,
    model,
    ci,
    exponentiate,
    p_adjust = p_adjust,
    verbose = verbose,
    ...
  )
  attr(parameters, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))

  parameters
}


#' @export
model_parameters.vgam <- model_parameters.gam

#' @export
model_parameters.scam <- model_parameters.gam


#' @export
model_parameters.gamm <- function(model,
                                  ci = .95,
                                  bootstrap = FALSE,
                                  iterations = 1000,
                                  verbose = TRUE,
                                  ...) {
  model <- model$gam
  class(model) <- c("gam", "lm", "glm")
  model_parameters(
    model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    robust = FALSE,
    ...
  )
}


#' @export
model_parameters.list <- function(model,
                                  ci = .95,
                                  bootstrap = FALSE,
                                  iterations = 1000,
                                  robust = FALSE,
                                  verbose = TRUE,
                                  ...) {
  if ("gam" %in% names(model)) {
    model <- model$gam
    class(model) <- c("gam", "lm", "glm")
    model_parameters(
      model,
      ci = ci,
      bootstrap = bootstrap,
      iterations = iterations,
      robust = robust,
      ...
    )
  }
}


#' @export
model_parameters.gamlss <- model_parameters.gam



#' @rdname model_parameters.gam
#' @export
model_parameters.rqss <- function(model,
                                  ci = .95,
                                  bootstrap = FALSE,
                                  iterations = 1000,
                                  component = c("conditional", "smooth_terms", "all"),
                                  standardize = NULL,
                                  exponentiate = FALSE,
                                  verbose = TRUE,
                                  ...) {
  component <- match.arg(component)
  if (component == "all") {
    merge_by <- c("Parameter", "Component")
  } else {
    merge_by <- "Parameter"
  }

  ## TODO check merge by

  out <-
    .model_parameters_generic(
      model = model,
      ci = ci,
      component = component,
      bootstrap = bootstrap,
      iterations = iterations,
      merge_by = merge_by,
      standardize = standardize,
      exponentiate = exponentiate,
      ...
    )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}


#' @rdname model_parameters.gam
#' @export
model_parameters.cgam <- model_parameters.rqss


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


#' @title Parameters from Generalized Additive (Mixed) Models
#' @name model_parameters.cgam
#'
#' @description Extract and compute indices and measures to describe parameters of generalized additive models (GAM(M)s).
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

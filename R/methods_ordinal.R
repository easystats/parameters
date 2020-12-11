
#' @export
ci.clm <- ci.gamlss

#' @rdname ci.merMod
#' @export
ci.clm2 <- function(x, ci = .95, component = c("all", "conditional", "scale"), ...) {
  component <- match.arg(component)
  ci_wald(model = x, ci = ci, dof = Inf, component = component)
}

#' @export
ci.clmm2 <- ci.clm2

#' @rdname standard_error
#' @importFrom insight get_parameters
#' @export
standard_error.clm2 <- function(model, component = c("all", "conditional", "scale"), ...) {
  component <- match.arg(component)
  stats <- .get_se_from_summary(model)
  parms <- insight::get_parameters(model, component = component)

  .data_frame(
    Parameter = parms$Parameter,
    SE = stats[parms$Parameter],
    Component = parms$Component
  )
}


#' @export
standard_error.clmm2 <- standard_error.clm2


#' @export
model_parameters.clmm2 <- model_parameters.clm2


#' @rdname model_parameters.merMod
#' @export
model_parameters.clmm <- function(model,
                                  ci = .95,
                                  bootstrap = FALSE,
                                  iterations = 1000,
                                  standardize = NULL,
                                  exponentiate = FALSE,
                                  details = FALSE,
                                  df_method = NULL,
                                  verbose = TRUE,
                                  ...) {

  # p-values, CI and se might be based on differen df-methods
  df_method <- .check_df_method(df_method)

  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = "Parameter",
    standardize = standardize,
    exponentiate = exponentiate,
    effects = "fixed",
    robust = FALSE,
    df_method = df_method,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)

  if (isTRUE(details)) {
    attr(out, "details") <- .randomeffects_summary(model)
  }

  out
}


# tools --------------------

.check_df_method <- function(df_method) {
  if (!is.null(df_method)) {
    df_method <- tolower(df_method)
    if (df_method %in% c("satterthwaite", "kenward", "kr")) {
      warning("Satterthwaite or Kenward-Rogers approximation of degrees of freedom is only available for linear mixed models.", call. = FALSE)
      df_method <- "wald"
    }
    df_method <- match.arg(df_method, choices = c("wald", "ml1", "betwithin"))
  }
  df_method
}

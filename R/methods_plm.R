# plm package: .plm, .pgmm, .pggls


# plm ---------------------------


#' @export
standard_error.plm <- function(model, ...) {
  se <- stats::coef(summary(model))

  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(se)),
    SE = as.vector(se[, 2])
  )
}


#' @export
p_value.plm <- function(model, ...) {
  p <- stats::coef(summary(model))

  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(p)),
    p = as.vector(p[, 4])
  )
}



# pggls ------------------------


#' @export
p_value.pggls <- function(model, ...) {
  cs <- summary(model)$CoefTable
  p <- cs[, 4]
  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(cs)),
    p = as.vector(p)
  )
}



# pgmm --------------------


#' @export
model_parameters.pgmm <- function(model,
                                  ci = .95,
                                  component = c("conditional", "all"),
                                  exponentiate = FALSE,
                                  robust = TRUE,
                                  p_adjust = NULL,
                                  keep = NULL,
                                  drop = NULL,
                                  parameters = keep,
                                  verbose = TRUE,
                                  ...) {
  component <- match.arg(component)

  params <- .extract_parameters_generic(
    model,
    merge_by = c("Parameter", "Component"),
    ci = ci,
    component = component,
    robust = robust,
    p_adjust = p_adjust,
    keep_parameters = keep,
    drop_parameters = drop,
    ...
  )


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
  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}


#' @export
standard_error.pgmm <- function(model, component = c("conditional", "all"), ...) {
  component <- match.arg(component)
  params <- insight::get_parameters(model, component = component, ...)
  se <- sqrt(diag(insight::get_varcov(model, component = component, ...)))

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(se)
  )
}


#' @export
ci.pgmm <- function(x, ci = .95, dof = Inf, method = NULL, robust = FALSE, component = "conditional", ...) {
  if (!is.null(method)) {
    method <- tolower(method)
  } else {
    method <- "wald"
  }

  .ci_generic(model = x, ci = ci, dof = dof, robust = robust, method == method, component = component)
}

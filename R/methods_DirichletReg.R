
#' @rdname model_parameters.mlm
#' @export
model_parameters.DirichletRegModel <- function(model,
                                               ci = .95,
                                               bootstrap = FALSE,
                                               iterations = 1000,
                                               component = c("all", "conditional", "precision"),
                                               standardize = NULL,
                                               exponentiate = FALSE,
                                               verbose = TRUE,
                                               ...) {
  component <- match.arg(component)
  if (component == "all") {
    merge_by <- c("Parameter", "Component", "Response")
  } else {
    merge_by <- c("Parameter", "Response")
  }

  ## TODO check merge by

  junk <- utils::capture.output(out <- .model_parameters_generic(
    model = model,
    ci = ci,
    component = component,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = merge_by,
    standardize = standardize,
    exponentiate = exponentiate,
    ...
  ))

  out$Response[is.na(out$Response)] <- ""
  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}


#' @export
ci.DirichletRegModel <- function(x,
                                 ci = .95,
                                 component = c("all", "conditional", "precision"),
                                 ...) {
  component <- match.arg(component)
  params <- insight::get_parameters(x, component = component)
  out <- .ci_generic(model = x, ci = ci, dof = Inf, ...)

  if (is.null(out$Component)) {
    component <- "all"
  }
  if ("Response" %in% colnames(params)) {
    out$Response <- params$Response
  }
  if (component != "all") {
    out <- out[out$Component == component, ]
  }

  out
}


#' @rdname standard_error
#' @export
standard_error.DirichletRegModel <- function(model,
                                             component = c("all", "conditional", "precision"),
                                             ...) {
  component <- match.arg(component)
  params <- insight::get_parameters(model)

  out <- .data_frame(
    Parameter = params$Parameter,
    Response = params$Response,
    SE = as.vector(model$se)
  )

  if (!is.null(params$Component)) {
    out$Component <- params$Component
  } else {
    component <- "all"
  }

  if (component != "all") {
    out <- out[out$Component == component, ]
  }

  out
}


#' @title p-values for Models with Special Components
#' @name p_value.DirichletRegModel
#'
#' @description This function attempts to return, or compute, p-values of models
#'   with special model components.
#'
#' @param model A statistical model.
#' @param component Should all parameters, parameters for the conditional model,
#'   precision- or scale-component or smooth_terms be returned? `component`
#'   may be one of `"conditional"`, `"precision"`, `"scale"`,
#'   `"smooth_terms"`, `"full"` or `"all"` (default).
#' @inheritParams p_value
#'
#' @return The p-values.
#' @export
p_value.DirichletRegModel <- function(model,
                                      component = c("all", "conditional", "precision"),
                                      ...) {
  component <- match.arg(component)
  params <- insight::get_parameters(model)

  out <- .data_frame(
    Parameter = params$Parameter,
    Response = params$Response,
    p = as.vector(2 * stats::pnorm(-abs(params$Estimate / model$se)))
  )

  if (!is.null(params$Component)) {
    out$Component <- params$Component
  } else {
    component <- "all"
  }

  if (component != "all") {
    out <- out[out$Component == component, ]
  }

  out
}

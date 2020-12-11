
#' @rdname ci.merMod
#' @export
ci.DirichletRegModel <- function(x, ci = .95, component = c("all", "conditional", "precision"), ...) {
  component <- match.arg(component)
  params <- insight::get_parameters(x, component = component)
  out <- ci_wald(model = x, ci = ci, dof = Inf, ...)

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
standard_error.DirichletRegModel <- function(model, component = c("all", "conditional", "precision"), ...) {
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
    robust = FALSE,
    ...
  ))

  out$Response[is.na(out$Response)] <- ""
  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}


#' @rdname model_parameters.mlm
#' @export
model_parameters.clm2 <- function(model,
                                  ci = .95,
                                  bootstrap = FALSE,
                                  iterations = 1000,
                                  component = c("all", "conditional", "scale"),
                                  standardize = NULL,
                                  exponentiate = FALSE,
                                  p_adjust = NULL,
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
      merge_by = c("Parameter", "Component"),
      standardize = standardize,
      exponentiate = exponentiate,
      p_adjust = p_adjust,
      ...
    )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}


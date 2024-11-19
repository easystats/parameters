# model parameters -------------------


#' @rdname model_parameters.mlm
#' @export
model_parameters.clm2 <- function(model,
                                  ci = 0.95,
                                  bootstrap = FALSE,
                                  iterations = 1000,
                                  component = "all",
                                  standardize = NULL,
                                  exponentiate = FALSE,
                                  p_adjust = NULL,
                                  summary = getOption("parameters_summary", FALSE),
                                  include_info = getOption("parameters_info", FALSE),
                                  keep = NULL,
                                  drop = NULL,
                                  verbose = TRUE,
                                  ...) {
  component <- insight::validate_argument(component, c("all", "conditional", "scale"))
  if (component == "all") {
    merge_by <- c("Parameter", "Component")
  } else {
    merge_by <- "Parameter"
  }

  ## TODO remove deprecated later
  if (!missing(summary)) {
    .deprecated_warning("summary", "include_info", verbose)
    include_info <- summary
  }

  ## TODO check merge by

  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    component = component,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = c("Parameter", "Component"),
    standardize = standardize,
    exponentiate = exponentiate,
    p_adjust = p_adjust,
    keep_parameters = keep,
    drop_parameters = drop,
    include_info = include_info,
    ...
  )

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}


#' @export
model_parameters.clmm2 <- model_parameters.clm2


#' @export
model_parameters.clmm <- model_parameters.cpglmm




# CI ---------------------


## TODO residual df?

#' @export
ci.clm2 <- function(x, ci = 0.95, component = c("all", "conditional", "scale"), ...) {
  component <- match.arg(component)
  .ci_generic(model = x, ci = ci, dof = Inf, component = component)
}


#' @export
ci.clmm2 <- ci.clm2



# standard errors -----------------


#' @export
standard_error.clm2 <- function(model, component = "all", ...) {
  component <- match.arg(component, choices = c("all", "conditional", "scale"))
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




# p values ----------------


#' @export
p_value.clm2 <- function(model, component = "all", ...) {
  component <- insight::validate_argument(
    component,
    c("all", "conditional", "scale")
  )

  params <- insight::get_parameters(model)
  cs <- stats::coef(summary(model))
  p <- cs[, 4]

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
p_value.clmm2 <- p_value.clm2


# simulate model -------------------


#' @export
simulate_model.clm2 <- function(model, iterations = 1000, component = "all", ...) {
  component <- insight::validate_argument(
    component,
    c("all", "conditional", "scale")
  )
  out <- .simulate_model(model, iterations, component = component, ...)

  class(out) <- c("parameters_simulate_model", class(out))
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}


#' @export
simulate_model.clmm2 <- simulate_model.clm2

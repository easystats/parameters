# model parameters -------------------


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
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}


#' @export
model_parameters.clmm2 <- model_parameters.clm2


#' @rdname model_parameters.merMod
#' @export
model_parameters.clmm <- model_parameters.cpglmm




# CI ---------------------


#' @export
ci.clm <- ci.tobit


#' @rdname ci.merMod
#' @export
ci.clm2 <- function(x, ci = .95, component = c("all", "conditional", "scale"), ...) {
  component <- match.arg(component)
  ci_wald(model = x, ci = ci, dof = Inf, component = component)
}


#' @export
ci.clmm2 <- ci.clm2



# standard errors -----------------


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




# p values ----------------


#' @importFrom stats coef
#' @importFrom insight get_parameters
#' @rdname p_value.DirichletRegModel
#' @export
p_value.clm2 <- function(model, component = c("all", "conditional", "scale"), ...) {
  component <- match.arg(component)

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
simulate_model.clm2 <- function(model,
                                iterations = 1000,
                                component = c("all", "conditional", "scale"),
                                ...) {
  component <- match.arg(component)
  out <- .simulate_model(model, iterations, component = component)

  class(out) <- c("parameters_simulate_model", class(out))
  attr(out, "object_name") <- .safe_deparse(substitute(model))
  out
}


#' @export
simulate_model.clmm2 <- simulate_model.clm2

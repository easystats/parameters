#' @export
model_parameters.lcmm <- function(
  model,
  ci = 0.95,
  ci_method = "residual",
  component = "all",
  p_adjust = NULL,
  keep = NULL,
  drop = NULL,
  verbose = TRUE,
  ...
) {
  out <- .model_parameters_generic(
    model = model,
    effects = "all",
    component = component,
    ci = ci,
    ci_method = ci_method,
    merge_by = c("Parameter", "Component", "Group"),
    p_adjust = p_adjust,
    keep_parameters = keep,
    drop_parameters = drop,
    include_info = FALSE,
    verbose = verbose,
    ...
  )

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}

#' @export
model_parameters.externVar <- function(
  model,
  ci = 0.95,
  ci_method = "residual",
  component = "all",
  p_adjust = NULL,
  keep = NULL,
  drop = NULL,
  verbose = TRUE,
  ...
) {
  out <- .model_parameters_generic(
    model = model,
    effects = "all",
    component = component,
    ci = ci,
    ci_method = ci_method,
    merge_by = c("Parameter", "Group"),
    p_adjust = p_adjust,
    keep_parameters = keep,
    drop_parameters = drop,
    include_info = FALSE,
    verbose = verbose,
    ...
  )

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}

#' @export
model_parameters.externX <- model_parameters.externVar


# p-values ----------------------------------------------------------------

# Helper to format output for lcmm methods
.format_lcmm_output <- function(model, value, colname, component, ...) {
  component <- insight::validate_argument(
    component,
    c("all", "conditional", "membership", "longitudinal", "beta", "splines", "linear")
  )

  out <- insight::get_parameters(model, component = "all", ...)
  out[[colname]] <- as.vector(value)

  if (component != "all") {
    out <- out[out$Component == component, , drop = FALSE]
  }

  # clean up
  out$Estimate <- NULL
  out <- out[intersect(c("Parameter", colname, "Component", "Group"), colnames(out))]

  insight::text_remove_backticks(out, verbose = FALSE)
}


#' @export
p_value.lcmm <- function(model, component = "all", ...) {
  id <- seq_along(model$best)
  indice <- rep(id * (id + 1) / 2)
  se <- sqrt(model$V[indice])
  statistic <- model$best / se
  p <- 2 * stats::pt(abs(statistic), df = Inf, lower.tail = FALSE)

  p <- p[!startsWith(names(model$best), "cholesky ") & !startsWith(names(model$best), "varcov ")]
  .format_lcmm_output(model, p, "p", component, ...)
}

#' @export
p_value.externX <- p_value.lcmm

#' @export
p_value.externVar <- p_value.lcmm


# standard errors -------------------------------------------------------------

#' @export
standard_error.lcmm <- function(model, component = "all", ...) {
  id <- seq_along(model$best)
  indice <- rep(id * (id + 1) / 2)
  se <- sqrt(model$V[indice])

  se <- se[!startsWith(names(model$best), "cholesky ") & !startsWith(names(model$best), "varcov ")]
  .format_lcmm_output(model, se, "SE", component, ...)
}

#' @export
standard_error.externX <- standard_error.lcmm

#' @export
standard_error.externVar <- standard_error.lcmm

# plm package: .plm, .pgmm, .pggls


# plm ---------------------------


#' @export
degrees_of_freedom.plm <- function(model, method = "wald", ...) {
  if (identical(method, "normal")) {
    return(Inf)
  } else {
    model$df.residual
  }
}


#' @export
standard_error.plm <- function(model, vcov = NULL, vcov_args = NULL, verbose = TRUE, ...) {
  dots <- list(...)
  se <- NULL
  se_standard <- stats::coef(summary(model))

  # vcov: matrix
  if (is.matrix(vcov)) {
    se <- sqrt(diag(vcov))
  }

  # vcov: function which returns a matrix
  if (is.function(vcov)) {
    args <- c(list(model), vcov_args, dots)
    se <- .safe(sqrt(diag(do.call("vcov", args))))
  }

  # vcov: character (with backward compatibility for `robust = TRUE`)
  if (is.character(vcov) || isTRUE(dots[["robust"]])) {
    .vcov <- insight::get_varcov(
      model,
      vcov = vcov,
      vcov_args = vcov_args,
      verbose = verbose,
      ...
    )
    se <- sqrt(diag(.vcov))
  }

  if (is.null(se)) {
    se <- as.vector(se_standard[, 2])
  }

  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(se_standard)),
    SE = se
  )
}


#' @export
p_value.plm <- p_value.default



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
                                  ci = 0.95,
                                  component = c("conditional", "all"),
                                  exponentiate = FALSE,
                                  p_adjust = NULL,
                                  keep = NULL,
                                  drop = NULL,
                                  verbose = TRUE,
                                  ...) {
  component <- match.arg(component)

  params <- .extract_parameters_generic(
    model,
    merge_by = c("Parameter", "Component"),
    ci = ci,
    component = component,
    p_adjust = p_adjust,
    keep_parameters = keep,
    drop_parameters = drop,
    ...
  )


  # exponentiate coefficients and SE/CI, if requested
  params <- .exponentiate_parameters(params, model, exponentiate)

  params <- .add_model_parameters_attributes(
    params,
    model,
    ci,
    exponentiate,
    p_adjust = p_adjust,
    verbose = verbose,
    ...
  )
  attr(params, "object_name") <- insight::safe_deparse_symbol(substitute(model))
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
ci.pgmm <- function(x, ci = 0.95, dof = Inf, method = NULL, component = "conditional", ...) {
  if (!is.null(method)) {
    method <- tolower(method)
  } else {
    method <- "wald"
  }

  .ci_generic(model = x, ci = ci, dof = dof, method = method, component = component)
}

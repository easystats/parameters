#' @export
model_parameters.nestedLogit <- function(model,
                                         ci = 0.95,
                                         bootstrap = FALSE,
                                         iterations = 1000,
                                         standardize = NULL,
                                         exponentiate = FALSE,
                                         p_adjust = NULL,
                                         summary = getOption("parameters_summary", FALSE),
                                         keep = NULL,
                                         drop = NULL,
                                         vcov = NULL,
                                         vcov_args = NULL,
                                         verbose = TRUE,
                                         ...) {
  dots <- list(...)

  # set default
  if (is.null(ci_method)) {
    if (isTRUE(bootstrap)) {
      ci_method <- "quantile"
    } else if (!is.null(vcov) || !is.null(vcov_args)) {
      ci_method <- "wald"
    } else {
      ci_method <- "profile"
    }
  }

  # profiled CIs may take a long time to compute, so we warn the user about it
  if (insight::n_obs(model) > 1e4 && identical(ci_method, "profile")) {
    insight::format_alert(
      "Profiled confidence intervals may take longer time to compute.",
      "Use `ci_method=\"wald\"` for faster computation of CIs."
    )
  }

  # tell user that profiled CIs don't respect vcov-args
  if (identical(ci_method, "profile") && (!is.null(vcov) || !is.null(vcov_args)) && isTRUE(verbose)) {
    insight::format_alert(
      "When `ci_method=\"profile\"`, `vcov` only modifies standard errors, test-statistic and p-values, but not confidence intervals.",
      "Use `ci_method=\"wald\"` to return confidence intervals based on robust standard errors."
    )
  }

  args <- list(
    model = model,
    ci = ci,
    ci_method = ci_method,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = c("Parameter", "Response"),
    standardize = standardize,
    exponentiate = exponentiate,
    p_adjust = p_adjust,
    summary = summary,
    keep_parameters = keep,
    drop_parameters = drop,
    vcov = vcov,
    vcov_args = vcov_args
  )
  args <- c(args, dots)
  out <- do.call(".model_parameters_generic", args)

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}


#' @export
degrees_of_freedom.nestedLogit <- function(model,
                                           method = NULL,
                                           component = "all",
                                           verbose = TRUE,
                                           ...) {
  if (is.null(method)) {
    method <- "wald"
  }
  if (tolower(method) == "residual") {
    cf <- as.data.frame(stats::coef(model))
    dof <- rep(vapply(model$models, df.residual, numeric(1)), each = nrow(cf))
    if (!is.null(component) && !identical(component, "all")) {
      comp <- intersect(names(dof), component)
      if (!length(comp)) {
        if (verbose) {
          insight::format_alert(
            paste0(
              "No matching model found. Possible values for `component` are ",
              toString(paste0("'", names(model$models), "'")),
              "."
            )
          )
        }
        dof <- Inf
      } else {
        dof <- dof[comp]
      }
    }
  } else {
    dof <- Inf
  }
  dof
}


#' @export
standard_error.nestedLogit <- function(model,
                                       component = "all",
                                       vcov = NULL,
                                       vcov_args = NULL,
                                       verbose = TRUE,
                                       ...) {
  dots <- list(...)
  se <- NULL

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
      component = component,
      vcov = vcov,
      vcov_args = vcov_args,
      verbose = verbose,
      ...
    )
    se <- unlist(lapply(.vcov, function(i) sqrt(diag(i))), use.names = FALSE)
  }

  # classical se from summary()
  if (is.null(se)) {
    se <- as.vector(as.data.frame(do.call(rbind, lapply(x$models, function(i) {
      stats::coef(summary(i))
    })))[, "Std. Error"])
  }

  # classical se from get_varcov()
  if (is.null(se)) {
    .vcov <- insight::get_varcov(
      model,
      component = component,
      verbose = verbose,
      ...
    )
    se <- unlist(lapply(.vcov, function(i) sqrt(diag(i))), use.names = FALSE)
  }

  params <- insight::get_parameters(model, component = component)
  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(se),
    Response = params$Response,
    Component = params$Component
  )
}


#' @export
p_value.nestedLogit <- function(model,
                                dof = NULL,
                                method = NULL,
                                component = "all",
                                vcov = NULL,
                                vcov_args = NULL,
                                verbose = TRUE,
                                ...) {

  if (!is.null(vcov)) {
    return(p_value.default(
      model,
      dof = dof,
      method = method,
      component = component,
      vcov = vcov,
      vcov_args = vcov_args,
      verbose = verbose,
      ...
    ))
  }

  p <- as.vector(as.data.frame(do.call(rbind, lapply(model$models, function(i) {
    stats::coef(summary(i))
  })))[, "Pr(>|z|)"])

  params <- insight::get_parameters(model, component = component)
  .data_frame(
    Parameter = params$Parameter,
    p = p,
    Response = params$Response,
    Component = params$Component
  )
}

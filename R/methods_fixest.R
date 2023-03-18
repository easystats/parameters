# .fixest -----------------------

#' @export
standard_error.fixest <- function(model, vcov = NULL, vcov_args = NULL, ...) {
  params <- insight::get_parameters(model)

  if (!is.null(vcov)) {
    # we don't want to wrap this in a tryCatch because the `fixest` error is
    # informative when `vcov` is wrong.
    V <- insight::get_varcov(model, vcov = vcov, vcov_args = vcov_args)
    SE <- sqrt(diag(V))
  } else {
    stats <- summary(model)
    SE <- as.vector(stats$se)
  }

  .data_frame(
    Parameter = params$Parameter,
    SE = SE
  )
}


#' @export
degrees_of_freedom.fixest <- function(model, method = "wald", ...) {
  # fixest degrees of freedom can be tricky. best to use the function by the
  # package.
  insight::check_if_installed("fixest")
  if (is.null(method)) {
    method <- "wald"
  }
  method <- match.arg(
    tolower(method),
    choices = c("wald", "residual")
  )
  method <- switch(method,
    "wald" = "t",
    "residual" = "resid"
  )
  fixest::degrees_freedom(model, type = method)
}




# .feglm -----------------------

#' @export
standard_error.feglm <- function(model, ...) {
  stats <- stats::coef(summary(model))
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(stats[, "Std. error"])
  )
}

## TODO add ci_method later?

#' @export
p_value.feglm <- function(model, ...) {
  stats <- stats::coef(summary(model))
  params <- insight::get_parameters(model)
  .data_frame(
    Parameter = params$Parameter,
    p = as.vector(stats[, 4])
  )
}




# .fixest_multi -----------------------------------

#' @export
model_parameters.fixest_multi <- function(model,
                                          ci = 0.95,
                                          ci_method = NULL,
                                          bootstrap = FALSE,
                                          iterations = 1000,
                                          standardize = NULL,
                                          exponentiate = FALSE,
                                          p_adjust = NULL,
                                          summary = getOption("parameters_summary", FALSE),
                                          keep = NULL,
                                          drop = NULL,
                                          verbose = TRUE,
                                          vcov = NULL,
                                          vcov_args = NULL,
                                          ...) {
  # iterate over responses
  out <- lapply(
    model,
    model_parameters.default,
    ci = ci,
    ci_method = ci_method,
    bootstrap = bootstrap,
    iterations = iterations,
    standardize = standardize,
    exponentiate = exponentiate,
    p_adjust = p_adjust,
    summary = summary,
    keep = keep,
    drop = drop,
    verbose = verbose,
    vcov = vcov,
    vcov_args = vcov_args,
    ...
  )

  # add response column
  resp <- insight::find_response(model)
  for (i in seq_along(out)) {
    out[[i]]$Response <- resp[[i]]
  }

  # bind lists together to one data frame, save attributes
  att <- attributes(out[[1]])
  params <- do.call(rbind, out)

  attributes(params) <- utils::modifyList(att, attributes(params))
  params
}


#' @export
ci.fixest_multi <- function(x, ...) {
  out <- lapply(x, ci, ...)

  # add response column
  resp <- insight::find_response(x)
  for (i in seq_along(out)) {
    out[[i]]$Response <- resp[[i]]
  }
  out <- do.call(rbind, out)
  row.names(out) <- NULL
  out
}


#' @export
standard_error.fixest_multi <- function(model, ...) {
  out <- lapply(model, standard_error, ...)

  # add response column
  resp <- insight::find_response(model)
  for (i in seq_along(out)) {
    out[[i]]$Response <- resp[[i]]
  }
  out <- do.call(rbind, out)
  row.names(out) <- NULL
  out
}


#' @export
degrees_of_freedom.fixest_multi <- function(model, ...) {
  out <- lapply(model, degrees_of_freedom, ...)

  # add response column
  resp <- insight::find_response(model)
  for (i in seq_along(out)) {
    out[[i]]$Response <- resp[[i]]
  }
  out <- do.call(rbind, out)
  row.names(out) <- NULL
  out
}


#' @export
p_value.fixest_multi <- function(model, ...) {
  out <- lapply(model, p_value, ...)

  # add response column
  resp <- insight::find_response(model)
  for (i in seq_along(out)) {
    out[[i]]$Response <- resp[[i]]
  }
  out <- do.call(rbind, out)
  row.names(out) <- NULL
  out
}


#' @export
simulate_model.fixest_multi <- function(model, ...) {
  lapply(model, simulate_model, ...)
}

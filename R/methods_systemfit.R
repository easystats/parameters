#' @export
model_parameters.systemfit <- function(model,
                                       ci = 0.95,
                                       ci_method = NULL,
                                       bootstrap = FALSE,
                                       iterations = 1000,
                                       standardize = NULL,
                                       exponentiate = FALSE,
                                       p_adjust = NULL,
                                       summary = FALSE,
                                       include_info = getOption("parameters_info", FALSE),
                                       keep = NULL,
                                       drop = NULL,
                                       verbose = TRUE,
                                       ...) {
  ## TODO remove deprecated later
  if (!missing(summary)) {
    .deprecated_warning("summary", "include_info", verbose)
    include_info <- summary
  }

  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    ci_method = ci_method,
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
standard_error.systemfit <- function(model, ...) {
  cf <- stats::coef(summary(model))
  f <- insight::find_formula(model, verbose = FALSE)

  system_names <- names(f)
  parameter_names <- row.names(cf)

  out <- lapply(system_names, function(i) {
    pattern <- paste0("^", i, "_(.*)")
    params <- grepl(pattern, parameter_names)
    data.frame(
      Parameter = gsub(pattern, "\\1", parameter_names[params]),
      SE = as.vector(cf[params, 2]),
      Component = i,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, out)
}


#' @export
p_value.systemfit <- function(model, ...) {
  cf <- stats::coef(summary(model))
  f <- insight::find_formula(model, verbose = FALSE)

  system_names <- names(f)
  parameter_names <- row.names(cf)

  out <- lapply(system_names, function(i) {
    pattern <- paste0("^", i, "_(.*)")
    params <- grepl(pattern, parameter_names)
    data.frame(
      Parameter = gsub(pattern, "\\1", parameter_names[params]),
      p = as.vector(cf[params, 4]),
      Component = i,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, out)
}


#' @export
ci.systemfit <- ci.lm

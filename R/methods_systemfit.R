#' @export
model_parameters.systemfit <- function(model,
                                       ci = .95,
                                       df_method = NULL,
                                       bootstrap = FALSE,
                                       iterations = 1000,
                                       standardize = NULL,
                                       exponentiate = FALSE,
                                       robust = FALSE,
                                       p_adjust = NULL,
                                       summary = FALSE,
                                       verbose = TRUE,
                                       ...) {
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    df_method = df_method,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = c("Parameter", "Component"),
    standardize = standardize,
    exponentiate = exponentiate,
    robust = robust,
    p_adjust = p_adjust,
    summary = summary,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}



#' @export
standard_error.systemfit <- function(model, ...) {
  cf <- stats::coef(summary(model))
  f <- insight::find_formula(model)

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
  f <- insight::find_formula(model)

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
degrees_of_freedom.systemfit <- function(model, ...) {
  df <- c()
  s <- summary(model)$eq
  params <- insight::find_parameters(model)
  f <- insight::find_formula(model)
  system_names <- names(f)

  for (i in 1:length(system_names)) {
    dfs <- rep(s[[i]]$df[2], length(params[[i]]))
    df_names <- rep(names(params[i]), length(params[[i]]))
    df <- c(df, stats::setNames(dfs, df_names))
  }

  df
}



#' @export
ci.systemfit <- ci.lm

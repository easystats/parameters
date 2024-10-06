#' @export
model_parameters.mblogit <- function(model,
                                     ci = 0.95,
                                     bootstrap = FALSE,
                                     iterations = 1000,
                                     standardize = NULL,
                                     exponentiate = FALSE,
                                     p_adjust = NULL,
                                     summary = getOption("parameters_summary", FALSE),
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
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = c("Parameter", "Response"),
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
standard_error.mblogit <- function(model, ...) {
  s <- stats::coef(summary(model))
  out <- data.frame(
    Parameter = gsub("(.*)~(.*)", "\\2", rownames(s)),
    SE = unname(s[, "Std. Error"]),
    Response = gsub("(.*)~(.*)", "\\1", rownames(s)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' @export
p_value.mblogit <- function(model, ...) {
  s <- stats::coef(summary(model))
  out <- data.frame(
    Parameter = gsub("(.*)~(.*)", "\\2", rownames(s)),
    p = unname(s[, "Pr(>|z|)"]),
    Response = gsub("(.*)~(.*)", "\\1", rownames(s)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' @export
simulate_parameters.mblogit <- simulate_parameters.multinom

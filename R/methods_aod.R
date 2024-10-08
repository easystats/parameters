# classes: .glimML


## TODO add ci_method later?


#################### .glimML ------


#' @rdname model_parameters.averaging
#' @export
model_parameters.glimML <- function(model,
                                    ci = 0.95,
                                    bootstrap = FALSE,
                                    iterations = 1000,
                                    component = c("conditional", "random", "dispersion", "all"),
                                    standardize = NULL,
                                    exponentiate = FALSE,
                                    p_adjust = NULL,
                                    summary = getOption("parameters_summary", FALSE),
                                    include_info = getOption("parameters_info", FALSE),
                                    keep = NULL,
                                    drop = NULL,
                                    verbose = TRUE,
                                    ...) {
  component <- match.arg(component)
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

  # dispersion is just an alias...
  if (component == "dispersion") {
    component <- "random"
  }

  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    component = component,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = "Parameter",
    standardize = standardize,
    exponentiate = exponentiate,
    p_adjust = p_adjust,
    keep_parameters = keep,
    drop_parameters = drop,
    include_info = include_info,
    verbose = verbose,
    ...
  )

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}


#' @export
standard_error.glimML <- function(model, ...) {
  insight::check_if_installed("aod")

  s <- methods::slot(aod::summary(model), "Coef")
  se <- s[, 2]

  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(s)),
    SE = as.vector(se)
  )
}


#' @export
p_value.glimML <- function(model, ...) {
  insight::check_if_installed("aod")

  s <- methods::slot(aod::summary(model), "Coef")
  p <- s[, 4]

  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(s)),
    p = as.vector(p)
  )
}

############# .complmrob --------------


#' @export
standard_error.complmrob <- function(model, ...) {
  stats <- summary(model)$stats
  params <- insight::get_parameters(model)
  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(stats[, "Std. Error"])
  )
}


#' @export
p_value.complmrob <- p_value.default


#' @export
ci.complmrob <- ci.default


#' @export
degrees_of_freedom.complmrob <- function(model, method = "wald", ...) {
  .degrees_of_freedom_no_dfresid_method(model, method)
}





############# .Gam --------------


#' @rdname model_parameters.cgam
#' @inheritParams model_parameters.aov
#' @export
model_parameters.Gam <- function(model,
                                 es_type = NULL,
                                 df_error = NULL,
                                 type = NULL,
                                 table_wide = FALSE,
                                 verbose = TRUE,
                                 effectsize_type = NULL,
                                 ...) {
  ## TODO: remove deprecation warning later
  if (!is.null(effectsize_type)) {
    insight::format_warning("Argument `effectsize_type` is deprecated. Use `es_type` instead.")
    es_type <- effectsize_type
  }

  model_parameters(
    summary(model)$parametric.anova,
    es_type = es_type,
    df_error = df_error,
    type = type,
    table_wide = table_wide,
    verbose = verbose,
    ...
  )
}


#' @export
p_value.Gam <- function(model, ...) {
  p.aov <- stats::na.omit(summary(model)$parametric.anova)
  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(p.aov)),
    p = as.vector(p.aov[, 5])
  )
}

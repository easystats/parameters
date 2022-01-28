#' @export
model_parameters.bfsl <- function(model,
                                  ci = .95,
                                  ci_method = "residual",
                                  p_adjust = NULL,
                                  verbose = TRUE,
                                  ...) {
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    ci_method = ci_method,
    merge_by = "Parameter",
    p_adjust = p_adjust,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}


#' @export
standard_error.bfsl <- function(model, ...) {
  cf <- stats::coef(model)

  params <- data.frame(
    Parameter = rownames(cf),
    SE = unname(cf[, "Std. Error"]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  insight::text_remove_backticks(params)
}



#' @export
degrees_of_freedom.bfsl <- function(model, method = "residual", ...) {
  if (is.null(method)) {
    method <- "wald"
  }
  method <- match.arg(tolower(method), choices = c("analytical", "any", "fit", "wald", "residual", "normal"))

  if (method %in% c("wald", "residual", "fit")) {
    model$df.residual
  } else {
    degrees_of_freedom.default(model, method = method, ...)
  }
}

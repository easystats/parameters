# .varest

#' @export
model_parameters.varest <- function(model,
                                    ci = .95,
                                    bootstrap = FALSE,
                                    iterations = 1000,
                                    standardize = NULL,
                                    exponentiate = FALSE,
                                    robust = FALSE,
                                    p_adjust = NULL,
                                    verbose = TRUE,
                                    ...) {
  params <- lapply(names(model$varresult), function(i) {
    out <- model_parameters(model = model$varresult[[i]], ci = ci,
                            bootstrap = bootstrap, iterations = iterations,
                            standardize = standardize, exponentiate = exponentiate,
                            robust = robust, p_adjust = p_adjust, verbose = verbose,
                            ...)
    out$Group <- i
    out
  })

  params <- do.call(rbind, params)
  attr(params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  params
}



#' @export
ci.varest <- function(x, ci = .95, method = NULL, ...) {
  params <- lapply(names(model$varresult), function(i) {
    out <- ci(x = model$varresult[[i]], ci = ci, method = method, ...)
    out$Group <- i
    out
  })

  do.call(rbind, params)
}



#' @export
standard_error.varest <- function(model, method = NULL, ...) {
  params <- lapply(names(model$varresult), function(i) {
    out <- standard_error(model = model$varresult[[i]], method = method, ...)
    out$Group <- i
    out
  })

  do.call(rbind, params)
}



#' @export
p_value.varest <- function(model, ...) {
  params <- lapply(names(model$varresult), function(i) {
    out <- p_value(model = model$varresult[[i]], ...)
    out$Group <- i
    out
  })

  do.call(rbind, params)
}

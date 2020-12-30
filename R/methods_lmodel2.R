# lmodel2


#' @export
model_parameters.lmodel2 <- function(model,
                                     exponentiate = FALSE,
                                     p_adjust = NULL,
                                     verbose = TRUE,
                                     ...) {
  out <- .model_parameters_generic(
    model = model,
    ci = .95,
    bootstrap = FALSE,
    iterations = 10,
    merge_by = c("Parameter", "Component"),
    standardize = NULL,
    exponentiate = exponentiate,
    robust = FALSE,
    p_adjust = p_adjust,
    verbose = verbose,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}


#' @export
standard_error.lmodel2 <- function(model, ...) {
  NULL
}


#' @importFrom utils stack
#' @export
p_value.lmodel2 <- function(model, ...) {
  res <- model$regression.results
  data.frame(
    Parameter = rep(c("Intercept", "Slope"), each = nrow(res)),
    p = utils::stack(res, select = 5)[[1]],
    Component = rep(res$Method, 2),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}


#' @importFrom utils stack
#' @export
ci.lmodel2 <- function(x, ...) {
  res <- x$confidence.intervals
  data.frame(
    Parameter = rep(c("Intercept", "Slope"), each = nrow(res)),
    CI = 95,
    CI_low = utils::stack(res, select = c(2, 4))[[1]],
    CI_high = utils::stack(res, select = c(3, 5))[[1]],
    Component = rep(res$Method, 2),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

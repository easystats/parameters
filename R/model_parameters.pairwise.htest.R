#' @rdname model_parameters.htest
#' @export
model_parameters.pairwise.htest <- function(model, ...) {
  m <- model$p.value
  parameters <-
    data.frame(
      group1 = rep(rownames(m), each = ncol(m)),
      group2 = rep(colnames(m), times = nrow(m)),
      p.value = as.numeric(t(m)),
      stringsAsFactors = FALSE
    )

  parameters <- stats::na.omit(parameters)

  parameters <- .add_htest_attributes(parameters, model, p_adjust = model$p.adjust.method)
  class(parameters) <- c("parameters_model", class(parameters))
  parameters
}







#' @keywords internal
.add_htest_attributes <- function(params, model, p_adjust = NULL, ...) {
  dot.arguments <- lapply(match.call(expand.dots = FALSE)$`...`, function(x) x)

  attr(params, "p_adjust") <- p_adjust
  attr(params, "model_class") <- class(model)

  if ("digits" %in% names(dot.arguments)) {
    attr(params, "digits") <- eval(dot.arguments[["digits"]])
  } else {
    attr(params, "digits") <- 2
  }

  if ("ci_digits" %in% names(dot.arguments)) {
    attr(params, "ci_digits") <- eval(dot.arguments[["ci_digits"]])
  } else {
    attr(params, "ci_digits") <- 2
  }

  if ("p_digits" %in% names(dot.arguments)) {
    attr(params, "p_digits") <- eval(dot.arguments[["p_digits"]])
  } else {
    attr(params, "p_digits") <- 3
  }

  if ("s_value" %in% names(dot.arguments)) {
    attr(params, "s_value") <- eval(dot.arguments[["s_value"]])
  }

  params
}

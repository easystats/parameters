# x <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
# model <- marginaleffects(x, newdata = insight::get_datagrid(x, at = "Species"), variables = "Petal.Length")

# model_parameters ----------------

#' @rdname model_parameters.averaging
#' @export
model_parameters.marginaleffects <- function(model,
                                             ci = .95,
                                             ...) {

  out <- insight::standardize_names(
    marginaleffects::tidy(model, conf_level = ci, ...),
    style = "easystats"
  )

  out <- tryCatch(
    .add_model_parameters_attributes(out, model, ci, ...),
    error = function(e) out)

  attr(out, "object_name") <- insight::safe_deparse(substitute(model))

  if (inherits(model, "marginalmeans")) {
    attr(out, "coefficient_name") <- "Marginal Means"
  } else if (inherits(model, "comparisons")) {
    attr(out, "coefficient_name") <- "Contrast"
  } else if (inherits(model, "marginaleffects")) {
    attr(out, "coefficient_name") <- "Slope"
  }

  class(out) <- c("parameters_model", "see_parameters_model", class(out))
  out
}

#' @rdname model_parameters.averaging
#' @export
model_parameters.comparisons <- model_parameters.marginaleffects


#' @rdname model_parameters.averaging
#' @export
model_parameters.marginalmeans <- model_parameters.marginaleffects


#' @rdname model_parameters.averaging
#' @export
model_parameters.deltamethod <- model_parameters.marginaleffects

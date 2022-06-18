# x <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
# model <- marginaleffects(x, newdata = insight::get_datagrid(x, at = "Species"), variables = "Petal.Length")

# model_parameters ----------------

#' @rdname model_parameters.averaging
#' @export
model_parameters.marginaleffects <- function(model,
                                             ci = .95,
                                             ...) {

  # Convert to dataframe and rename some columns
  replacement <- c(
        "type" = "Type",
        "term" = "Parameter",
        "estimate" = "Coefficient",
        "std.error" = "SE",
        "statistic" = "z",
        "p.value" = "p",
        "value" = "value",
        "conf.low" = "CI_low",
        "conf.high" = "CI_high")
    
  out <- marginaleffects::tidy(model, conf_level = ci, ...)

  out <- datawizard::data_rename(
    out,
    pattern = names(replacement),
    replacement = replacement)

  out <- tryCatch(
    .add_model_parameters_attributes(out, model, ci, ...),
    error = function(e) out)

  attr(out, "object_name") <- insight::safe_deparse(substitute(model))

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

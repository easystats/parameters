# x <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
# model <- marginaleffects(x, newdata = insight::get_datagrid(x, at = "Species"), variables = "Petal.Length")

# model_parameters ----------------

#' @export
model_parameters.marginaleffects <- function(model,
                                             ci = .95,
                                             ...) {
  # Extract model and original data
  mod <- attributes(model)$model
  ori_data <- insight::get_data(mod)

  # Convert to dataframe and rename some columns
  out <- datawizard::data_rename(as.data.frame(model),
                                 pattern = c("type", "term", "dydx", "std.error"),
                                 replacement = c("Type", "Term", "Coefficient", "SE"))


  if("posterior_draws" %in% names(attributes(model))) {
    # ---- if Bayesian ----
    # Remove point-estimates (going to recompute them anyway)
    out <- datawizard::data_remove(out, c("Coefficient", "SE", "conf.low", "conf.high"))
    draws <- data.frame(t(attributes(model)$posterior_draws))
    draws <- bayestestR::describe_posterior(draws, ci = ci, ...)
    out <- cbind(out, datawizard::data_remove(draws, "Parameter"))

  } else {
    # ---- if Frequentist ----
    # Add CI
    if(all(c("Coefficient", "SE") %in% names(out))) {
      out$CI_low <- out$Coefficient + qnorm((1 - ci) / 2) * out$SE
      out$CI_high <- out$Coefficient - qnorm((1 - ci) / 2) * out$SE
    }
  }

  # Move columns from original data at the beginning
  out <- datawizard::data_relocate(out, names(ori_data), before = 1)

  out <- suppressWarnings(.add_model_parameters_attributes(out, model, ci, ...))
  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  attr(out, "parameter_names") <- names(out)[names(out) %in% names(ori_data)]

  class(out) <- c("parameters_model", "see_parameters_model", class(out))
  out
}
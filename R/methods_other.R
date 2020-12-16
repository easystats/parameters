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
p_value.complmrob <- function(model, ...) {
  stats <- summary(model)$stats
  params <- insight::get_parameters(model)
  .data_frame(
    Parameter = params$Parameter,
    p = as.vector(stats[, "Pr(>|t|)"])
  )
}


#' @export
ci.comlmrob <- ci.default




############# .Gam --------------


#' @export
model_parameters.Gam <- function(model,
                                 omega_squared = NULL,
                                 eta_squared = NULL,
                                 epsilon_squared = NULL,
                                 df_error = NULL,
                                 type = NULL,
                                 verbose = TRUE,
                                 ...) {
  model_parameters(
    summary(model)$parametric.anova,
    omega_squared = omega_squared,
    eta_squared = eta_squared,
    epsilon_squared = epsilon_squared,
    df_error = df_error,
    type = type,
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

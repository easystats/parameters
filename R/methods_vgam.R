# classes: .vglm, .vgam


########### .vgam ---------------


#' @export
model_parameters.vgam <- model_parameters.gam


#' @export
standard_error.vgam <- function(model, ...) {
  params <- insight::get_parameters(model)
  se <- sqrt(diag(insight::get_varcov(model)))
  # sort
  se <- se[params$Parameter]
  .data_frame(
    Parameter = .remove_backticks_from_string(names(se)),
    SE = as.vector(se),
    Component = params$Component
  )
}


#' @export
degrees_of_freedom.vgam <- function(model, ...) {
  params <- insight::get_parameters(model)
  out <- setNames(rep(NA, nrow(params)), params$Parameter)
  out[names(model@nl.df)] <- model@nl.df
  out
}


#' @export
p_value.vgam <- function(model, ...) {
  stat <- insight::get_statistic(model)
  stat$p <- as.vector(stats::pchisq(stat$Statistic, df = degrees_of_freedom(model), lower.tail = FALSE))

  stat[c("Parameter", "p", "Component")]
}


#' @export
simulate_model.vgam <- function(model, iterations = 1000, ...) {
  out <- .simulate_model(model, iterations, component = "all")
  class(out) <- c("parameters_simulate_model", class(out))
  out
}




########### .vglm ---------------


#' @export
ci.vglm <- ci.tobit


#' @export
p_value.vglm <- function(model, ...) {
  if (!requireNamespace("VGAM", quietly = TRUE)) {
    stop("Package `VGAM` required.", call. = FALSE)
  }

  cs <- VGAM::summary(model)@coef3
  p <- cs[, 4]

  .data_frame(
    Parameter = .remove_backticks_from_string(names(p)),
    p = as.vector(p)
  )
}


#' @export
standard_error.vglm <- function(model, ...) {
  se <- sqrt(diag(insight::get_varcov(model)))
  .data_frame(
    Parameter = .remove_backticks_from_string(names(se)),
    SE = as.vector(se)
  )
}





# ci.vgam <- function(x, ci = .95, component = c("all", "conditional", "smooth"), ...) {
#   component <- match.arg(component)
#
#   # dof and SE
#   dof <- degrees_of_freedom(x)
#   se <- standard_error(x)$SE
#   params <- insight::get_parameters(x)
#
#   se <- se[!is.na(dof)]
#   dof <- dof[!is.na(dof)]
#   params_names <- names(dof)
#
#   # Wald CI for non-chisq parameters
#   out <- ci_wald(model = x, ci = ci, dof = Inf)
#
#   chisq_fac <- stats::qchisq(se, df = dof, lower.tail = FALSE)
#   for (i in 1:length(params_names)) {
#     out$CI_low[out$Parameter == params_names[i]] <- params$Estimate[params$Parameter == params_names[i]] - se[i] * chisq_fac[i]
#     out$CI_high[out$Parameter == params_names[i]] <- params$Estimate[params$Parameter == params_names[i]] + se[i] * chisq_fac[i]
#   }
#
#   out
# }

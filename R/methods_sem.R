#' @export
model_parameters.sem <- model_parameters.default


#' @export
standard_error.sem <- function(model, ...) {
  if (!.is_semLme(model)) {
    return(NULL)
  }
  if (is.null(model$se)) {
    insight::format_alert(
      "Model has no standard errors. Please fit model again with bootstrapped standard errors."
    )
    return(NULL)
  }
  .data_frame(
    Parameter = names(model$se),
    SE = unname(model$se)
  )
}


#' @export
p_value.sem <- function(model, ...) {
  if (!.is_semLme(model)) {
    return(NULL)
  }

  stat <- insight::get_statistic(model)
  if (is.null(stat)) {
    return(NULL)
  }

  .data_frame(
    Parameter = stat$Parameter,
    p = 2 * stats::pnorm(abs(stat$Statistic), lower.tail = FALSE)
  )
}

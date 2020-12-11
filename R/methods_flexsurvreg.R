
#' @export
ci.flexsurvreg <- ci.gamlss


#' @export
standard_error.flexsurvreg <- function(model, ...) {
  params <- insight::find_parameters(model, flatten = TRUE)
  se <- model$res[rownames(model$res) %in% params, "se"]

  .data_frame(
    Parameter = .remove_backticks_from_string(names(se)),
    SE = as.vector(se)
  )
}


#' @export
p_value.flexsurvreg <- function(model, ...) {
  params <- insight::get_parameters(model)
  est <- params$Estimate
  se <- standard_error(model)$SE
  p <- 2 * stats::pt(abs(est / se), df = degrees_of_freedom(model, method = "any"), lower.tail = FALSE)
  .data_frame(
    Parameter = params$Parameter,
    p = as.vector(p)
  )
}

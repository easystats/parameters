
#' @export
ci.biglm <- function(x, ci = .95, ...) {
  out <- lapply(ci, function(i) {
    ci_list <- stats::confint(x, level = i, ...)
    .data_frame(
      Parameter = rownames(ci_list),
      CI = i,
      CI_low = as.vector(ci_list[, 1]),
      CI_high = as.vector(ci_list[, 2])
    )
  })
  .remove_backticks_from_parameter_names(do.call(rbind, out))
}


#' @export
standard_error.biglm <- function(model, ...) {
  cs <- summary(model)$mat
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(cs[, 4])
  )
}


#' @export
p_value.biglm <- function(model, ...) {
  cs <- summary(model)$mat
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    p = as.vector(cs[, 5])
  )
}

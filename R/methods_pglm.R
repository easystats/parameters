#' @export
p_value.pglm <- function(model, ...) {
  p <- summary(model)$estimate[, 4]

  .data_frame(
    Parameter = .remove_backticks_from_string(names(p)),
    p = as.vector(p)
  )
}


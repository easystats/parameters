
#' @export
standard_error.bife <- function(model, ...) {
  cs <- summary(model)
  se <- cs$cm[, 2]
  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(cs$cm)),
    SE = as.vector(se)
  )
}


#' @export
p_value.bife <- function(model, ...) {
  cs <- summary(model)
  p <- cs$cm[, 4]
  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(cs$cm)),
    p = as.vector(p)
  )
}

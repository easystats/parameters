
#' @export
ci.plm <- ci.tobit


#' @export
standard_error.plm <- function(model, ...) {
  se <- stats::coef(summary(model))

  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(se)),
    SE = as.vector(se[, 2])
  )
}


#' @export
p_value.plm <- function(model, ...) {
  p <- stats::coef(summary(model))

  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(p)),
    p = as.vector(p[, 4])
  )
}


#' @export
p_value.pggls <- function(model, ...) {
  cs <- summary(model)$CoefTable
  p <- cs[, 4]
  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(cs)),
    p = as.vector(p)
  )
}

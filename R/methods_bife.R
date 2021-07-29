
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

#' @export

model_parameters.bifeAPEs <- function(x, ...) {

  est <- x[["delta"]]
  se <- sqrt(diag(x[["vcov"]]))
  z <- est/se
  p <- 2 * pnorm(-abs(z))
  nms <- names(est)

  out <- data.frame(nms, est, se, z, p)
  colnames(out) <- c("Parameter", "Coefficient", "Std. error", "z value", "p")
  rownames(out) <- NULL
  out <- as.data.frame(out)

  class(out) <- c("parameters_model", "see_parameters_model", class(out))
  out
}
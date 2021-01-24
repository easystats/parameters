
#' @export
standard_error.gmnl <- function(model, ...) {
  cs <- summary(model)$CoefTable
  se <- cs[, 2]

  pv <- .data_frame(
    Parameter = .remove_backticks_from_string(names(se)),
    SE = as.vector(se)
  )

  # rename intercepts
  intercepts <- grepl(":(intercept)", pv$Parameter, fixed = TRUE)
  pv$Parameter[intercepts] <- sprintf(
    "(Intercept: %s)",
    sub(":(intercept)", replacement = "", pv$Parameter[intercepts], fixed = TRUE)
  )

  pv
}


#' @export
p_value.gmnl <- function(model, ...) {
  cs <- summary(model)$CoefTable
  p <- cs[, 4]
  # se <- cs[, 2]

  pv <- .data_frame(
    Parameter = .remove_backticks_from_string(rownames(cs)),
    p = as.vector(p)
  )

  # rename intercepts
  intercepts <- grepl(":(intercept)", pv$Parameter, fixed = TRUE)
  pv$Parameter[intercepts] <- sprintf(
    "(Intercept: %s)",
    sub(":(intercept)", replacement = "", pv$Parameter[intercepts], fixed = TRUE)
  )

  pv
}

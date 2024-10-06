## from rms / rmsb package


# model parameters -------------


#' @export
model_parameters.blrm <- model_parameters.bayesQR




# standard error -------------


#' @export
standard_error.lrm <- function(model, ...) {
  se <- sqrt(diag(stats::vcov(model)))

  # psm-models returns vcov-matrix w/o dimnames
  if (is.null(names(se))) names(se) <- names(stats::coef(model))

  .data_frame(
    Parameter = .remove_backticks_from_string(names(se)),
    SE = as.vector(se)
  )
}

#' @export
standard_error.ols <- standard_error.lrm

#' @export
standard_error.rms <- standard_error.lrm

#' @export
standard_error.psm <- standard_error.lrm




# p-values -----------------------

#' @export
p_value.lrm <- function(model, ...) {
  stat <- insight::get_statistic(model)

  # Issue: 697: typically the degrees of freedom are the same for every
  # observation, but the value is repeated. This poses problems in multiple
  # imputation models with Hmisc when we get more df values than parameters.
  dof <- insight::get_df(model, type = "wald")
  dfu <- unique(dof)
  if (length(dfu) == 1) {
    dof <- dfu
  }

  p <- 2 * stats::pt(abs(stat$Statistic), df = dof, lower.tail = FALSE)

  .data_frame(
    Parameter = .remove_backticks_from_string(stat$Parameter),
    p = as.vector(p)
  )
}

#' @export
p_value.ols <- p_value.lrm

#' @export
p_value.rms <- p_value.lrm

#' @export
p_value.psm <- p_value.lrm

#' @export
p_value.blrm <- p_value.BFBayesFactor

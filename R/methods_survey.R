

# simulate_model -----------------------------------------

#' @export
simulate_model.svyglm.nb <- simulate_model.default


#' @export
simulate_model.svyglm.zip <- simulate_model.default


# standard erors -----------------------------------------

#' @export
standard_error.svyglm.nb <- function(model, ...) {
  if (!isNamespaceLoaded("survey")) {
    requireNamespace("survey", quietly = TRUE)
  }
  se <- sqrt(diag(stats::vcov(model, stderr = "robust")))
  .data_frame(
    Parameter = .remove_backticks_from_string(names(se)),
    SE = as.vector(se)
  )
}


#' @export
standard_error.svyglm.zip <- standard_error.svyglm.nb


#' @export
standard_error.svyglm <- function(model, ...) {
  vc <- insight::get_varcov(model)
  .data_frame(
    Parameter = .remove_backticks_from_string(row.names(vc)),
    SE = as.vector(sqrt(diag(vc)))
  )
}


#' @export
standard_error.svyolr <- standard_error.svyglm



# confidence intervals -----------------------------------

#' @export
ci.svyglm.nb <- ci.tobit

#' @export
ci.svyglm.glimML <- ci.tobit

#' @export
ci.svyglm.zip <- ci.tobit



# p values -----------------------------------------------

#' @export
p_value.svyglm <- function(model, verbose = TRUE, ...) {
  statistic <- insight::get_statistic(model)
  df <- insight::get_df(model, type = "residual")
  p <- 2 * stats::pt(-abs(statistic$Statistic), df = df)
  .data_frame(
    Parameter = statistic$Parameter,
    p = as.vector(p)
  )
}


#' @export
p_value.svyolr <- p_value.svyglm


#' @export
p_value.svyglm.nb <- function(model, ...) {
  if (!isNamespaceLoaded("survey")) {
    requireNamespace("survey", quietly = TRUE)
  }

  est <- stats::coef(model)
  se <- sqrt(diag(stats::vcov(model, stderr = "robust")))
  p <- 2 * stats::pt(abs(est / se), df = degrees_of_freedom(model, method = "any"), lower.tail = FALSE)

  .data_frame(
    Parameter = .remove_backticks_from_string(names(p)),
    p = as.vector(p)
  )
}


#' @export
p_value.svyglm.zip <- p_value.svyglm.nb



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
  cs <- stats::coef(summary(model))
  se <- cs[, 2]
  .data_frame(
    Parameter = .remove_backticks_from_string(names(se)),
    SE = as.vector(se)
  )
}


# confidence intervals -----------------------------------

#' @export
ci.svyglm.nb <- ci.tobit

#' @export
ci.svyglm.glimML <- ci.tobit

#' @export
ci.svyglm.zip <- ci.tobit


# p values -----------------------------------------------

#' @export
p_value.svyglm <- function(model, ...) {
  cs <- stats::coef(summary(model))
  p <- cs[, 4]

  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(cs)),
    p = as.vector(p)
  )
}


#' @export
p_value.svyolr <- function(model, ...) {
  cs <- stats::coef(summary(model))
  p <- 2 * stats::pt(abs(cs[, 3]), df = degrees_of_freedom(model, method = "any"), lower.tail = FALSE)

  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(cs)),
    p = as.vector(p)
  )
}


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

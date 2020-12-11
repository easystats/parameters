# standard errors --------------------------------------


#' @export
standard_error.metaplus <- function(model, ...) {
  ci_low <- as.vector(model$results[, "95% ci.lb"])
  ci_high <- as.vector(model$results[, "95% ci.ub"])
  cis <- apply(cbind(ci_low, ci_high), MARGIN = 1, diff)

  out <- .data_frame(
    Parameter = .remove_backticks_from_string(rownames(model$results)),
    SE = cis / (2 * stats::qnorm(.975))
  )

  out$Parameter[grepl("muhat", out$Parameter)] <- "(Intercept)"
  out
}


#' @export
standard_error.meta_random <- function(model, ...) {
  params <- as.data.frame(model$estimates)
  out <- data.frame(
    Parameter = .remove_backticks_from_string(rownames(params)),
    SE = params$sd,
    stringsAsFactors = FALSE
  )
  out$Parameter[grepl("d", out$Parameter)] <- "(Intercept)"
  out
}

#' @export
standard_error.meta_fixed <- standard_error.meta_random

#' @export
standard_error.meta_bma <- standard_error.meta_random


# p values --------------------------------------------

#' @export
p_value.metaplus <- function(model, ...) {
  out <- .data_frame(
    Parameter = .remove_backticks_from_string(rownames(model$results)),
    p = as.vector(model$results[, "pvalue"])
  )
  out$Parameter[grepl("muhat", out$Parameter)] <- "(Intercept)"
  out
}


# format_parameters -----------------------------------

#' @importFrom utils packageVersion
#' @export
format_parameters.meta_random <- function(model, ...) {
  params <- insight::find_parameters(model, flatten = TRUE)
  names(params) <- params
  params
}


#' @export
format_parameters.meta_fixed <- format_parameters.meta_random


#' @export
format_parameters.meta_bma <- format_parameters.meta_random


# confidence intervals -------------------------------

#' @export
ci.metaplus <- function(x, ...) {
  out <- .data_frame(
    Parameter = .remove_backticks_from_string(rownames(x$results)),
    CI_low = as.vector(x$results[, "95% ci.lb"]),
    CI_high = as.vector(x$results[, "95% ci.ub"])
  )

  out$Parameter[grepl("muhat", out$Parameter)] <- "(Intercept)"
  out
}


#' @export
ci.meta_random <- function(x, method = "hdi", ...) {
  # process arguments
  params <- as.data.frame(x$estimates)
  ci_method <- match.arg(method, choices = c("hdi", "eti"))

  # extract ci-level and find ci-columns
  ci <- .meta_bma_extract_ci(params)
  ci_cols <- .metabma_ci_columns(ci_method, ci)

  out <- data.frame(
    Parameter = rownames(params),
    CI = .95,
    CI_low = params[[ci_cols[1]]],
    CI_high = params[[ci_cols[2]]],
    stringsAsFactors = FALSE
  )

  out$Parameter[grepl("d", out$Parameter)] <- "(Intercept)"
  out
}

#' @export
ci.meta_fixed <- ci.meta_random

#' @export
ci.meta_bma <- ci.meta_random



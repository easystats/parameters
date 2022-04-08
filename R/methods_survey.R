# model_parameters -----------------------------------------

#' @export
model_parameters.svyglm <- function(model,
                                    ci = .95,
                                    ci_method = "wald",
                                    bootstrap = FALSE,
                                    iterations = 1000,
                                    standardize = NULL,
                                    exponentiate = FALSE,
                                    p_adjust = NULL,
                                    verbose = TRUE,
                                    ...) {
  if (insight::n_obs(model) > 1e4 && ci_method == "likelihood") {
    message(insight::format_message("Likelihood confidence intervals may take longer time to compute. Use 'ci_method=\"wald\"' for faster computation of CIs."))
  }

  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    ci_method = ci_method,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = "Parameter",
    standardize = standardize,
    exponentiate = exponentiate,
    p_adjust = p_adjust,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}



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
ci.svyglm <- function(x, ci = .95, method = "wald", ...) {
  method <- match.arg(method, choices = c("wald", "residual", "normal", "likelihood"))
  if (method == "likelihood") {
    out <- lapply(ci, function(i) .ci_likelihood(model = x, ci = i))
    out <- do.call(rbind, out)
  } else {
    out <- .ci_generic(model = x, ci = ci, method = method, ...)
  }

  row.names(out) <- NULL
  out
}

#' @export
ci.svyolr <- ci.svyglm



# p values -----------------------------------------------

## TODO how to calculate p when ci-method is "likelihood"?

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




# helper --------------------

.ci_likelihood <- function(model, ci) {
  glm_ci <- tryCatch(
    {
      out <- as.data.frame(stats::confint(model, level = ci, method = "likelihood"), stringsAsFactors = FALSE)
      names(out) <- c("CI_low", "CI_high")

      out$CI <- ci
      out$Parameter <- insight::get_parameters(model, effects = "fixed", component = "conditional")$Parameter

      out <- out[c("Parameter", "CI", "CI_low", "CI_high")]
      rownames(out) <- NULL

      out
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(glm_ci)) {
    glm_ci <- .ci_generic(model, ci = ci)
  }

  glm_ci
}

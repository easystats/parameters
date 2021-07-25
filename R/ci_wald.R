#' @rdname p_value_wald
#'
#' @param ci Confidence Interval (CI) level. Default to `0.95` (`95%`).
#' @param dof Degrees of Freedom. If not specified, for `ci_wald()`, defaults to
#'   model's residual degrees of freedom (i.e. `n-k`, where `n` is the number of
#'   observations and `k` is the number of parameters). For `p_value_wald()`,
#'   defaults to `Inf`.
#'
#' @inheritParams simulate_model
#' @inheritParams standard_error
#' @inheritParams model_parameters.default
#'
#' @export
ci_wald <- function(model,
                    ci = .95,
                    dof = NULL,
                    effects = c("fixed", "random", "all"),
                    component = c("all", "conditional", "zi", "zero_inflated", "dispersion", "precision", "scale", "smooth_terms", "full", "marginal"),
                    robust = FALSE,
                    ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)
  out <- lapply(ci, function(i) {
    .ci_wald(
      model = model,
      ci = i,
      dof = dof,
      effects = effects,
      component = component,
      robust = robust,
      method = "wald",
      ...
    )
  })
  out <- do.call(rbind, out)
  row.names(out) <- NULL
  out
}


#' @keywords internal
.ci_wald <- function(model,
                     ci,
                     dof,
                     effects,
                     component,
                     robust = FALSE,
                     method = "wald",
                     se = NULL,
                     ...) {
  if (inherits(model, "emmGrid")) {
    params <- insight::get_parameters(
      model,
      effects = effects,
      component = component,
      merge_parameters = TRUE
    )
  } else {
    params <- insight::get_parameters(model,
      effects = effects,
      component = component,
      verbose = FALSE
    )
  }

  # check if all estimates are non-NA
  params <- .check_rank_deficiency(params, verbose = FALSE)

  method <- tolower(method)

  # if we have adjusted SE, e.g. from kenward-roger, don't recompute
  # standard errors to save time...
  if (is.null(se)) {
    stderror <- if (isTRUE(robust)) {
      standard_error_robust(model, component = component, ...)
    } else {
      switch(method,
        "wald" = standard_error(model, component = component),
        "kenward" = ,
        "kr" = se_kenward(model),
        "ml1" = se_ml1(model),
        "betwithin" = se_betwithin(model),
        "satterthwaite" = se_satterthwaite(model),
        standard_error(model, component = component)
      )
    }

    if (.is_empty_object(stderror)) {
      return(NULL)
    }

    # filter non-matching parameters
    if (nrow(stderror) != nrow(params)) {
      params <- stderror <- merge(stderror, params, sort = FALSE)
    }
    se <- stderror$SE
  }

  if (is.null(dof)) {
    # residual df
    dof <- degrees_of_freedom(model, method = "any")
    # make sure we have a value for degrees of freedom
    if (is.null(dof) || length(dof) == 0) {
      dof <- Inf
    } else if (length(dof) > nrow(params)) {
      # filter non-matching parameters
      dof <- dof[1:nrow(params)]
    }
  }

  alpha <- (1 + ci) / 2
  fac <- suppressWarnings(stats::qt(alpha, df = dof))
  out <- cbind(
    CI_low = params$Estimate - se * fac,
    CI_high = params$Estimate + se * fac
  )

  out <- as.data.frame(out)
  out$CI <- ci
  out$Parameter <- params$Parameter

  out <- out[c("Parameter", "CI", "CI_low", "CI_high")]
  if ("Component" %in% names(params)) out$Component <- params$Component
  if ("Effects" %in% names(params) && effects != "fixed") out$Effects <- params$Effects
  if ("Response" %in% names(params)) out$Response <- params$Response

  if (anyNA(params$Estimate)) {
    out[stats::complete.cases(out), ]
  } else {
    out
  }
}

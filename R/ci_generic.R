.ci_generic <- function(model,
                        ci = .95,
                        method = "wald",
                        dof = NULL,
                        effects = c("fixed", "random", "all"),
                        component = c("all", "conditional", "zi", "zero_inflated", "dispersion", "precision", "scale", "smooth_terms", "full", "marginal"),
                        robust = FALSE,
                        ...) {

  # check method
  if (is.null(method)) {
    method <- "wald"
  }
  method <- tolower(method)
  method <- match.arg(method, choices = c("wald", "ml1", "betwithin", "kr",
                                          "satterthwaite", "kenward", "boot",
                                          "profile", "residual", "normal"))

  effects <- match.arg(effects)
  component <- match.arg(component)

  if (method == "ml1") {
    return(ci_ml1(model, ci = ci))
  } else if (method == "betwithin") {
    return(ci_betwithin(model, ci = ci))
  } else if (method == "satterthwaite") {
    return(ci_satterthwaite(model, ci = ci))
  } else if (method %in% c("kenward", "kr")) {
    return(ci_kenward(model, ci = ci))
  }

  out <- lapply(ci, function(i) {
    .ci_dof(
      model = model,
      ci = i,
      dof = dof,
      effects = effects,
      component = component,
      robust = robust,
      method = method,
      ...
    )
  })

  out <- do.call(rbind, out)
  row.names(out) <- NULL
  out
}


#' @keywords internal
.ci_dof <- function(model,
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

  # sanity check...
  if (is.null(method)) {
    method <- "wald"
  }
  method <- tolower(method)

  # if we have adjusted SE, e.g. from kenward-roger, don't recompute
  # standard errors to save time...
  if (is.null(se)) {
    stderror <- if (isTRUE(robust)) {
      standard_error_robust(model, component = component, ...)
    } else {
      switch(method,
        "kenward" = ,
        "kr" = se_kenward(model),
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
    dof <- degrees_of_freedom(model, method = method, verbose = FALSE, ...)
    # make sure we have a value for degrees of freedom
    if (is.null(dof) || length(dof) == 0 || .is_chi2_model(model, dof, ...)) {
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



.is_chi2_model <- function(model, dof, ...) {
  dot_args <- list(...)
  if ("statistic" %in% names(dot_args)) {
    statistic <- dot_args[["statistic"]]
  } else {
    statistic <- insight::find_statistic(model)
  }
  (all(dof == 1) && identical(statistic, "chi-squared statistic"))
}

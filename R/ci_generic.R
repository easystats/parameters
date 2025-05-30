# generic function for CI calculation
.ci_generic <- function(model,
                        ci = 0.95,
                        method = "wald",
                        dof = NULL,
                        effects = "fixed",
                        component = "all",
                        vcov = NULL,
                        vcov_args = NULL,
                        verbose = TRUE,
                        ...) {
  # check method
  if (is.null(method)) {
    method <- "wald"
  }
  method <- tolower(method)
  method <- insight::validate_argument(
    method,
    c(
      "wald", "ml1", "betwithin", "kr", "satterthwaite", "kenward", "boot",
      "profile", "residual", "normal"
    )
  )

  effects <- insight::validate_argument(effects, c("fixed", "random", "all"))
  component <- insight::validate_argument(
    component,
    c(
      "all", "conditional", "zi", "zero_inflated", "dispersion", "precision",
      "scale", "smooth_terms", "full", "marginal"
    )
  )

  if (method == "ml1") { # nolint
    return(ci_ml1(model, ci = ci))
  } else if (method == "betwithin") {
    return(ci_betwithin(model, ci = ci))
  } else if (method == "satterthwaite") {
    return(ci_satterthwaite(model, ci = ci))
  } else if (method %in% c("kenward", "kr")) {
    return(ci_kenward(model, ci = ci))
  }

  # default CIs follow here (methods wald, boot, profile, residual, normal)
  out <- lapply(ci, function(i) {
    .ci_dof(
      model = model,
      ci = i,
      dof = dof,
      effects = effects,
      component = component,
      method = method,
      vcov = vcov,
      vcov_args = vcov_args,
      verbose = verbose,
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
                    method = "wald",
                    se = NULL,
                    vcov = NULL,
                    vcov_args = NULL,
                    verbose = TRUE,
                    ...) {
  # need parameters to calculate the CIs
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
  params <- .check_rank_deficiency(model, params, verbose = FALSE)
  # for polr, we need to fix parameter names
  params$Parameter <- gsub("Intercept: ", "", params$Parameter, fixed = TRUE)

  # validation check...
  if (is.null(method)) {
    method <- "wald"
  }
  method <- tolower(method)

  # Fist, we want standard errors for parameters
  # --------------------------------------------

  # if we have adjusted SE, e.g. from kenward-roger, don't recompute
  # standard errors to save time...
  if (is.null(se)) {
    if (!is.null(vcov) || isTRUE(list(...)[["robust"]])) {
      # robust (HC) standard errors?
      stderror <- standard_error(model,
        component = component,
        vcov = vcov,
        vcov_args = vcov_args,
        verbose = verbose,
        ...
      )
    } else {
      # normal standard errors, including small-sample approximations
      stderror <- switch(method,
        kenward = se_kenward(model),
        kr = se_kenward(model),
        satterthwaite = se_satterthwaite(model),
        standard_error(model, component = component)
      )
    }

    # if we have a non-empty stderror, use it
    if (insight::is_empty_object(stderror)) {
      return(NULL)
    }

    # filter non-matching parameters, resp. sort stderror and parameters,
    # so both have the identical order of values
    if (nrow(stderror) != nrow(params) ||
      !all(stderror$Parameter %in% params$Parameter) ||
      !all(order(stderror$Parameter) == order(params$Parameter))) {
      params <- stderror <- merge(stderror, params, sort = FALSE)
    }

    se <- stderror$SE
  }

  # Next, we need degrees of freedom
  # --------------------------------

  # check if we have a valid dof vector
  if (is.null(dof)) {
    # residual df
    dof <- insight::get_df(x = model, type = method, verbose = FALSE)
    # make sure we have a value for degrees of freedom
    if (is.null(dof) || length(dof) == 0 || .is_chi2_model(model, dof)) {
      dof <- Inf
    } else if (length(dof) > nrow(params)) {
      # filter non-matching parameters
      dof <- dof[seq_len(nrow(params))]
    }
  }

  # Now we can calculate CIs
  # ------------------------

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

  # for cox-panel models, we have non-linear parameters with NA coefficient,
  # but test statistic and p-value - don't check for NA estimates in this case
  if (anyNA(params$Estimate) && !inherits(model, "coxph.penal")) {
    out[stats::complete.cases(out), ]
  } else {
    out
  }
}


.is_chi2_model <- function(model, dof) {
  statistic <- insight::find_statistic(model)
  (all(dof == 1) && identical(statistic, "chi-squared statistic"))
}

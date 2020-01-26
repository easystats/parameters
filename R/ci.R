#' Confidence Intervals (CI)
#'
#' Compute confidence intervals (CI) for frequentist models.
#'
#' @param x A statistical model.
#' @param ci Confidence Interval (CI) level. Default to 0.95 (95\%).
#' @param method For mixed models of class \code{merMod}, can be \code{\link[=ci_wald]{"wald"}} (default), \code{"ml1"}, \code{"satterthwaite"}, \code{"kenward"} or \code{"boot"} (see also \code{\link{p_value_kenward}} and \code{lme4::confint.merMod}). For (generalized) linear models, can be \code{"robust"} to compute confidence intervals based on robust standard errors, and for generalized linear models, may also be \code{"profile"} (default) or \code{"wald"}.
#' @param ... Arguments passed down to \code{standard_error_robust()} when confidence intervals or p-values based on robust standard errors should be computed.
#' @inheritParams simulate_model
#' @inheritParams standard_error
#'
#' @return A data frame containing the CI bounds.
#'
#' @note \code{ci_robust()} resp. \code{ci(method = "robust")}
#'   rely on the \pkg{sandwich} or \pkg{clubSandwich} package (the latter if
#'   \code{vcov_estimation = "CR"} for cluster-robust standard errors) and will
#'   thus only work for those models supported by those packages.
#'
#' @examples
#' \donttest{
#' library(parameters)
#' library(glmmTMB)
#'
#' model <- glmmTMB(
#'   count ~ spp + mined + (1 | site),
#'   ziformula = ~mined,
#'   family = poisson(),
#'   data = Salamanders
#' )
#'
#' ci(model)
#' ci(model, component = "zi")
#' }
#' @importFrom insight find_parameters
#' @export
ci.merMod <- function(x, ci = 0.95, method = c("wald", "ml1", "satterthwaite", "kenward", "boot"), ...) {
  method <- tolower(method)
  method <- match.arg(method)

  # Wald approx
  if (method == "wald") {
    out <- ci_wald(model = x, ci = ci, dof = Inf)

    # ml1 approx
  } else if (method == "ml1") {
    out <- ci_ml1(x, ci)

    # Satterthwaite
  } else if (method == "satterthwaite") {
    out <- ci_satterthwaite(x, ci)

    # Kenward approx
  } else if (method %in% c("kenward", "kr")) {
    out <- ci_kenward(x, ci)

    # bootstrapping
  } else if (method == "boot") {
    out <- lapply(ci, function(ci, x) .ci_boot_merMod(x, ci, ...), x = x)
    out <- do.call(rbind, out)
    row.names(out) <- NULL
  }

  out
}


#' @importFrom bayestestR ci
#' @export
bayestestR::ci


# Default Wald CI method ------------------------------------------------------


#' @rdname ci.merMod
#' @export
ci.default <- function(x, ci = .95, method = NULL, ...) {
  robust <- !is.null(method) && method == "robust"
  ci_wald(model = x, ci = ci, robust = robust, ...)
}


#' @export
ci.mlm <- function(x, ci = .95, ...) {
  out <- lapply(ci, function(i) {
    .ci <- stats::confint(x, level = i, ...)
    rn <- rownames(.ci)
    .data_frame(
      Parameter = gsub("^(.*):(.*)", "\\2", rn),
      CI = i,
      CI_low = .ci[, 1],
      CI_high = .ci[, 2],
      Response = gsub("^(.*):(.*)", "\\1", rn)
    )
  })

  .remove_backticks_from_parameter_names(do.call(rbind, out))
}


#' @method ci lm
#' @export
ci.lm <- function(x, ci = .95, method = NULL, ...) {
  robust <- !is.null(method) && method == "robust"
  ci_wald(model = x, ci = ci, robust = robust, ...)
}

#' @export
ci.lm_robust <- ci.lm

#' @export
ci.comlmrob <- ci.lm

#' @export
ci.rq <- ci.lm

#' @export
ci.rqss <- ci.lm

#' @export
ci.crq <- ci.lm

#' @export
ci.nlrq <- ci.lm

#' @export
ci.BBmm <- ci.lm

#' @export
ci.BBreg <- ci.lm


#' @export
ci.gam <- function(x, ci = .95, ...) {
  ci_wald(model = x, ci = ci, ...)
}


#' @export
ci.list <- function(x, ci = .95, ...) {
  if ("gam" %in% names(x)) {
    x <- x$gam
    class(x) <- c("gam", "lm", "glm")
    ci(x, ci = ci, ...)
  } else {
    return(NULL)
  }
}





# glm CI method with profiling -----------------------------------------------


#' @rdname ci.merMod
#' @method ci glm
#' @export
ci.glm <- function(x, ci = .95, method = c("profile", "wald", "robust"), ...) {
  method <- match.arg(method)
  if (method == "profile") {
    out <- lapply(ci, function(i) .ci_profiled(model = x, ci = i))
    out <- do.call(rbind, out)
  } else if (method == "robust") {
    out <- ci_wald(model = x, ci = ci, robust = TRUE, ...)
  } else {
    out <- ci_wald(model = x, ci = ci)
  }

  row.names(out) <- NULL
  out
}

#' @export
ci.negbin <- ci.glm

#' @export
ci.logistf <- ci.glm


#' @rdname ci.merMod
#' @export
ci.polr <- function(x, ci = .95, method = c("profile", "wald", "robust"), ...) {
  method <- match.arg(method)
  if (method == "profile") {
    out <- lapply(ci, function(i) .ci_profiled2(model = x, ci = i))
    out <- do.call(rbind, out)
  } else if (method == "robust") {
    out <- ci_wald(model = x, ci = ci, robust = TRUE, ...)
  } else {
    out <- ci_wald(model = x, ci = ci)
  }

  # for polr, profiled CI do not return CI for response levels
  # thus, we also calculate Wald CI and add missing rows to result

  out_missing <- ci_wald(model = x, ci = ci)
  missing_rows <- out_missing$Parameter %in% setdiff(out_missing$Parameter, out$Parameter)
  out <- rbind(out, out_missing[missing_rows, ])

  # fix names, to match standard error and p_value

  out$Parameter <- gsub("Intercept: ", "", out$Parameter, fixed = TRUE)
  row.names(out) <- NULL

  out
}






# Default Wald CI method with Inf dof -----------------------------------------


#' @export
ci.gamlss <- function(x, ci = .95, method = NULL, ...) {
  robust <- !is.null(method) && method == "robust"
  ci_wald(model = x, ci = ci, dof = Inf, robust = robust, ...)
}

#' @export
ci.speedglm <- ci.gamlss

#' @export
ci.cpglm <- ci.gamlss

#' @export
ci.cpglmm <- ci.gamlss

#' @export
ci.glmx <- ci.gamlss

#' @export
ci.glmmadmb <- ci.gamlss

#' @export
ci.fixest <- ci.gamlss

#' @export
ci.feglm <- ci.gamlss

#' @export
ci.speedlm <- ci.gamlss

#' @export
ci.glmrob <- ci.gamlss

#' @export
ci.plm <- ci.gamlss

#' @export
ci.LORgee <- ci.gamlss

#' @export
ci.truncreg <- ci.gamlss

#' @export
ci.ivreg <- ci.gamlss

#' @export
ci.gee <- ci.gamlss

#' @export
ci.tobit <- ci.gamlss

#' @export
ci.geeglm <- ci.gamlss

#' @export
ci.coxph <- ci.gamlss

#' @export
ci.aareg <- ci.gamlss

#' @export
ci.clm <- ci.gamlss

#' @export
ci.crch <- ci.gamlss

#' @export
ci.feis <- ci.gamlss

#' @export
ci.censReg <- ci.gamlss

#' @export
ci.survreg <- ci.gamlss

#' @export
ci.flexsurvreg <- ci.gamlss

#' @export
ci.coxme <- ci.gamlss

#' @export
ci.svyglm.nb <- ci.gamlss

#' @export
ci.lrm <- ci.gamlss

#' @export
ci.psm <- ci.gamlss

#' @export
ci.ols <- ci.gamlss

#' @export
ci.rms <- ci.gamlss

#' @export
ci.svyglm.zip <- ci.gamlss

#' @export
ci.vglm <- ci.gamlss

#' @export
ci.svyglm.glimML <- ci.gamlss


#' @rdname ci.merMod
#' @export
ci.mixor <- function(x, ci = .95, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  ci_wald(model = x, ci = ci, dof = Inf, effects = effects, robust = FALSE, ...)
}


#' @export
ci.gamm <- function(x, ci = .95, ...) {
  x <- x$gam
  class(x) <- c("gam", "lm", "glm")
  ci(x, ci = ci, ...)
}

#' @export
ci.gamm4 <- ci.gamm


#' @export
ci.multinom <- function(x, ci = .95, method = NULL, ...) {
  robust <- !is.null(method) && method == "robust"
  params <- insight::get_parameters(x)

  out <- ci_wald(model = x, ci = ci, dof = Inf, robust = robust, ...)

  if ("Response" %in% colnames(params)) {
    out$Response <- params$Response
  }

  out
}

#' @export
ci.brmultinom <- ci.multinom

#' @export
ci.bracl <- ci.multinom


#' @export
ci.DirichletRegModel <- function(x, ci = .95, component = c("all", "conditional", "precision"), ...) {
  component <- match.arg(component)
  params <- insight::get_parameters(x, component = component)

  out <- ci_wald(model = x, ci = ci, dof = Inf, ...)

  if (is.null(out$Component)) {
    component <- "all"
  }

  if (component != "all") {
    out <- out[out$Component == component, ]
  }

  if ("Response" %in% colnames(params)) {
    out$Response <- params$Response
  }

  out
}








# Zero-Inflated and Mixed models -----------------------------------------


#' @rdname ci.merMod
#' @export
ci.glmmTMB <- function(x, ci = .95, component = c("all", "conditional", "zi", "zero_inflated"), method = NULL, ...) {
  robust <- !is.null(method) && method == "robust"
  component <- match.arg(component)

  if (is.null(.check_component(x, component))) {
    return(NULL)
  }
  ci_wald(model = x, ci = ci, dof = Inf, component = component, robust = robust)
}

#' @rdname ci.merMod
#' @export
ci.zeroinfl <- ci.glmmTMB

#' @rdname ci.merMod
#' @export
ci.hurdle <- ci.glmmTMB

#' @export
ci.zerocount <- ci.glmmTMB


#' @rdname ci.merMod
#' @export
ci.MixMod <- function(x, ci = .95, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)
  if (is.null(.check_component(x, component))) {
    return(NULL)
  }
  ci_wald(model = x, ci = ci, dof = Inf, component = component)
}






# Special models -----------------------------------------


#' @rdname ci.merMod
#' @export
ci.betareg <- function(x, ci = .95, component = c("all", "conditional", "precision"), ...) {
  component <- match.arg(component)
  ci_wald(model = x, ci = ci, dof = Inf, component = component)
}


#' @rdname ci.merMod
#' @export
ci.clm2 <- function(x, ci = .95, component = c("all", "conditional", "scale"), ...) {
  component <- match.arg(component)
  ci_wald(model = x, ci = ci, dof = Inf, component = component)
}

#' @export
ci.clmm2 <- ci.clm2


#' @export
ci.biglm <- function(x, ci = .95, ...) {
  out <- lapply(ci, function(i) {
    ci_list <- stats::confint(x, level = i, ...)
    .data_frame(
      Parameter = rownames(ci_list),
      CI = i * 100,
      CI_low = as.vector(ci_list[, 1]),
      CI_high = as.vector(ci_list[, 2])
    )
  })

  .remove_backticks_from_parameter_names(do.call(rbind, out))
}

#' @export
ci.gls <- ci.biglm


#' @rdname ci.merMod
#' @export
ci.lme <- function(x, ci = .95, method = c("wald", "ml1", "satterthwaite"), ...) {
  method <- tolower(method)
  method <- match.arg(method)

  if (method == "wald") {
    if (!requireNamespace("nlme", quietly = TRUE)) {
      ci_wald(model = x, ci = ci)
    } else {
      out <- lapply(ci, function(i) {
        ci_list <- nlme::intervals(x, level = i, ...)
        .data_frame(
          Parameter = rownames(ci_list$fixed),
          CI = i * 100,
          CI_low = as.vector(ci_list$fixed[, "lower"]),
          CI_high = as.vector(ci_list$fixed[, "upper"])
        )
      })
      .remove_backticks_from_parameter_names(do.call(rbind, out))
    }
    # ml1 approx
  } else if (method == "ml1") {
    ci_ml1(x, ci)

    # Satterthwaite
  } else if (method == "satterthwaite") {
    ci_satterthwaite(x, ci)
  }
}



#' @importFrom insight print_color
#' @importFrom stats qnorm
#' @export
ci.effectsize_std_params <- function(x, ci = .95, ...) {
  se <- attr(x, "standard_error")

  if (is.null(se)) {
    insight::print_color("\nCould not extract standard errors of standardized coefficients.\n", "red")
    return(NULL)
  }

  out <- lapply(ci, function(i) {
    alpha <- (1 + i) / 2
    fac <- stats::qnorm(alpha)
    data.frame(
      Parameter = x$Parameter,
      CI = i * 100,
      CI_low = x$Std_Coefficient - se * fac,
      CI_high = x$Std_Coefficient + se * fac,
      stringsAsFactors = FALSE
    )
  })

  .remove_backticks_from_parameter_names(do.call(rbind, out))
}



#' @export
ci.rma <- function(x, ci = .95, ...) {
  params <- insight::get_parameters(x)
  out <- lapply(ci, function(i) {
    model <- stats::update(x, level = i)
    .data_frame(
      Parameter = params[[1]],
      CI = i * 100,
      CI_low = as.vector(model$ci.lb),
      CI_high = as.vector(model$ci.ub)
    )
  })
  .remove_backticks_from_parameter_names(do.call(rbind, out))
}






# helper -----------------------------------------


#' @keywords internal
.check_component <- function(m, x) {
  if (!insight::model_info(m)$is_zero_inflated && x %in% c("zi", "zero_inflated")) {
    insight::print_color("Model has no zero-inflation component!\n", "red")
    x <- NULL
  }
  x
}


#' @keywords internal
.ci_from_refit <- function(std_coef, ci) {
  se <- attributes(std_coef)$standard_error$SE
  alpha <- (1 + ci) / 2
  fac <- stats::qnorm(alpha)
  out <- data.frame(
    Parameter = std_coef$Parameter,
    CI = ci * 100,
    CI_low = std_coef$Std_Coefficient - se * fac,
    CI_high = std_coef$Std_Coefficient + se * fac,
    stringsAsFactors = FALSE
  )
  .remove_backticks_from_parameter_names(out)
}

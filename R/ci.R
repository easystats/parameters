#' Confidence Interval (CI)
#'
#' Compute confidence intervals (CI) for frequentist models.
#'
#' @param x A statistical model.
#' @param ci Confidence Interval (CI) level. Default to 0.95 (95\%).
#' @param method For mixed models of class \code{merMod}, can be \code{\link[=ci_wald]{"wald"}} (default) or \code{"boot"} (see \code{lme4::confint.merMod}). For generalized linear models, can be \code{"profile"} (default) or \code{"wald"}.
#' @param ... Arguments passed to or from other methods.
#' @inheritParams model_simulate
#'
#' @return A data frame containing the CI bounds.
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
ci.merMod <- function(x, ci = 0.95, method = c("wald", "boot"), ...) {
  method <- match.arg(method)

  # Wald approx
  if (method == "wald") {
    out <- ci_wald(model = x, ci = ci, dof = Inf)

    # Bootstrapped CIs
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


#' @export
ci.default <- function(x, ci = .95, ...) {
  ci_wald(model = x, ci = ci, ...)
}


#' @method ci lm
#' @export
ci.lm <- function(x, ci = .95, ...) {
  ci_wald(model = x, ci = ci, component = "conditional")
}


#' @export
ci.gam <- function(x, ci = .95, ...) {
  ci_wald(model = x, ci = ci, ...)
}


#' @export
ci.lm_robust <- ci.lm


#' @export
ci.rq <- ci.lm


#' @export
ci.crq <- ci.lm


#' @export
ci.nlrq <- ci.lm


#' @export
ci.BBmm <- ci.lm


#' @export
ci.BBreg <- ci.lm


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
ci.glm <- function(x, ci = .95, method = c("profile", "wald"), ...) {
  method <- match.arg(method)
  if (method == "profile") {
    out <- lapply(ci, function(i) .ci_profiled(model = x, ci = i))
    out <- do.call(rbind, out)
  } else {
    out <- ci_wald(model = x, ci = ci, component = "conditional")
  }
  row.names(out) <- NULL
  out
}


#' @export
ci.negbin <- ci.glm


#' @export
ci.logistf <- ci.glm


#' @export
ci.polr <- function(x, ci = .95, method = c("profile", "wald"), ...) {
  method <- match.arg(method)
  if (method == "profile") {
    out <- lapply(ci, function(i) .ci_profiled2(model = x, ci = i))
    out <- do.call(rbind, out)
  } else {
    out <- ci_wald(model = x, ci = ci, component = "conditional")
  }

  # for polr, profiled CI do not return CI for response levels
  # thus, we also calculate Wald CI and add missing rows to result

  out_missing <- ci_wald(model = x, ci = ci, component = "conditional")
  missing_rows <- out_missing$Parameter %in% setdiff(out_missing$Parameter, out$Parameter)
  out <- rbind(out, out_missing[missing_rows, ])

  # fix names, to match standard error and p_value

  out$Parameter <- gsub("Intercept: ", "", out$Parameter, fixed = TRUE)
  row.names(out) <- NULL

  out
}






# Default Wald CI method with Inf dof -----------------------------------------


#' @export
ci.gamlss <- function(x, ci = .95, ...) {
  ci_wald(model = x, ci = ci, dof = Inf, ...)
}

#' @export
ci.speedglm <- ci.gamlss

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
ci.clm <- ci.gamlss

#' @export
ci.clm2 <- ci.gamlss

#' @export
ci.crch <- ci.gamlss

#' @export
ci.feis <- ci.gamlss

#' @export
ci.betareg <- ci.gamlss

#' @export
ci.censReg <- ci.gamlss

#' @export
ci.survreg <- ci.gamlss

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

#' @export
ci.gamm <- function(x, ci = .95, ...) {
  x <- x$gam
  class(x) <- c("gam", "lm", "glm")
  ci(x, ci = ci, ...)
}


#' @export
ci.gamm4 <- ci.gamm






# Zero-Inflated and Mixed models -----------------------------------------


#' @rdname ci.merMod
#' @export
ci.glmmTMB <- function(x, ci = .95, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)
  if (is.null(.check_component(x, component))) {
    return(NULL)
  }
  ci_wald(model = x, ci = ci, dof = Inf, component = component)
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

  do.call(rbind, out)
}


#' @export
ci.gls <- function(x, ci = .95, ...) {
  out <- lapply(ci, function(i) {
    ci_list <- stats::confint(x, level = i, ...)
    .data_frame(
      Parameter = rownames(ci_list),
      CI = i * 100,
      CI_low = as.vector(ci_list[, 1]),
      CI_high = as.vector(ci_list[, 2])
    )
  })

  do.call(rbind, out)
}


#' @export
ci.lme <- function(x, ci = .95, ...) {
  if (!requireNamespace("nlme", quietly = TRUE)) {
    ci_wald(model = x, ci = ci, component = "conditional")
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
    do.call(rbind, out)
  }
}






# helper -----------------------------------------


.check_component <- function(m, x) {
  if (!insight::model_info(m)$is_zero_inflated && x %in% c("zi", "zero_inflated")) {
    insight::print_color("Model has no zero-inflation component!\n", "red")
    x <- NULL
  }
  x
}

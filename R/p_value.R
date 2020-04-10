#' p-values
#'
#' This function attempts to return, or compute, p-values of a model's parameters. The nature of the p-values is different depending on the model:
#' \itemize{
#' \item Mixed models (\pkg{lme4}): By default, p-values are based on Wald-test approximations (see \code{\link{p_value_wald}}). For certain situations, the "m-l-1" rule might be a better approximation. That is, for \code{method = "ml1"}, \code{\link{p_value_ml1}} is called. For \code{lmerMod} objects, if \code{method = "kenward"}, p-values are based on Kenward-Roger approximations, i.e. \code{\link{p_value_kenward}} is called, and \code{method = "satterthwaite"} calls \code{\link{p_value_satterthwaite}}.
#' \item Bayesian models (\pkg{rstanarm}, \pkg{brms}): For Bayesian models, the p-values corresponds to the \emph{probability of direction} (\code{\link[bayestestR]{p_direction}}), which is converted to a p-value using \code{\link[bayestestR]{convert_pd_to_p}}.
#' }
#'
#' @param model A statistical model.
#' @param method For mixed models, can be \code{\link[=p_value_wald]{"wald"}} (default), \code{\link[=p_value_ml1]{"ml1"}}, \code{\link[=p_value_betwithin]{"betwithin"}}, \code{\link[=p_value_satterthwaite]{"satterthwaite"}} or \code{\link[=p_value_kenward]{"kenward"}}. For models that are supported by the \pkg{sandwich} or \pkg{clubSandwich} packages, may also be \code{method = "robust"} to compute p-values based ob robust standard errors.
#' @param ... Arguments passed down to \code{standard_error_robust()} when confidence intervals or p-values based on robust standard errors should be computed.
#' @inheritParams simulate_model
#' @inheritParams standard_error
#'
#' @note \code{p_value_robust()} resp. \code{p_value(method = "robust")}
#'   rely on the \pkg{sandwich} or \pkg{clubSandwich} package (the latter if
#'   \code{vcov_estimation = "CR"} for cluster-robust standard errors) and will
#'   thus only work for those models supported by those packages.
#'
#' @examples
#' if (require("lme4")) {
#'   data(iris)
#'   model <- lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
#'   p_value(model)
#' }
#' @return The p-values.
#' @importFrom bayestestR p_direction convert_pd_to_p
#' @importFrom stats coef vcov pt pnorm na.omit
#' @importFrom insight get_statistic get_parameters find_parameters print_color
#' @importFrom methods slot
#' @importFrom utils capture.output
#' @export
p_value <- function(model, ...) {
  UseMethod("p_value")
}



# p-Values from Standard Models -----------------------------------------------


#' @rdname p_value
#' @export
p_value.default <- function(model, method = NULL, ...) {
  if (!is.null(method)) {
    method <- tolower(method)
  } else {
    method <- "wald"
  }

  p <- NULL

  if (method == "robust") {
    return(p_value_robust(model, ...))
  } else if (method == "ml1") {
    return(p_value_ml1(model))
  } else if (method == "betwithin") {
    return(p_value_betwithin(model))
  } else {
    # first, we need some special handling for Zelig-models
    p <- tryCatch(
      {
        if (grepl("^Zelig-", class(model)[1])) {
          unlist(model$get_pvalue())
        } else {
          # try to get p-value from classical summary for default models
          .get_pval_from_summary(model)
        }
      },
      error = function(e) {
        NULL
      }
    )
  }

  # if all fails, try to get p-value from test-statistic
  if (is.null(p)) {
    p <- tryCatch(
      {
        stat <- insight::get_statistic(model)
        p_from_stat <- 2 * stats::pnorm(abs(stat$Statistic), lower.tail = FALSE)
        names(p_from_stat) <- stat$Parameter
        p_from_stat
      },
      error = function(e) {
        NULL
      }
    )
  }

  if (is.null(p)) {
    insight::print_color("\nCould not extract p-values from model object.\n", "red")
  } else {
    .data_frame(
      Parameter = names(p),
      p = as.vector(p)
    )
  }
}

#' @export
p_value.lm <- p_value.default

#' @export
p_value.LORgee <- p_value.default

#' @export
p_value.lm_robust <- p_value.default

#' @export
p_value.truncreg <- p_value.default

#' @export
p_value.geeglm <- p_value.default

#' @export
p_value.censReg <- p_value.default

#' @export
p_value.ivreg <- p_value.default

#' @export
p_value.negbin <- p_value.default



#' @export
p_value.mlm <- function(model, ...) {
  cs <- stats::coef(summary(model))
  p <- lapply(names(cs), function(x) {
    params <- cs[[x]]
    .data_frame(
      Parameter = rownames(params),
      p = params[, "Pr(>|t|)"],
      Response = gsub("^Response (.*)", "\\1", x)
    )
  })

  .remove_backticks_from_parameter_names(do.call(rbind, p))
}



#' @export
p_value.tobit <- function(model, ...) {
  params <- insight::get_parameters(model)
  p <- p_value.default(model, ...)
  p[p$Parameter %in% params$Parameter, ]
}






# p-Values from Zero-Inflated Models ------------------------------------------


#' @export
p_value.zeroinfl <- function(model, component = c("all", "conditional", "zi", "zero_inflated"), method = NULL, ...) {
  component <- match.arg(component)
  if (is.null(.check_component(model, component))) {
    return(NULL)
  }

  robust <- !is.null(method) && method == "robust"
  if (isTRUE(robust)) {
    return(p_value_robust(model, ...))
  }

  cs <- .compact_list(stats::coef(summary(model)))
  x <- lapply(names(cs), function(i) {
    comp <- ifelse(i == "count", "conditional", "zi")
    stats <- cs[[i]]

    # remove log(theta)
    theta <- grepl("Log(theta)", rownames(stats), fixed = TRUE)
    if (any(theta)) {
      stats <- stats[!theta, ]
    }

    .data_frame(
      Parameter = insight::find_parameters(model, effects = "fixed", component = comp, flatten = TRUE),
      p = as.vector(stats[, 4]),
      Component = comp
    )
  })

  p <- do.call(rbind, x)
  p$Component <- .rename_values(p$Component, "cond", "conditional")
  p$Component <- .rename_values(p$Component, "zi", "zero_inflated")

  .filter_component(p, component)
}


#' @export
p_value.hurdle <- p_value.zeroinfl

#' @export
p_value.zerocount <- p_value.zeroinfl


#' @importFrom utils capture.output
#' @export
p_value.zcpglm <- function(model, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  if (!requireNamespace("cplm", quietly = TRUE)) {
    stop("To use this function, please install package 'cplm'.")
  }

  component <- match.arg(component)
  junk <- utils::capture.output(stats <- cplm::summary(model)$coefficients)
  params <- get_parameters(model)

  tweedie <- data.frame(
    Parameter = params$Parameter[params$Component == "conditional"],
    p = as.vector(stats$tweedie[, "Pr(>|z|)"]),
    Component = "conditional",
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  zero <- data.frame(
    Parameter = params$Parameter[params$Component == "zero_inflated"],
    p = as.vector(stats$zero[, "Pr(>|z|)"]),
    Component = "zero_inflated",
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  out <- .filter_component(rbind(tweedie, zero), component)
  out
}







# p-Values from Mixed Models -----------------------------------------------


#' @export
p_value.lme <- function(model, ...) {
  cs <- stats::coef(summary(model))
  p <- cs[, 5]

  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(cs)),
    p = as.vector(p)
  )
}



#' @rdname p_value
#' @export
p_value.lmerMod <- function(model, method = "wald", ...) {
  method <- tolower(method)
  method <- match.arg(method, c("wald", "ml1", "betwithin", "satterthwaite", "kr", "kenward"))
  if (method == "wald") {
    p_value_wald(model, ...)
  } else if (method == "ml1") {
    p_value_ml1(model, ...)
  } else if (method == "betwithin") {
    p_value_betwithin(model, ...)
  } else if (method == "satterthwaite") {
    p_value_satterthwaite(model, ...)
  } else if (method %in% c("kr", "kenward")) {
    p_value_kenward(model, ...)
  }
}



#' @rdname p_value
#' @export
p_value.merMod <- function(model, method = "wald", ...) {
  method <- tolower(method)
  method <- match.arg(method, c("wald", "betwithin", "ml1"))
  if (method == "wald") {
    dof <- Inf
  } else if (method == "ml1") {
    dof <- dof_ml1(model)
  } else {
    dof <- dof_betwithin(model)
  }
  p_value_wald(model, dof, ...)
}

#' @export
p_value.cpglmm <- p_value.merMod


#' @rdname p_value
#' @export
p_value.rlmerMod <- function(model, method = "wald", ...) {
  method <- match.arg(method, c("wald", "betwithin", "ml1"))
  if (method == "wald") {
    dof <- Inf
  } else if (method == "ml1") {
    dof <- dof_ml1(model)
  } else {
    dof <- dof_betwithin(model)
  }
  p_value_wald(model, dof, ...)
}



#' @rdname p_value
#' @export
p_value.glmmTMB <- function(model, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)
  if (is.null(.check_component(model, component))) {
    return(NULL)
  }

  cs <- .compact_list(stats::coef(summary(model)))
  x <- lapply(names(cs), function(i) {
    .data_frame(
      Parameter = insight::find_parameters(model, effects = "fixed", component = i, flatten = TRUE),
      p = as.vector(cs[[i]][, 4]),
      Component = i
    )
  })

  p <- do.call(rbind, x)
  p$Component <- .rename_values(p$Component, "cond", "conditional")
  p$Component <- .rename_values(p$Component, "zi", "zero_inflated")

  .filter_component(p, component)
}



#' @rdname p_value
#' @export
p_value.MixMod <- function(model, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)
  if (is.null(.check_component(model, component))) {
    return(NULL)
  }

  s <- summary(model)
  cs <- list(s$coef_table, s$coef_table_zi)
  names(cs) <- c("conditional", "zero_inflated")
  cs <- .compact_list(cs)
  x <- lapply(names(cs), function(i) {
    .data_frame(
      Parameter = insight::find_parameters(model, effects = "fixed", component = i, flatten = TRUE),
      p = as.vector(cs[[i]][, 4]),
      Component = i
    )
  })

  p <- do.call(rbind, x)
  .filter_component(p, component)
}


#' @rdname p_value
#' @importFrom insight get_parameters
#' @export
p_value.mixor <- function(model, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  stats <- model$Model[, "P(>|z|)"]
  parms <- get_parameters(model, effects = effects)

  .data_frame(
    Parameter = parms$Parameter,
    p = stats[parms$Parameter],
    Effects = parms$Effects
  )
}







# p-Values from Bayesian Models -----------------------------------------------


#' @export
p_value.MCMCglmm <- function(model, ...) {
  nF <- model$Fixed$nfl
  p <- 1 - colSums(model$Sol[, 1:nF, drop = FALSE] > 0) / dim(model$Sol)[1]

  .data_frame(
    Parameter = insight::find_parameters(model, effects = "fixed", flatten = TRUE),
    p = p
  )
}


#' @export
p_value.brmsfit <- function(model, ...) {
  p <- bayestestR::p_direction(model)

  .data_frame(
    Parameter = .remove_backticks_from_string(p$Parameter),
    p = sapply(p$pd, bayestestR::convert_pd_to_p, simplify = TRUE)
  )
}


#' @export
p_value.stanreg <- p_value.brmsfit


#' @export
p_value.BFBayesFactor <- p_value.brmsfit


#' @export
p_value.bcplm <- p_value.brmsfit







# p-Values from Survey Models -----------------------------------------------


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







# p-Values from ANOVA -----------------------------------------------


#' @export
p_value.aov <- function(model, ...) {
  params <- model_parameters(model)

  if (nrow(params) == 0) {
    return(NA)
  }

  if ("Group" %in% names(params)) {
    params <- params[params$Group == "Within", ]
  }

  if ("Residuals" %in% params$Parameter) {
    params <- params[params$Parameter != "Residuals", ]
  }

  if (!"p" %in% names(params)) {
    return(NA)
  }

  .data_frame(
    Parameter = params$Parameter,
    p = params$p
  )
}


#' @export
p_value.anova <- p_value.aov


#' @export
p_value.aovlist <- p_value.aov







# p-Values from Survival Models -----------------------------------------------


#' @export
p_value.coxph <- function(model, method = NULL, ...) {
  robust <- !is.null(method) && method == "robust"
  if (isTRUE(robust)) {
    return(p_value_robust(model, ...))
  }

  cs <- stats::coef(summary(model))
  p <- cs[, 5]
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    p = as.vector(p)
  )
}


#' @export
p_value.aareg <- function(model, ...) {
  s <- summary(model)
  p <- s$table[, "p"]

  .data_frame(
    Parameter = .remove_backticks_from_string(names(p)),
    p = as.vector(p)
  )
}



#' @export
p_value.coxme <- function(model, ...) {
  stat <- insight::get_statistic(model)

  if (!is.null(stat)) {
    .data_frame(
      Parameter = stat$Parameter,
      p = as.vector(1 - stats::pchisq(stat$Statistic^2, df = 1))
    )
  }
}



#' @export
p_value.survreg <- function(model, method = NULL, ...) {
  robust <- !is.null(method) && method == "robust"
  if (isTRUE(robust)) {
    return(p_value_robust(model, ...))
  }

  s <- summary(model)
  p <- s$table[, "p"]

  .data_frame(
    Parameter = .remove_backticks_from_string(names(p)),
    p = as.vector(p)
  )
}


#' @export
p_value.flexsurvreg <- function(model, ...) {
  params <- insight::get_parameters(model)
  est <- params$Estimate
  se <- standard_error(model)$SE
  p <- 2 * stats::pt(abs(est / se), df = degrees_of_freedom(model, method = "any"), lower.tail = FALSE)

  .data_frame(
    Parameter = params$Parameter,
    p = as.vector(p)
  )
}





# p-Values from Special Models -----------------------------------------------


#' @rdname p_value
#' @export
p_value.averaging <- function(model, component = c("conditional", "full"), ...) {
  component <- match.arg(component)
  params <- get_parameters(model, component = component)
  if (component == "full") {
    s <- summary(model)$coefmat.full
  } else {
    s <- summary(model)$coefmat.subset
  }

  .data_frame(
    Parameter = .remove_backticks_from_string(params$Parameter),
    p = as.vector(s[, 5])
  )
}


#' @rdname p_value
#' @export
p_value.DirichletRegModel <- function(model, component = c("all", "conditional", "precision"), ...) {
  component <- match.arg(component)
  params <- insight::get_parameters(model)

  out <- .data_frame(
    Parameter = params$Parameter,
    Response = params$Response,
    p = as.vector(2 * stats::pnorm(-abs(params$Estimate / model$se)))
  )

  if (!is.null(params$Component)) {
    out$Component <- params$Component
  } else {
    component <- "all"
  }

  if (component != "all") {
    out <- out[out$Component == component, ]
  }

  out
}


#' @rdname p_value
#' @export
p_value.clm2 <- function(model, component = c("all", "conditional", "scale"), ...) {
  component <- match.arg(component)

  params <- insight::get_parameters(model)
  cs <- stats::coef(summary(model))
  p <- cs[, 4]

  out <- .data_frame(
    Parameter = params$Parameter,
    Component = params$Component,
    p = as.vector(p)
  )

  if (component != "all") {
    out <- out[out$Component == component, ]
  }

  out
}


#' @export
p_value.clmm2 <- p_value.clm2


#' @export
p_value.cgam <- function(model, component = c("all", "conditional", "smooth_terms"), ...) {
  component <- match.arg(component)

  params <- insight::get_parameters(model, component = "all")
  cs <- summary(model)
  p <- as.vector(cs$coefficients[, 4])
  if (!is.null(cs$coefficients2)) p <- c(p, as.vector(cs$coefficients2[, "p.value"]))

  out <- .data_frame(
    Parameter = params$Parameter,
    Component = params$Component,
    p = as.vector(p)
  )

  if (component != "all") {
    out <- out[out$Component == component, ]
  }

  out
}


#' @importFrom utils capture.output
#' @export
p_value.cpglm <- function(model, ...) {
  if (!requireNamespace("cplm", quietly = TRUE)) {
    stop("To use this function, please install package 'cplm'.")
  }

  junk <- utils::capture.output(stats <- cplm::summary(model)$coefficients)
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    p = as.vector(stats[, "Pr(>|t|)"])
  )
}



#' @export
p_value.glmx <- function(model, ...) {
  stats <- stats::coef(summary(model))
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    p = c(as.vector(stats$glm[, "Pr(>|z|)"]), as.vector(stats$extra[, "Pr(>|z|)"])),
    Component = params$Component
  )
}



#' @export
p_value.rq <- function(model, ...) {
  p <- tryCatch(
    {
      cs <- suppressWarnings(stats::coef(summary(model)))
      cs[, "Pr(>|t|)"]
    },
    error = function(e) {
      .get_pval_from_summary(
        model,
        cs = suppressWarnings(stats::coef(summary(model, covariance = TRUE)))
      )
    }
  )

  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    p = p
  )
}

#' @export
p_value.crq <- p_value.rq

#' @export
p_value.nlrq <- p_value.rq



#' @export
p_value.rqss <- function(model, component = c("all", "conditional", "smooth_terms"), ...) {
  component <- match.arg(component)

  cs <- summary(model)$coef
  p_column <- intersect(c("Pr(>|t|)", "Pr(>|z|)"), colnames(cs))
  p_cond <- cs[, p_column]

  cs <- summary(model)$qsstab
  p_smooth <- cs[, "Pr(>F)"]

  params_cond <- insight::get_parameters(model, component = "conditional")
  params_smooth <- insight::get_parameters(model, component = "smooth_terms")

  out_cond <- .data_frame(
    Parameter = params_cond$Parameter,
    p = as.vector(p_cond),
    Component = "conditional"
  )

  out_smooth <- .data_frame(
    Parameter = params_smooth$Parameter,
    p = as.vector(p_smooth),
    Component = "smooth_terms"
  )

  switch(
    component,
    "all" = rbind(out_cond, out_smooth),
    "conditional" = out_cond,
    "smooth_terms" = out_smooth
  )
}



#' @export
p_value.biglm <- function(model, ...) {
  cs <- summary(model)$mat
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    p = as.vector(cs[, 5])
  )
}



#' @export
p_value.complmrob <- function(model, ...) {
  stats <- summary(model)$stats
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    p = as.vector(stats[, "Pr(>|t|)"])
  )
}



#' @export
p_value.fixest <- function(model, ...) {
  stats <- summary(model)$coeftable
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    p = as.vector(stats[, "Pr(>|z|)"])
  )
}



#' @export
p_value.feglm <- function(model, ...) {
  stats <- stats::coef(summary(model))
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    p = as.vector(stats[, 4])
  )
}



#' @export
p_value.crch <- function(model, ...) {
  cs <- do.call(rbind, stats::coef(summary(model), model = "full"))
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    p = as.vector(cs[, 4])
  )
}



#' @rdname p_value
#' @export
p_value.gee <- function(model, method = NULL, ...) {
  cs <- stats::coef(summary(model))

  if (!is.null(method) && method == "robust") {
    p <- 2 * stats::pt(abs(cs[, "Estimate"] / cs[, "Robust S.E."]), df = degrees_of_freedom(model, method = "any"), lower.tail = FALSE)
  } else {
    p <- 2 * stats::pt(abs(cs[, "Estimate"] / cs[, "Naive S.E."]), df = degrees_of_freedom(model, method = "any"), lower.tail = FALSE)
  }

  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(cs)),
    p = as.vector(p)
  )
}



#' @export
p_value.glimML <- function(model, ...) {
  if (!requireNamespace("aod", quietly = TRUE)) {
    stop("Package 'aod' required for this function to work. Please install it.")
  }

  s <- methods::slot(aod::summary(model), "Coef")
  p <- s[, 4]

  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(s)),
    p = as.vector(p)
  )
}



#' @export
p_value.logistf <- function(model, ...) {
  utils::capture.output(s <- summary(model))

  .data_frame(
    Parameter = .remove_backticks_from_string(names(s$prob)),
    p = as.vector(s$prob)
  )
}



#' @export
p_value.lrm <- function(model, ...) {
  stat <- insight::get_statistic(model)
  p <- 2 * stats::pt(abs(stat$Statistic), df = degrees_of_freedom(model, method = "any"), lower.tail = FALSE)

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
p_value.rlm <- function(model, ...) {
  cs <- stats::coef(summary(model))
  p <- 2 * stats::pt(abs(cs[, 3]), df = degrees_of_freedom(model, method = "any"), lower.tail = FALSE)

  .data_frame(
    Parameter = .remove_backticks_from_string(names(p)),
    p = as.vector(p)
  )
}



#' @export
p_value.betareg <- function(model, component = c("all", "conditional", "precision"), ...) {
  component <- match.arg(component)

  params <- insight::get_parameters(model)
  cs <- do.call(rbind, stats::coef(summary(model)))
  p <- cs[, 4]

  out <- .data_frame(
    Parameter = params$Parameter,
    Component = params$Component,
    p = as.vector(p)
  )

  if (component != "all") {
    out <- out[out$Component == component, ]
  }

  out
}



#' @export
p_value.gamlss <- function(model, ...) {
  parms <- insight::get_parameters(model)
  utils::capture.output(cs <- summary(model))

  .data_frame(
    Parameter = parms$Parameter,
    p = as.vector(cs[, 4]),
    Component = parms$Component
  )
}



#' @export
p_value.BBmm <- function(model, ...) {
  .data_frame(
    Parameter = insight::find_parameters(model, effects = "fixed", component = "conditional", flatten = TRUE),
    p = as.data.frame(summary(model)$fixed.coefficients)$p.value
  )
}



#' @export
p_value.BBreg <- function(model, ...) {
  .data_frame(
    Parameter = insight::find_parameters(model, effects = "fixed", component = "conditional", flatten = TRUE),
    p = as.data.frame(summary(model)$coefficients)$p.value
  )
}



#' @export
p_value.wbm <- function(model, ...) {
  s <- summary(model)
  p <- c(
    s$within_table[, "p"],
    s$between_table[, "p"],
    s$ints_table[, "p"]
  )
  params <- insight::get_parameters(model, effects = "fixed")

  .data_frame(
    Parameter = params$Parameter,
    p = as.vector(p),
    Component = params$Component
  )
}

#' @export
p_value.wbgee <- p_value.wbm



#' @export
p_value.gam <- function(model, ...) {
  p.table <- summary(model)$p.table
  s.table <- summary(model)$s.table

  d1 <- .data_frame(
    Parameter = rownames(p.table),
    p = as.vector(p.table[, 4]),
    Component = "conditional"
  )

  d2 <- .data_frame(
    Parameter = rownames(s.table),
    p = as.vector(s.table[, 4]),
    Component = "smooth_terms"
  )

  .remove_backticks_from_parameter_names(rbind(d1, d2))
}



#' @export
p_value.Gam <- function(model, ...) {
  p.aov <- stats::na.omit(summary(model)$parametric.anova)

  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(p.aov)),
    p = as.vector(p.aov[, 5])
  )
}



#' @export
p_value.gamm <- function(model, ...) {
  model <- model$gam
  class(model) <- c("gam", "lm", "glm")
  p_value(model)
}



#' @export
p_value.gamm4 <- p_value.gamm



#' @export
p_value.gls <- function(model, ...) {
  cs <- summary(model)$tTable
  p <- cs[, 4]
  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(cs)),
    p = as.vector(p)
  )
}



#' @export
p_value.pggls <- function(model, ...) {
  cs <- summary(model)$CoefTable
  p <- cs[, 4]
  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(cs)),
    p = as.vector(p)
  )
}



#' @export
p_value.gmnl <- function(model, ...) {
  cs <- summary(model)$CoefTable
  p <- cs[, 4]
  # se <- cs[, 2]

  pv <- .data_frame(
    Parameter = .remove_backticks_from_string(rownames(cs)),
    p = as.vector(p)
  )

  # rename intercepts
  intercepts <- grepl(":(intercept)", pv$Parameter, fixed = TRUE)
  pv$Parameter[intercepts] <- sprintf(
    "(Intercept: %s)",
    sub(":(intercept)", replacement = "", pv$Parameter[intercepts], fixed = TRUE)
  )

  pv
}



#' @export
p_value.htest <- function(model, ...) {
  model$p.value
}



#' @export
p_value.multinom <- function(model, ...) {
  stat <- insight::get_statistic(model)
  p <- 2 * stats::pnorm(abs(stat$Statistic), lower.tail = FALSE)

  .data_frame(
    Parameter = stat$Parameter,
    p = as.vector(p),
    Response = stat$Response
  )
}

#' @export
p_value.brmultinom <- p_value.multinom



#' @export
p_value.bracl <- function(model, ...) {
  smry <- suppressMessages(as.data.frame(stats::coef(summary(model))))
  p <- smry[[4]]
  names(p) <- rownames(smry)

  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    p = as.vector(p),
    Response = params$Response
  )
}



#' @export
p_value.maxLik <- function(model, ...) {
  p <- summary(model)$estimate[, 4]

  .data_frame(
    Parameter = .remove_backticks_from_string(names(p)),
    p = as.vector(p)
  )
}



#' @export
p_value.pglm <- function(model, ...) {
  p <- summary(model)$estimate[, 4]

  .data_frame(
    Parameter = .remove_backticks_from_string(names(p)),
    p = as.vector(p)
  )
}



#' @export
p_value.plm <- function(model, ...) {
  p <- stats::coef(summary(model))

  .data_frame(
    Parameter = .remove_backticks_from_string(names(p[, 4])),
    p = as.vector(p[, 4])
  )
}



#' @export
p_value.polr <- function(model, method = NULL, ...) {
  robust <- !is.null(method) && method == "robust"
  if (isTRUE(robust)) {
    return(standard_error_robust(model, ...))
  }

  smry <- suppressMessages(as.data.frame(stats::coef(summary(model))))
  tstat <- smry[[3]]
  p <- 2 * stats::pt(abs(tstat), df = degrees_of_freedom(model, method = "any"), lower.tail = FALSE)
  names(p) <- rownames(smry)

  .data_frame(
    Parameter = .remove_backticks_from_string(names(p)),
    p = as.vector(p)
  )
}



#' @export
p_value.vglm <- function(model, ...) {
  if (!requireNamespace("VGAM", quietly = TRUE)) {
    stop("Package `VGAM` required.", call. = FALSE)
  }

  cs <- VGAM::summary(model)@coef3
  p <- cs[, 4]

  .data_frame(
    Parameter = .remove_backticks_from_string(names(p)),
    p = as.vector(p)
  )
}



#' @export
p_value.vgam <- function(model, ...) {
  params <- insight::get_parameters(model)
  stat <- insight::get_statistic(model)
  p <- 2 * stats::pnorm(abs(stat$Statistic), lower.tail = FALSE)

  .data_frame(
    Parameter = .remove_backticks_from_string(stat$Parameter),
    p = as.vector(p),
    Component = params$Component
  )
}



#' @export
p_value.rma <- function(model, ...) {
  params <- insight::get_parameters(model)
  .data_frame(
    Parameter = .remove_backticks_from_string(params$Parameter),
    p = model$pval
  )
}


#' @export
p_value.lavaan <- function(model, ...) {
  out <- .extract_parameters_lavaan(model, ...)
  out[out$Operator != "~1", c("To", "Operator", "From", "p")]
}



#' @export
p_value.blavaan <- function(model, ci = .95, ...) {
  out <- .extract_parameters_blavaan(model, ...)
  out[out$Operator != "~1", c("To", "Operator", "From", "p")]
}



#' @export
p_value.bife <- function(model, ...) {
  cs <- summary(model)
  p <- cs$cm[, 4]

  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(cs$cm)),
    p = as.vector(p)
  )
}







# p-Values from standard classes ---------------------------------------------


#' @seealso https://blogs.sas.com/content/iml/2011/11/02/how-to-compute-p-values-for-a-bootstrap-distribution.html
#' @export
p_value.numeric <- function(model, ...) {
  2 * (1 - max(
    c(
      (1 + length(model[model > 0])) / (1 + length(model)),
      (1 + length(model[model < 0])) / (1 + length(model))
    )
  ))
}



#' @export
p_value.data.frame <- function(model, ...) {
  data <- model[sapply(model, is.numeric)]
  .data_frame(
    Parameter = names(data),
    p = sapply(data, p_value)
  )
}


#' @export
p_value.list <- function(model, ...) {
  if ("gam" %in% names(model)) {
    model <- model$gam
    class(model) <- c("gam", "lm", "glm")
    p_value(model)
  } else {
    insight::print_color("\nCould not extract p-values from model object.\n", "red")
  }
}






# helper --------------------------------------------------------


.get_pval_from_summary <- function(model, cs = NULL) {
  if (is.null(cs)) cs <- stats::coef(summary(model))
  p <- NULL

  if (ncol(cs) >= 4) {

    # do we have a p-value column based on t?
    pvcn <- which(colnames(cs) == "Pr(>|t|)")

    # if not, do we have a p-value column based on z?
    if (length(pvcn) == 0) {
      pvcn <- which(colnames(cs) == "Pr(>|z|)")
    }

    # if not, default to 4
    if (length(pvcn) == 0) pvcn <- 4

    p <- cs[, pvcn]

    if (is.null(names(p))) {
      coef_names <- rownames(cs)
      if (length(coef_names) == length(p)) names(p) <- coef_names
    }
  }

  names(p) <- .remove_backticks_from_string(names(p))
  p
}

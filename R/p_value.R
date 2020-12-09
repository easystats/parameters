#' p-values
#'
#' This function attempts to return, or compute, p-values of a model's parameters. See the documentation for your object's class:
#' \itemize{
#'  \item{\link[=p_value.lmerMod]{Mixed models} (\pkg{lme4}, \pkg{nlme}, \pkg{glmmTMB}, ...)}
#'  \item{\link[=p_value.brmsfit]{Bayesian models} (\pkg{rstanarm}, \pkg{brms}, \pkg{MCMCglmm}, ...)}
#'  \item{\link[=p_value.zeroinfl]{Zero-inflated models} (\code{hurdle}, \code{zeroinfl}, \code{zerocount}, ...)}
#'  \item{\link[=p_value.poissonmfx]{Marginal effects models} (\pkg{mfx})}
#'  \item{\link[=p_value.DirichletRegModel]{Models with special components} (\code{DirichletRegModel}, \code{clm2}, \code{cgam}, ...)}
#'  }
#'
#' @param model A statistical model.
#' @param method If \code{"robust"}, and if model is supported by the \pkg{sandwich} or \pkg{clubSandwich} packages, computes p-values based on robust covariance matrix estimation.
#' @param adjust Character value naming the method used to adjust p-values or confidence intervals. See \code{?emmeans::summary.emmGrid} for details.
#' @param verbose Toggle warnings and messages.
#' @param ... Arguments passed down to \code{standard_error_robust()} when confidence intervals or p-values based on robust standard errors should be computed. Only available for models where \code{method = "robust"} is supported.
#' @inheritParams simulate_model
#' @inheritParams standard_error
#' @inheritParams ci.merMod
#'
#' @note \code{p_value_robust()} resp. \code{p_value(method = "robust")}
#'   rely on the \pkg{sandwich} or \pkg{clubSandwich} package (the latter if
#'   \code{vcov_estimation = "CR"} for cluster-robust standard errors) and will
#'   thus only work for those models supported by those packages.
#'
#' @return A data frame with at least two columns: the parameter names and the p-values. Depending on the model, may also include columns for model components etc.
#'
#' @examples
#' data(iris)
#' model <- lm(Petal.Length ~ Sepal.Length + Species, data = iris)
#' p_value(model)
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
p_value.default <- function(model, method = NULL, verbose = TRUE, ...) {
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
    if (isTRUE(verbose)) {
      warning("Could not extract p-values from model object.", call. = FALSE)
    }
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



#' @export
p_value.speedlm <- function(model, ...) {
  p <- p_value.default(model, ...)
  if (!is.numeric(p$p)) {
    p$p <- tryCatch(
      {
        as.numeric(as.character(p$p))
      },
      error = function(e) {
        p$p
      }
    )
  }
  p
}





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


#' @rdname p_value
#' @export
p_value.emmGrid <- function(model, ci = .95, adjust = "none", ...) {
  s <- summary(model, level = ci, adjust = adjust)
  estimate_pos <- which(colnames(s) == model@misc$estName)

  if (length(estimate_pos)) {
    stat <- insight::get_statistic(model, ci = ci, adjust = adjust)
    p <- 2 * stats::pt(abs(stat$Statistic), df = s$df, lower.tail = FALSE)

    .data_frame(
      s[, 1:(estimate_pos - 1), drop = FALSE],
      p = as.vector(p)
    )
  } else {
    return(NULL)
  }
}


#' @export
p_value.emm_list <- function(model, ...) {
  params <- insight::get_parameters(model)
  s <- summary(model)

  # p-values
  p <- unlist(lapply(s, function(i) {
    if (is.null(i$p)) {
      rep(NA, nrow(i))
    } else {
      i$p
    }
  }))

  # result
  out <- .data_frame(
    Parameter = params$Parameter,
    p = as.vector(p),
    Component = params$Component
  )

  # any missing values?
  if (anyNA(out$p)) {

    # standard errors
    se <- unlist(lapply(s, function(i) {
      if (is.null(i$SE)) {
        rep(NA, nrow(i))
      } else {
        i$SE
      }
    }))

    # test statistic and p-values
    stat <- params$Estimate / se
    df <- degrees_of_freedom(model)
    p_val <- 2 * stats::pt(abs(stat), df = df, lower.tail = FALSE)
    out$p[is.na(out$p)] <- p_val[is.na(out$p)]
  }

  out
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
  p_column <- grep("^(Pr\\(>|p)", colnames(cs))
  p <- cs[, p_column]
  params <- insight::get_parameters(model)

  # check
  if (length(p) > nrow(params)) {
    p <- p[match(params$Parameter, .remove_backticks_from_string(rownames(cs)))]
  }

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
p_value.coxr <- function(model, ...) {
  stat <- insight::get_statistic(model)

  if (!is.null(stat)) {
    .data_frame(
      Parameter = stat$Parameter,
      p = as.vector(2 * stats::pnorm(abs(stat$Statistic), lower.tail = FALSE))
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


#' @export
p_value.mediate <- function(model, ...) {
  info <- model_info(model$model.y)
  if (info$is_linear && !model$INT) {
    out <- data.frame(
      Parameter = c("ACME", "ADE", "Total Effect", "Prop. Mediated"),
      p = c(model$d0.p, model$z0.p, model$tau.p, model$n0.p),
      stringsAsFactors = FALSE
    )
  } else {
    out <- data.frame(
      Parameter = c(
        "ACME (control)", "ACME (treated)", "ADE (control)", "ADE (treated)",
        "Total Effect", "Prop. Mediated (control)", "Prop. Mediated (treated)",
        "ACME (average)", "ADE (average)", "Prop. Mediated (average)"
      ),
      p = c(
        model$d0.p, model$d1.p, model$z0.p, model$z1.p, model$tau.p, model$n0.p,
        model$n1.p, model$d.avg.p, model$z.avg.p, model$n.avg.p
      ),
      stringsAsFactors = FALSE
    )
  }
  out
}


#' @export
p_value.margins <- function(model, ...) {
  params <- insight::get_parameters(model)
  .data_frame(
    Parameter = params$Parameter,
    p = summary(model)$p
  )
}


#' @export
p_value.lqmm <- function(model, ...) {
  out <- model_parameters(model, ...)
  as.data.frame(out[c("Parameter", "p")])
}

#' @export
p_value.lqm <- p_value.lqmm


#' @export
p_value.mipo <- function(model, ...) {
  .data_frame(
    Parameter = as.vector(summary(model)$term),
    p = as.vector(summary(model)$p.value)
  )
}


#' @export
p_value.mira <- function(model, ...) {
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop("Package 'mice' needed for this function to work. Please install it.")
  }
  p_value(mice::pool(model), ...)
}


#' @export
p_value.mle2 <- function(model, ...) {
  if (!requireNamespace("bbmle", quietly = TRUE)) {
    stop("Package `bbmle` needs to be installed to extract p-values.", call. = FALSE)
  }
  s <- bbmle::summary(model)
  .data_frame(
    Parameter = names(s@coef[, 4]),
    p = unname(s@coef[, 4])
  )
}


#' @importFrom insight find_parameters
#' @export
p_value.glht <- function(model, ...) {
  s <- summary(model)
  .data_frame(
    Parameter = insight::find_parameters(model, flatten = TRUE),
    p = unname(s$test$pvalues)
  )
}


#' @importFrom stats na.omit
#' @export
p_value.robmixglm <- function(model, ...) {
  p <- stats::na.omit(.get_pval_from_summary(model))
  .data_frame(
    Parameter = names(p),
    p = as.vector(p)
  )
}


#' @export
p_value.bayesx <- function(model, ...) {
  .data_frame(
    Parameter = find_parameters(model, component = "conditional", flatten = TRUE),
    p = model$fixed.effects[, 4]
  )
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
  p <- .get_quantreg_p(model)

  params <- insight::get_parameters(model)
  params$p <- p
  params[intersect(colnames(params), c("Parameter", "p", "Component"))]
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
  stat_col <- which(colnames(stats) %in% c("Pr(>|t|)", "Pr(>|z|)"))

  .data_frame(
    Parameter = params$Parameter,
    p = as.vector(stats[[stat_col]])
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
p_value.scam <- p_value.gam



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
  out <- .data_frame(
    Parameter = stat$Parameter,
    p = as.vector(p)
  )
  if (!is.null(stat$Response)) {
    out$Response <- stat$Response
  }
  out
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
    Parameter = .remove_backticks_from_string(rownames(p)),
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



#' @importFrom stats pchisq
#' @export
p_value.vgam <- function(model, ...) {
  stat <- insight::get_statistic(model)
  stat$p <- as.vector(stats::pchisq(stat$Statistic, df = degrees_of_freedom(model), lower.tail = FALSE))

  stat[c("Parameter", "p", "Component")]
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
p_value.metaplus <- function(model, ...) {
  out <- .data_frame(
    Parameter = .remove_backticks_from_string(rownames(model$results)),
    p = as.vector(model$results[, "pvalue"])
  )

  out$Parameter[grepl("muhat", out$Parameter)] <- "(Intercept)"
  out
}


#' @export
p_value.lavaan <- function(model, ...) {
  out <- .extract_parameters_lavaan(model, ...)
  out[out$Operator != "~1", c("To", "Operator", "From", "p")]
}



#' @export
p_value.blavaan <- function(model, ci = .95, ...) {
  out <- .extract_parameters_lavaan(model, ...)
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
p_value.list <- function(model, verbose = TRUE, ...) {
  if ("gam" %in% names(model)) {
    model <- model$gam
    class(model) <- c("gam", "lm", "glm")
    p_value(model)
  } else {
    if (isTRUE(verbose)) {
      warning("Could not extract p-values from model object.", call. = FALSE)
    }
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



#' @importFrom stats coef setNames
#' @importFrom insight get_varcov
.get_quantreg_p <- function(model) {
  p <- tryCatch(
    {
      cs <- suppressWarnings(stats::coef(summary(model)))
      cs[, "Pr(>|t|)"]
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(p)) {
    p <- tryCatch(
      {
        .get_pval_from_summary(
          model,
          cs = suppressWarnings(stats::coef(summary(model, covariance = TRUE)))
        )
      },
      error = function(e) {
        NULL
      }
    )
  }

  if (is.null(p)) {
    p <- tryCatch(
      {
        sc <- summary(model)
        if (all(unlist(lapply(sc, is.list)))) {
          list_sc <- lapply(sc, function(i) {
            .x <- as.data.frame(i)
            .x$Parameter <- rownames(.x)
            .x
          })
          out <- do.call(rbind, list_sc)
          p <- stats::setNames(out[[grep("^coefficients\\.Pr", colnames(out))]], sprintf("tau (%g)", out$tau))
        } else {
          p <- stats::setNames(unname(sc$coefficients[, 6]), names(sc$coefficients[, 6]))
        }
      },
      error = function(e) {
        NULL
      }
    )
  }

  p
}

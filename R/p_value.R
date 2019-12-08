#' p-values
#'
#' This function attempts to return, or compute, p-values of a model's parameters. The nature of the p-values is different depending on the model:
#' \itemize{
#' \item Mixed models (lme4): By default, p-values are based on Wald-test approximations (see \code{\link{p_value_wald}}). For certain situations, the "m-l-1" rule might be a better approximation. That is, for \code{method = "ml1"}, \code{\link{p_value_ml1}} is called. For \code{lmerMod} objects, if \code{method = "kenward"}, p-values are based on Kenward-Roger approximations, i.e. \code{\link{p_value_kenward}} is called, and \code{method = "satterthwaite"} calls \code{\link{p_value_satterthwaite}}.
#' }
#'
#' @param model A statistical model.
#' @param method For mixed models, can be \code{\link[=p_value_wald]{"wald"}} (default), \code{\link[=p_value_ml1]{"ml1"}}, \code{\link[=p_value_satterthwaite]{"satterthwaite"}} or \code{\link[=p_value_kenward]{"kenward"}}.
#' @param ... Arguments passed down to \code{standard_error_robust()} when confidence intervals or p-values based on robust standard errors should be computed.
#' @inheritParams model_simulate
#' @inheritParams standard_error
#'
#' @examples
#' model <- lme4::lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
#' p_value(model)
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


#' @export
p_value.default <- function(model, ...) {
  # first, we need some special handling for Zelig-models
  p <- tryCatch({
    if (grepl("^Zelig-", class(model)[1])) {
      if (!requireNamespace("Zelig", quietly = T)) {
        stop("Package `Zelig` required. Please install", call. = F)
      }
      unlist(Zelig::get_pvalue(model))
    } else {
      # try to get p-value from classical summary for default models
      .get_pval_from_summary(model)
    }
  },
  error = function(e) {
    NULL
  }
  )

  # if all fails, try to get p-value from test-statistic
  if (is.null(p)) {
    p <- tryCatch({
      stat <- insight::get_statistic(model)
      p <- 2 * stats::pnorm(abs(stat$Statistic), lower.tail = FALSE)
      names(p) <- stat$Parameter
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
p_value.tobit <- function(model, ...) {
  params <- insight::get_parameters(model)
  p <- p_value.default(model, ...)
  p[p$Parameter %in% params$Parameter, ]
}






# p-Values from Zero-Inflated Models ------------------------------------------


#' @export
p_value.zeroinfl <- function(model, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)
  if (is.null(.check_component(model, component))) {
    return(NULL)
  }

  cs <- .compact_list(stats::coef(summary(model)))
  x <- lapply(names(cs), function(i) {
    comp <- ifelse(i == "count", "conditional", "zi")
    .data_frame(
      Parameter = insight::find_parameters(model, effects = "fixed", component = comp, flatten = TRUE),
      p = as.vector(cs[[i]][, 4]),
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
  method <- match.arg(method, c("wald", "ml1", "satterthwaite", "kr", "kenward"))
  if (method == "wald") {
    p_value_wald(model, ...)
  } else if (method == "ml1") {
    p_value_ml1(model, ...)
  } else if (method == "satterthwaite") {
    p_value_satterthwaite(model, ...)
  } else if (method %in% c("kr", "kenward")) {
    p_value_kenward(model, ...)
  }
}



#' @rdname p_value
#' @export
p_value.merMod <- function(model, method = "wald", ...) {
  method <- match.arg(method, c("wald", "ml1"))
  if (method == "wald") {
    dof <- Inf
  } else {
    dof <- dof_ml1(model)
  }
  p_value_wald(model, dof, ...)
}



#' @rdname p_value
#' @export
p_value.rlmerMod <- function(model, method = "wald", ...) {
  method <- match.arg(method, c("wald", "ml1"))
  if (method == "wald") {
    dof <- Inf
  } else {
    dof <- dof_ml1(model)
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
p_value.coxph <- function(model, ...) {
  cs <- stats::coef(summary(model))
  p <- cs[, 5]

  .data_frame(
    Parameter = .remove_backticks_from_string(names(p)),
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
p_value.survreg <- function(model, ...) {
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
p_value.rq <- function(model, ...) {
  p <- tryCatch({
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
standard_error.feglm <- function(model, ...) {
  stats <- stats::coef(summary(model))
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(stats[, 4])
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



#' @export
p_value.gee <- function(model, ...) {
  cs <- stats::coef(summary(model))
  p <- 2 * stats::pt(abs(cs[, "Estimate"] / cs[, "Naive S.E."]), df = degrees_of_freedom(model, method = "any"), lower.tail = FALSE)

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
p_value.polr <- function(model, ...) {
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

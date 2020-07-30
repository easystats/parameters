#' Standard Errors
#'
#' \code{standard_error()} attempts to return standard errors of model parameters, while \code{standard_error_robust()} attempts to return robust standard errors.
#'
#' @param model A model.
#' @param force Logical, if \code{TRUE}, factors are converted to numerical
#'   values to calculate the standard error, with the lowest level being the
#'   value \code{1} (unless the factor has numeric levels, which are converted
#'   to the corresponding numeric value). By default, \code{NA} is returned
#'   for factors or character vectors.
#' @param method If \code{"robust"}, robust standard errors are computed
#'   by calling \code{\link[=standard_error_robust]{standard_error_robust()}}.
#'   \code{standard_error_robust()}, in turn, calls one of the \code{vcov*()}-functions
#'   from the \pkg{sandwich} or \pkg{clubSandwich} package for robust covariance
#'   matrix estimators. For certain mixed models, \code{method} may also be one
#'   of \code{"wald"}, \code{\link[=p_value_ml1]{"ml1"}}, \code{\link[=p_value_betwithin]{"betwithin"}},
#'   \code{\link[=p_value_satterthwaite]{"satterthwaite"}} or \code{\link[=p_value_kenward]{"kenward"}}.
#' @param verbose Toggle off warnings.
#' @param ... Arguments passed to or from other methods. For \code{standard_error()},
#'   if \code{method = "robust"}, arguments \code{vcov_estimation}, \code{vcov_type}
#'   and \code{vcov_args} can be passed down to \code{\link[=standard_error_robust]{standard_error_robust()}}.
#' @param effects Should standard errors for fixed effects or random effects
#'    be returned? Only applies to mixed models. May be abbreviated. When
#'    standard errors for random effects are requested, for each grouping factor
#'    a list of standard errors (per group level) for random intercepts and slopes
#'    is returned.
#' @inheritParams simulate_model
#'
#' @note For Bayesian models (from \pkg{rstanarm} or \pkg{brms}), the standard error is the SD of the posterior samples.
#'
#' @examples
#' model <- lm(Petal.Length ~ Sepal.Length * Species, data = iris)
#' standard_error(model)
#' @return A data frame.
#' @importFrom stats coef vcov setNames var na.omit
#' @importFrom insight get_varcov print_color get_parameters find_parameters
#' @importFrom utils capture.output
#' @export
standard_error <- function(model, ...) {
  UseMethod("standard_error")
}




# Standard objects ---------------------------------------------------------


#' @rdname standard_error
#' @export
standard_error.factor <- function(model, force = FALSE, verbose = TRUE, ...) {
  if (force) {
    standard_error(as.numeric(model), ...)
  } else {
    if (verbose) {
      warning("Can't compute standard error of non-numeric variables.", call. = FALSE)
    }
    return(NA)
  }
}


#' @export
standard_error.character <- standard_error.factor


#' @export
standard_error.numeric <- function(model, ...) {
  sqrt(stats::var(model, na.rm = TRUE) / length(stats::na.omit(model)))
}


#' @export
standard_error.data.frame <- function(model, verbose = TRUE, ...) {
  unlist(sapply(model, standard_error, verbose = verbose))
}


#' @export
standard_error.list <- function(model, ...) {
  if ("gam" %in% names(model)) {
    model <- model$gam
    class(model) <- c("gam", "lm", "glm")
    standard_error(model)
  } else {
    insight::print_color("\nCould not extract standard errors from model object.\n", "red")
  }
}


#' @export
standard_error.table <- function(model, ...) {
  # compute standard error of proportions
  if (length(dim(model)) == 1) {
    total.n <- as.vector(sum(model))
    rel.frq <- as.vector(model) / total.n

    out <- .data_frame(
      Value = names(model),
      Proportion = rel.frq,
      SE = suppressWarnings(sqrt(rel.frq * (1 - rel.frq) / total.n))
    )
  } else {
    out <- NA
  }

  out
}


#' @export
standard_error.xtabs <- standard_error.table


#' @importFrom insight print_color
#' @export
standard_error.effectsize_std_params <- function(model, ...) {
  se <- attr(model, "standard_error")

  if (is.null(se)) {
    insight::print_color("\nCould not extract standard errors of standardized coefficients.\n", "red")
    return(NULL)
  }

  out <- .data_frame(
    Parameter = model$Parameter,
    SE = as.vector(se)
  )

  .remove_backticks_from_parameter_names(out)
}


#' @export
standard_error.parameters_skewness <- function(model, ...) {
  attributes(model)$SE
}

#' @export
standard_error.parameters_kurtosis <- standard_error.parameters_skewness








# Default methods ---------------------------------------------------------


#' @rdname standard_error
#' @export
standard_error.default <- function(model, method = NULL, ...) {
  if (!is.null(method)) {
    method <- tolower(method)
  } else {
    method <- "wald"
  }

  if (method == "robust") {
    standard_error_robust(model, ...)
  } else if (method == "ml1") {
    se_ml1(model)
  } else if (method == "betwithin") {
    se_betwithin(model)
  } else {
    se <- tryCatch(
      {
        if (grepl("^Zelig-", class(model)[1])) {
          unlist(model$get_se())
        } else {
          .get_se_from_summary(model)
        }
      },
      error = function(e) {
        NULL
      }
    )

    # if all fails, try to get se from varcov
    if (is.null(se)) {
      se <- tryCatch(
        {
          varcov <- insight::get_varcov(model)
          se_from_varcov <- sqrt(diag(varcov))
          names(se_from_varcov) <- colnames(varcov)
          se_from_varcov
        },
        error = function(e) {
          NULL
        }
      )
    }


    if (is.null(se)) {
      insight::print_color("\nCould not extract standard errors from model object.\n", "red")
    } else {
      .data_frame(
        Parameter = names(se),
        SE = as.vector(se)
      )
    }
  }
}

#' @export
standard_error.truncreg <- standard_error.default

#' @export
standard_error.lm_robust <- standard_error.default

#' @export
standard_error.censReg <- standard_error.default

#' @export
standard_error.geeglm <- standard_error.default

#' @export
standard_error.negbin <- standard_error.default

#' @export
standard_error.ivreg <- standard_error.default

#' @export
standard_error.LORgee <- standard_error.default

#' @export
standard_error.lme <- standard_error.default

#' @export
standard_error.gls <- standard_error.default




#' @export
standard_error.mlm <- function(model, ...) {
  cs <- stats::coef(summary(model))
  se <- lapply(names(cs), function(x) {
    params <- cs[[x]]
    .data_frame(
      Parameter = rownames(params),
      SE = params[, "Std. Error"],
      Response = gsub("^Response (.*)", "\\1", x)
    )
  })

  .remove_backticks_from_parameter_names(do.call(rbind, se))
}


#' @export
standard_error.tobit <- function(model, ...) {
  params <- insight::get_parameters(model)
  std.error <- standard_error.default(model, ...)
  std.error[std.error$Parameter %in% params$Parameter, ]
}







# Methods that work like simple linear models ----------------------------------


#' @export
standard_error.lm <- function(model, method = NULL, ...) {
  robust <- !is.null(method) && method == "robust"

  if (isTRUE(robust)) {
    standard_error_robust(model, ...)
  } else {
    se <- .get_se_from_summary(model)
    .data_frame(
      Parameter = names(se),
      SE = as.vector(se)
    )
  }
}


#' @export
standard_error.glm <- standard_error.lm







# Mixed models ---------------------------------------------------------------


#' @rdname standard_error
#' @export
standard_error.merMod <- function(model, effects = c("fixed", "random"), method = NULL, ...) {
  effects <- match.arg(effects)
  if (is.null(method)) method <- "wald"
  robust <- !is.null(method) && method == "robust"

  if (effects == "random") {
    if (!requireNamespace("lme4", quietly = TRUE)) {
      stop("Package 'lme4' required to calculate standard errors for random effects. Please install it.")
    }

    rand.se <- lme4::ranef(model, condVar = TRUE)
    n.groupings <- length(rand.se)

    for (m in 1:n.groupings) {
      vars.m <- attr(rand.se[[m]], "postVar")

      K <- dim(vars.m)[1]
      J <- dim(vars.m)[3]

      names.full <- dimnames(rand.se[[m]])
      rand.se[[m]] <- array(NA, c(J, K))

      for (j in 1:J) {
        rand.se[[m]][j, ] <- sqrt(diag(as.matrix(vars.m[, , j])))
      }
      dimnames(rand.se[[m]]) <- list(names.full[[1]], names.full[[2]])
    }
    rand.se
  } else {
    if (isTRUE(robust)) {
      standard_error_robust(model, ...)
    } else {
      # Classic and Satterthwaite SE
      if (method %in% c("wald", "satterthwaite")) {
        .data_frame(
          Parameter = insight::find_parameters(model, effects = "fixed", component = "conditional", flatten = TRUE),
          SE = .get_se_from_summary(model)
        )
        # ml1 approx
      } else if (method == "ml1") {
        se_ml1(model)
      } else if (method == "betwithin") {
        se_betwithin(model)
        # Kenward approx
      } else if (method %in% c("kenward", "kr")) {
        se_kenward(model)
      }
    }
  }
}



#' @rdname standard_error
#' @export
standard_error.glmmTMB <- function(model, effects = c("fixed", "random"), component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), ...) {
  component <- match.arg(component)
  effects <- match.arg(effects)

  if (effects == "random") {
    if (requireNamespace("TMB", quietly = TRUE) && requireNamespace("glmmTMB", quietly = TRUE)) {
      s1 <- TMB::sdreport(model$obj, getJointPrecision = TRUE)
      s2 <- sqrt(s1$diag.cov.random)
      rand.ef <- glmmTMB::ranef(model)[[1]]
      rand.se <- lapply(rand.ef, function(.x) {
        cnt <- nrow(.x) * ncol(.x)
        s3 <- s2[1:cnt]
        s2 <- s2[-(1:cnt)]
        d <- as.data.frame(matrix(sqrt(s3), ncol = ncol(.x), byrow = TRUE))
        colnames(d) <- colnames(.x)
        d
      })
      rand.se
    } else {
      return(NULL)
    }
  } else {
    if (is.null(.check_component(model, component))) {
      return(NULL)
    }

    cs <- .compact_list(stats::coef(summary(model)))
    x <- lapply(names(cs), function(i) {
      .data_frame(
        Parameter = insight::find_parameters(model, effects = "fixed", component = i, flatten = TRUE),
        SE = as.vector(cs[[i]][, 2]),
        Component = i
      )
    })

    se <- do.call(rbind, x)
    se$Component <- .rename_values(se$Component, "cond", "conditional")
    se$Component <- .rename_values(se$Component, "zi", "zero_inflated")
    se$Component <- .rename_values(se$Component, "disp", "dispersion")

    .filter_component(se, component)
  }
}



#' @rdname standard_error
#' @importFrom insight find_random
#' @export
standard_error.MixMod <- function(model, effects = c("fixed", "random"), component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)
  effects <- match.arg(effects)

  if (effects == "random") {
    if (!requireNamespace("lme4", quietly = TRUE)) {
      stop("Package 'lme4' required to calculate standard errors for random effects. Please install it.")
    }
    rand.se <- lme4::ranef(model, post_vars = TRUE)
    vars.m <- attr(rand.se, "post_vars")
    all_names <- attributes(rand.se)$dimnames

    if (dim(vars.m[[1]])[1] == 1) {
      rand.se <- sqrt(unlist(vars.m))
    } else {
      rand.se <- do.call(
        rbind,
        lapply(vars.m, function(.x) t(as.data.frame(sqrt(diag(.x)))))
      )
      rownames(rand.se) <- all_names[[1]]
      colnames(rand.se) <- all_names[[2]]
      rand.se <- list(rand.se)
      names(rand.se) <- insight::find_random(model, flatten = TRUE)
    }
    rand.se
  } else {
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
        SE = as.vector(cs[[i]][, 2]),
        Component = i
      )
    })

    se <- do.call(rbind, x)
    .filter_component(se, component)
  }
}






# Zero-inflated models --------------------------------------------------------


#' @rdname standard_error
#' @export
standard_error.zeroinfl <- function(model, component = c("all", "conditional", "zi", "zero_inflated"), method = NULL, ...) {
  component <- match.arg(component)
  if (is.null(.check_component(model, component))) {
    return(NULL)
  }

  robust <- !is.null(method) && method == "robust"
  if (isTRUE(robust)) {
    return(standard_error_robust(model, ...))
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
      SE = as.vector(stats[, 2]),
      Component = comp
    )
  })

  se <- do.call(rbind, x)
  se$Component <- .rename_values(se$Component, "cond", "conditional")
  se$Component <- .rename_values(se$Component, "zi", "zero_inflated")

  .filter_component(se, component)
}


#' @export
standard_error.hurdle <- standard_error.zeroinfl

#' @export
standard_error.zerocount <- standard_error.zeroinfl





# ANOVA ---------------------------------------------------------------


#' @export
standard_error.aov <- function(model, ...) {
  params <- model_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    SE = params$SE
  )
}


#' @export
standard_error.anova <- standard_error.aov

#' @export
standard_error.aovlist <- standard_error.aov






# Survey models ---------------------------------------------------------------


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
  cs <- stats::coef(summary(model))
  se <- cs[, 2]

  .data_frame(
    Parameter = .remove_backticks_from_string(names(se)),
    SE = as.vector(se)
  )
}







# Survival Models -------------------------------------------------------


#' @export
standard_error.coxme <- function(model, ...) {
  beta <- model$coefficients

  if (length(beta) > 0) {
    .data_frame(
      Parameter = .remove_backticks_from_string(names(beta)),
      SE = sqrt(diag(stats::vcov(model)))
    )
  }
}



#' @rdname standard_error
#' @export
standard_error.coxph <- function(model, method = NULL, ...) {
  robust <- !is.null(method) && method == "robust"
  if (isTRUE(robust)) {
    return(standard_error_robust(model, ...))
  }

  params <- insight::get_parameters(model)
  cs <- stats::coef(summary(model))
  se <- cs[, 3]

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(se)
  )
}



#' @export
standard_error.survreg <- function(model, method = NULL, ...) {
  robust <- !is.null(method) && method == "robust"
  if (isTRUE(robust)) {
    return(standard_error_robust(model, ...))
  }

  s <- summary(model)
  se <- s$table[, 2]

  .data_frame(
    Parameter = .remove_backticks_from_string(names(se)),
    SE = as.vector(se)
  )
}



#' @export
standard_error.flexsurvreg <- function(model, ...) {
  params <- insight::find_parameters(model, flatten = TRUE)
  se <- model$res[rownames(model$res) %in% params, "se"]

  .data_frame(
    Parameter = .remove_backticks_from_string(names(se)),
    SE = as.vector(se)
  )
}


#' @export
standard_error.aareg <- function(model, ...) {
  s <- summary(model)
  se <- s$table[, "se(coef)"]

  .data_frame(
    Parameter = .remove_backticks_from_string(names(se)),
    SE = as.vector(se)
  )
}









# Ordinal Models ---------------------------------------------------------


#' @export
standard_error.multinom <- function(model, ...) {
  se <- tryCatch(
    {
      stderr <- summary(model)$standard.errors
      if (is.null(stderr)) {
        vc <- insight::get_varcov(model)
        stderr <- as.vector(sqrt(diag(vc)))
      } else {
        if (is.matrix(stderr)) {
          tmp <- c()
          for (i in 1:nrow(stderr)) {
            tmp <- c(tmp, as.vector(stderr[i, ]))
          }
        } else {
          tmp <- as.vector(stderr)
        }
        stderr <- tmp
      }
      stderr
    },
    error = function(e) {
      vc <- insight::get_varcov(model)
      as.vector(sqrt(diag(vc)))
    }
  )

  params <- insight::get_parameters(model)

  if ("Response" %in% colnames(params)) {
    .data_frame(
      Parameter = params$Parameter,
      SE = se,
      Response = params$Response
    )
  } else {
    .data_frame(
      Parameter = params$Parameter,
      SE = se
    )
  }
}


#' @export
standard_error.brmultinom <- standard_error.multinom


#' @export
standard_error.polr <- function(model, method = NULL, ...) {
  robust <- !is.null(method) && method == "robust"
  if (isTRUE(robust)) {
    return(standard_error_robust(model, ...))
  }

  smry <- suppressMessages(as.data.frame(stats::coef(summary(model))))
  se <- smry[[2]]
  names(se) <- rownames(smry)

  .data_frame(
    Parameter = .remove_backticks_from_string(names(se)),
    SE = as.vector(se)
  )
}


#' @rdname standard_error
#' @importFrom insight get_parameters
#' @export
standard_error.mixor <- function(model, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  stats <- model$Model[, "Std. Error"]
  parms <- insight::get_parameters(model, effects = effects)

  .data_frame(
    Parameter = parms$Parameter,
    SE = stats[parms$Parameter],
    Effects = parms$Effects
  )
}



#' @importFrom insight get_parameters get_varcov
#' @export
standard_error.glmm <- function(model, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  s <- summary(model)

  out <- insight::get_parameters(model, effects = "all")
  out$SE <- sqrt(diag(insight::get_varcov(model, effects = "all")))
  out <- out[, c("Parameter", "SE", "Effects")]

  if (effects != "all") {
    out <- out[out$Effects == effects, , drop = FALSE]
    out$Effects <- NULL
  }

  out
}



#' @rdname standard_error
#' @importFrom insight get_parameters
#' @export
standard_error.clm2 <- function(model, component = c("all", "conditional", "scale"), ...) {
  component <- match.arg(component)
  stats <- .get_se_from_summary(model)
  parms <- insight::get_parameters(model, component = component)

  .data_frame(
    Parameter = parms$Parameter,
    SE = stats[parms$Parameter],
    Component = parms$Component
  )
}


#' @export
standard_error.clmm2 <- standard_error.clm2



#' @export
standard_error.bracl <- function(model, ...) {
  smry <- suppressMessages(as.data.frame(stats::coef(summary(model))))
  se <- smry[[2]]
  names(se) <- rownames(smry)

  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(se),
    Response = params$Response
  )
}








# Bayesian ----------------------------------------------


#' @export
standard_error.stanreg <- function(model, effects = c("fixed", "random"), component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  effects <- match.arg(effects)
  component <- match.arg(component)

  params <- insight::get_parameters(model, effects = effects, component = component, ...)

  .data_frame(
    Parameter = colnames(params),
    SE = unname(sapply(params, stats::sd, na.rm = TRUE))
  )
}

#' @export
standard_error.brmsfit <- standard_error.stanreg

#' @export
standard_error.mvstanreg <- standard_error.stanreg

#' @export
standard_error.bayesx <- function(model, ...) {
  .data_frame(
    Parameter = find_parameters(model, component = "conditional", flatten = TRUE),
    SE = model$fixed.effects[, 2]
  )
}









# Other models ---------------------------------------------------------------


#' @export
standard_error.glht <- function(model, ...) {
  s <- summary(model)
  .data_frame(
    Parameter = insight::find_parameters(model, flatten = TRUE),
    SE = unname(s$test$sigma)
  )
}


#' @export
standard_error.sem <- function(model, ...) {
  if (!.is_semLme(model)) {
    return(NULL)
  }
  if (is.null(model$se)) {
    warning("Model has no standard errors. Please fit model again with bootstrapped standard errors.", call. = FALSE)
    return(NULL)
  }
  .data_frame(
    Parameter = names(model$se),
    SE = unname(model$se)
  )
}


#' @importFrom stats na.omit
#' @export
standard_error.robmixglm <- function(model, ...) {
  se <- stats::na.omit(.get_se_from_summary(model))
  .data_frame(
    Parameter = names(se),
    SE = as.vector(se)
  )
}


#' @export
standard_error.bife <- function(model, ...) {
  cs <- summary(model)
  se <- cs$cm[, 2]

  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(cs$cm)),
    SE = as.vector(se)
  )
}



#' @export
standard_error.cgam <- function(model, ...) {
  sc <- summary(model)
  se <- as.vector(sc$coefficients[, "StdErr"])

  params <- insight::get_parameters(model, component = "all")

  if (!is.null(sc$coefficients2)) se <- c(se, rep(NA, nrow(sc$coefficients2)))

  .data_frame(
    Parameter = params$Parameter,
    SE = se,
    Component = params$Component
  )
}



#' @importFrom utils capture.output
#' @export
standard_error.cpglm <- function(model, ...) {
  if (!requireNamespace("cplm", quietly = TRUE)) {
    stop("To use this function, please install package 'cplm'.")
  }

  junk <- utils::capture.output(stats <- cplm::summary(model)$coefficients)
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(stats[, "Std. Error"])
  )
}



#' @importFrom utils capture.output
#' @export
standard_error.zcpglm <- function(model, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  if (!requireNamespace("cplm", quietly = TRUE)) {
    stop("To use this function, please install package 'cplm'.")
  }

  component <- match.arg(component)
  junk <- utils::capture.output(stats <- cplm::summary(model)$coefficients)
  params <- insight::get_parameters(model)

  tweedie <- .data_frame(
    Parameter = params$Parameter[params$Component == "conditional"],
    SE = as.vector(stats$tweedie[, "Std. Error"]),
    Component = "conditional"
  )

  zero <- .data_frame(
    Parameter = params$Parameter[params$Component == "zero_inflated"],
    SE = as.vector(stats$zero[, "Std. Error"]),
    Component = "zero_inflated"
  )

  out <- .filter_component(rbind(tweedie, zero), component)
  out
}



#' @export
standard_error.cpglmm <- function(model, ...) {
  if (!requireNamespace("cplm", quietly = TRUE)) {
    stop("To use this function, please install package 'cplm'.")
  }

  stats <- cplm::summary(model)$coefs
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(stats[, "Std. Error"])
  )
}



#' @export
standard_error.rq <- function(model, ...) {
  se <- .get_quantreg_se(model)
  if (is.null(se)) {
    vc <- insight::get_varcov(model)
    se <- as.vector(sqrt(diag(vc)))
  }

  params <- insight::get_parameters(model)
  params$SE <- se
  params[intersect(colnames(params), c("Parameter", "SE", "Component"))]
}

#' @export
standard_error.crq <- standard_error.rq

#' @export
standard_error.nlrq <- standard_error.rq

#' @export
standard_error.rqss <- function(model, component = c("all", "conditional", "smooth_terms"), ...) {
  component <- match.arg(component)

  cs <- summary(model)$coef
  se_column <- intersect(c("Std Error", "Std. Error"), colnames(cs))
  se <- cs[, se_column]

  params_cond <- insight::get_parameters(model, component = "conditional")
  params_smooth <- insight::get_parameters(model, component = "smooth_terms")

  out_cond <- .data_frame(
    Parameter = params_cond$Parameter,
    SE = se,
    Component = "conditional"
  )

  out_smooth <- .data_frame(
    Parameter = params_smooth$Parameter,
    SE = NA,
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
standard_error.complmrob <- function(model, ...) {
  stats <- summary(model)$stats
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(stats[, "Std. Error"])
  )
}



#' @export
standard_error.glmx <- function(model, ...) {
  stats <- stats::coef(summary(model))
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    SE = c(as.vector(stats$glm[, "Std. Error"]), as.vector(stats$extra[, "Std. Error"])),
    Component = params$Component
  )
}



#' @export
standard_error.fixest <- function(model, ...) {
  stats <- summary(model)
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(stats$se)
  )
}



#' @export
standard_error.feglm <- function(model, ...) {
  stats <- stats::coef(summary(model))
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(stats[, "Std. error"])
  )
}



#' @export
standard_error.biglm <- function(model, ...) {
  cs <- summary(model)$mat
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(cs[, 4])
  )
}



#' @export
standard_error.crch <- function(model, ...) {
  cs <- do.call(rbind, stats::coef(summary(model), model = "full"))
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(cs[, 2])
  )
}



#' @export
standard_error.gee <- function(model, method = NULL, ...) {
  cs <- stats::coef(summary(model))
  robust <- !is.null(method) && method == "robust"

  if (isTRUE(robust)) {
    se <- as.vector(cs[, "Robust S.E."])
  } else {
    se <- as.vector(cs[, "Naive S.E."])
  }

  .data_frame(Parameter = .remove_backticks_from_string(rownames(cs)), SE = se)
}



#' @export
standard_error.logistf <- function(model, ...) {
  utils::capture.output(s <- summary(model))
  se <- sqrt(diag(s$var))

  .data_frame(
    Parameter = .remove_backticks_from_string(names(s$coefficients)),
    SE = as.vector(se)
  )
}



#' @export
standard_error.glimML <- function(model, ...) {
  if (!requireNamespace("aod", quietly = TRUE)) {
    stop("Package 'aod' required for this function to work. Please install it.")
  }

  s <- methods::slot(aod::summary(model), "Coef")
  se <- s[, 2]

  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(s)),
    SE = as.vector(se)
  )
}



#' @export
standard_error.lrm <- function(model, ...) {
  se <- sqrt(diag(stats::vcov(model)))

  # psm-models returns vcov-matrix w/o dimnames
  if (is.null(names(se))) names(se) <- names(stats::coef(model))

  .data_frame(
    Parameter = .remove_backticks_from_string(names(se)),
    SE = as.vector(se)
  )
}

#' @export
standard_error.ols <- standard_error.lrm

#' @export
standard_error.rms <- standard_error.lrm

#' @export
standard_error.psm <- standard_error.lrm



#' @rdname standard_error
#' @export
standard_error.betareg <- function(model, component = c("all", "conditional", "precision"), ...) {
  component <- match.arg(component)

  params <- insight::get_parameters(model)
  cs <- do.call(rbind, stats::coef(summary(model)))
  se <- cs[, 2]

  out <- .data_frame(
    Parameter = .remove_backticks_from_string(names(se)),
    Component = params$Component,
    SE = as.vector(se)
  )

  if (component != "all") {
    out <- out[out$Component == component, ]
  }

  out
}


#' @rdname standard_error
#' @export
standard_error.DirichletRegModel <- function(model, component = c("all", "conditional", "precision"), ...) {
  component <- match.arg(component)
  params <- insight::get_parameters(model)

  out <- .data_frame(
    Parameter = params$Parameter,
    Response = params$Response,
    SE = as.vector(model$se)
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



#' @export
standard_error.gamlss <- function(model, ...) {
  parms <- insight::get_parameters(model)
  utils::capture.output(cs <- summary(model))

  .data_frame(
    Parameter = parms$Parameter,
    SE = as.vector(cs[, 2]),
    Component = parms$Component
  )
}



#' @export
standard_error.plm <- function(model, ...) {
  se <- stats::coef(summary(model))

  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(se)),
    SE = as.vector(se[, 2])
  )
}


#' @export
standard_error.gam <- function(model, ...) {
  p.table <- summary(model)$p.table
  s.table <- summary(model)$s.table
  n_cond <- nrow(p.table)
  n_smooth <- nrow(s.table)

  .data_frame(
    Parameter = .remove_backticks_from_string(c(rownames(p.table), rownames(s.table))),
    SE = c(as.vector(p.table[, 2]), rep(NA, n_smooth)),
    Component = c(rep("conditional", n_cond), rep("smooth_terms", n_smooth))
  )
}


#' @export
standard_error.gamm <- function(model, ...) {
  model <- model$gam
  class(model) <- c("gam", "lm", "glm")
  standard_error(model)
}


#' @export
standard_error.gamm4 <- standard_error.gamm


#' @export
standard_error.MCMCglmm <- function(model, ...) {
  nF <- model$Fixed$nfl
  parms <- as.data.frame(model$Sol[, 1:nF, drop = FALSE])

  .data_frame(
    Parameter = .remove_backticks_from_string(colnames(parms)),
    SE = unname(sapply(parms, stats::sd))
  )
}



#' @export
standard_error.BBmm <- function(model, ...) {
  .data_frame(
    Parameter = insight::find_parameters(model, effects = "fixed", component = "conditional", flatten = TRUE),
    SE = as.data.frame(summary(model)$fixed.coefficients)$StdErr
  )
}



#' @export
standard_error.BBreg <- function(model, ...) {
  .data_frame(
    Parameter = insight::find_parameters(model, effects = "fixed", component = "conditional", flatten = TRUE),
    SE = as.data.frame(summary(model)$coefficients)$StdErr
  )
}



#' @export
standard_error.wbm <- function(model, ...) {
  s <- summary(model)
  se <- c(
    s$within_table[, "S.E."],
    s$between_table[, "S.E."],
    s$ints_table[, "S.E."]
  )
  params <- insight::get_parameters(model, effects = "fixed")

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(se),
    Component = params$Component
  )
}



#' @export
standard_error.wbgee <- standard_error.wbm


#' @export
standard_error.htest <- function(model, ...) {
}


#' @importFrom insight get_varcov
#' @export
standard_error.vglm <- function(model, ...) {
  se <- sqrt(diag(insight::get_varcov(model)))
  .data_frame(
    Parameter = .remove_backticks_from_string(names(se)),
    SE = as.vector(se)
  )
}

#' @importFrom insight get_varcov
#' @export
standard_error.vgam <- function(model, ...) {
  params <- insight::get_parameters(model)
  se <- sqrt(diag(insight::get_varcov(model)))
  .data_frame(
    Parameter = .remove_backticks_from_string(names(se)),
    SE = as.vector(se),
    Component = params$Component
  )
}


#' @export
standard_error.gmnl <- function(model, ...) {
  cs <- summary(model)$CoefTable
  se <- cs[, 2]

  pv <- .data_frame(
    Parameter = .remove_backticks_from_string(names(se)),
    SE = as.vector(se)
  )

  # rename intercepts
  intercepts <- grepl(":(intercept)", pv$Parameter, fixed = TRUE)
  pv$Parameter[intercepts] <- sprintf(
    "(Intercept: %s)",
    sub(":(intercept)", replacement = "", pv$Parameter[intercepts], fixed = TRUE)
  )

  pv
}






# mfx models -----------------------------

#' @export
standard_error.logitor <- function(model, ...) {
  standard_error.lm(model$fit, ...)
}

#' @export
standard_error.poissonirr <- standard_error.logitor

#' @export
standard_error.negbinirr <- standard_error.logitor

#' @rdname standard_error
#' @export
standard_error.poissonmfx <- function(model, component = c("all", "conditional", "marginal"), ...) {
  parms <- insight::get_parameters(model, component = "all")
  cs <- stats::coef(summary(model$fit))
  se <- c(as.vector(model$mfxest[, 2]), as.vector(cs[, 2]))

  out <- .data_frame(
    Parameter = parms$Parameter,
    SE = se,
    Component = parms$Component
  )

  component <- match.arg(component)
  if (component != "all") {
    out <- out[out$Component == component, ]
  }

  out
}

#' @export
standard_error.logitmfx <- standard_error.poissonmfx

#' @export
standard_error.probitmfx <- standard_error.poissonmfx

#' @export
standard_error.negbinmfx <- standard_error.poissonmfx

#' @export
standard_error.betaor <- function(model, component = c("all", "conditional", "precision"), ...) {
  component = match.arg(component)
  standard_error.betareg(model$fit, component = component, ...)
}

#' @rdname standard_error
#' @export
standard_error.betamfx <- function(model, component = c("all", "conditional", "precision", "marginal"), ...) {
  parms <- insight::get_parameters(model, component = "all")
  cs <- do.call(rbind, stats::coef(summary(model$fit)))
  se <- c(as.vector(model$mfxest[, 2]), as.vector(cs[, 2]))

  out <- .data_frame(
    Parameter = parms$Parameter,
    SE = se,
    Component = parms$Component
  )

  component <- match.arg(component)
  if (component != "all") {
    out <- out[out$Component == component, ]
  }

  out
}







# Special classes and models -----------------------------


#' @export
standard_error.emmGrid <- function(model, ...) {
  s <- summary(model)
  estimate_pos <- which(colnames(s) == model@misc$estName)

  if (length(estimate_pos)) {
    .data_frame(
      s[, 1:(estimate_pos - 1), drop = FALSE],
      SE = s$SE
    )
  } else {
    return(NULL)
  }
}


#' @export
standard_error.rma <- function(model, ...) {
  params <- insight::get_parameters(model)
  .data_frame(
    Parameter = .remove_backticks_from_string(params$Parameter),
    SE = model[["se"]]
  )
}



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



#' @rdname standard_error
#' @export
standard_error.averaging <- function(model, component = c("conditional", "full"), ...) {
  component <- match.arg(component)
  params <- insight::get_parameters(model, component = component)
  if (component == "full") {
    s <- summary(model)$coefmat.full
  } else {
    s <- summary(model)$coefmat.subset
  }

  .data_frame(
    Parameter = .remove_backticks_from_string(params$Parameter),
    SE = as.vector(s[, 3])
  )
}



#' @export
standard_error.lavaan <- function(model, ...) {
  out <- .extract_parameters_lavaan(model, ...)
  out[out$Operator != "~1", c("To", "Operator", "From", "SE")]
}



#' @export
standard_error.blavaan <- function(model, ci = .95, ...) {
  out <- .extract_parameters_blavaan(model, ...)
  out[out$Operator != "~1", c("To", "Operator", "From", "SE")]
}








# helper -----------------------------------------------------------------


.get_se_from_summary <- function(model, component = NULL) {
  cs <- stats::coef(summary(model))
  se <- NULL

  if (is.list(cs) && !is.null(component)) cs <- cs[[component]]

  if (!is.null(cs)) {
    # do we have a se column?
    se_col <- which(colnames(cs) == "Std. Error")

    # if not, default to 2
    if (length(se_col) == 0) se_col <- 2

    se <- as.vector(cs[, se_col])

    if (is.null(names(se))) {
      coef_names <- rownames(cs)
      if (length(coef_names) == length(se)) names(se) <- coef_names
    }
  }

  names(se) <- .remove_backticks_from_string(names(se))
  se
}


#' @importFrom stats coef setNames
#' @importFrom insight get_varcov
.get_quantreg_se <- function(model) {
  se <- tryCatch(
    {
      cs <- suppressWarnings(stats::coef(summary(model)))
      se_column <- intersect(c("Std Error", "Std. Error"), colnames(cs))
      if (length(se_column)) {
        cs[, se_column]
      } else {
        vc <- insight::get_varcov(model)
        as.vector(sqrt(diag(vc)))
      }
    },
    error = function(e) { NULL }
  )

  if (is.null(se)) {
    se <- tryCatch(
      {
        sc <- summary(model)
        if (all(unlist(lapply(sc, is.list)))) {
          list_sc <- lapply(sc, function(i) {
            .x <- as.data.frame(i)
            .x$Parameter <- rownames(.x)
            .x
          })
          out <- do.call(rbind, list_sc)
          se <- stats::setNames(out$coefficients.Std.Error, sprintf("tau (%g)", out$tau))
        } else {
          se <- stats::setNames(unname(sc$coefficients[, 4]), names(sc$coefficients[, 4]))
        }
      },
      error = function(e) { NULL }
    )
  }
  se
}


# .ranef_se <- function(x) {
#   if (!requireNamespace("lme4", quietly = TRUE)) {
#     stop("Package 'lme4' required for this function to work. Please install it by running `install.packages('lme4')`.")
#   }
#
#   cc <- stats::coef(model)
#
#   # get names of intercepts
#   inames <- names(cc)
#
#   # variances of fixed effects
#   fixed.vars <- diag(as.matrix(stats::vcov(model)))
#
#   # extract variances of conditional modes
#   r1 <- lme4::ranef(model, condVar = TRUE)
#
#   # we may have multiple random intercepts, iterate all
#   se.merMod <- lapply(1:length(cc), function(i) {
#     cmode.vars <- t(apply(attr(r1[[i]], "postVar"), 3, diag))
#     seVals <- sqrt(sweep(cmode.vars, 2, fixed.vars[names(r1[[i]])], "+", check.margin = FALSE))
#
#     if (length(r1[[i]]) == 1) {
#       seVals <- as.data.frame(t(seVals))
#       stats::setNames(seVals, names(r1[[i]]))
#     } else {
#       seVals <- seVals[, 1:2]
#       stats::setNames(as.data.frame(seVals), names(r1[[i]]))
#     }
#   })
#
#   # set names of list
#   names(se.merMod) <- inames
#
#   se.merMod
# }

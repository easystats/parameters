#' Extract standard errors
#'
#' This function attempts to return standard errors of model parameters.
#'
#' @param model A model.
#' @param force Logical, if \code{TRUE}, factors are converted to numerical
#'   values to calculate the standard error, with the lowest level being the
#'   value \code{1} (unless the factor has numeric levels, which are converted
#'   to the corresponding numeric value). By default, \code{NA} is returned
#'   for factors or character vectors.
#' @param verbose Toggle off warnings.
#' @param ... Arguments passed to or from other methods.
#' @inheritParams model_simulate
#'
#' @examples
#' model <- lm(Petal.Length ~ Sepal.Length * Species, data = iris)
#' standard_error(model)
#' @return A data frame.
#' @importFrom stats coef vcov setNames
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


#' @importFrom stats var na.omit
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





# Default methods ---------------------------------------------------------


#' @export
standard_error.default <- function(model, ...) {
  se <- tryCatch({
    if (grepl("^Zelig-", class(model)[1])) {
      if (!requireNamespace("Zelig", quietly = TRUE)) {
        stop("Package `Zelig` required. Please install", call. = FALSE)
      }
      unlist(Zelig::get_se(model))
    } else {
      .get_se_from_summary(model)
    }
  },
  error = function(e) {
    NULL
  }
  )

  if (is.null(se)) {
    insight::print_color("\nCould not extract standard errors from model object.\n", "red")
  } else {
    .data_frame(
      Parameter = names(se),
      SE = as.vector(se)
    )
  }
}


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

  do.call(rbind, se)
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
standard_error.tobit <- function(model, ...) {
  params <- insight::get_parameters(model)
  std.error <- standard_error.default(model, ...)
  ## TODO change to "$Parameter" once fixed in insight
  std.error[std.error$Parameter %in% params[[1]], ]
}







# Methods that work like simple linear models ----------------------------------


#' @export
standard_error.lm <- function(model, ...) {
  .data_frame(
    Parameter = insight::find_parameters(model, effects = "fixed", component = "conditional", flatten = TRUE),
    SE = .get_se_from_summary(model)
  )
}


#' @export
standard_error.glm <- standard_error.lm


#' @export
standard_error.merMod <- standard_error.lm






# Mixed models ---------------------------------------------------------------


#' @rdname standard_error
#' @export
standard_error.glmmTMB <- function(model, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)
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

  .filter_component(se, component)
}



#' @rdname standard_error
#' @export
standard_error.MixMod <- function(model, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
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
      SE = as.vector(cs[[i]][, 2]),
      Component = i
    )
  })

  se <- do.call(rbind, x)
  .filter_component(se, component)
}






# Zero-inflated models --------------------------------------------------------


#' @export
standard_error.zeroinfl <- function(model, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)
  if (is.null(.check_component(model, component))) {
    return(NULL)
  }

  cs <- .compact_list(stats::coef(summary(model)))
  x <- lapply(names(cs), function(i) {
    comp <- ifelse(i == "count", "conditional", "zi")
    .data_frame(
      Parameter = insight::find_parameters(model, effects = "fixed", component = comp, flatten = TRUE),
      SE = as.vector(cs[[i]][, 2]),
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

  data.frame(
    Parameter = params$Parameter,
    SE = params$SE,
    stringsAsFactors = FALSE
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
    Parameter = names(se),
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
    Parameter = names(se),
    SE = as.vector(se)
  )
}






# Other models ---------------------------------------------------------------


#' @importFrom insight get_varcov
#' @export
standard_error.rq <- function(model, ...) {
  se <- tryCatch({
    cs <- suppressWarnings(stats::coef(summary(model)))
    se_column <- intersect(c("Std Error", "Std. Error"), colnames(cs))
    if (length(se_column)) {
      cs[, se_column]
    } else {
      vc <- insight::get_varcov(model)
      as.vector(sqrt(diag(vc)))
    }
  },
  error = function(e) {
    vc <- insight::get_varcov(model)
    as.vector(sqrt(diag(vc)))
  }
  )

  params <- insight::get_parameters(model)

  .data_frame(
    ## TODO change to "$Parameter" once fixed in insight
    Parameter = params[[1]],
    SE = se
  )
}

#' @export
standard_error.crq <- standard_error.rq

#' @export
standard_error.nlrq <- standard_error.rq


#' @export
standard_error.biglm <- function(model, ...) {
  cs <- summary(model)$mat
  params <- insight::get_parameters(model)

  .data_frame(
    ## TODO change to "$Parameter" once fixed in insight
    Parameter = params[[1]],
    SE = as.vector(cs[, 4])
  )
}


#' @export
standard_error.crch <- function(model, ...) {
  cs <- do.call(rbind, stats::coef(summary(model), model = "full"))
  params <- insight::get_parameters(model)

  .data_frame(
    ## TODO change to "$Parameter" once fixed in insight
    Parameter = params[[1]],
    SE = as.vector(cs[, 2])
  )
}


#' @export
standard_error.gee <- function(model, ...) {
  cs <- stats::coef(summary(model))

  .data_frame(
    Parameter = rownames(cs),
    SE = as.vector(cs[, "Naive S.E."])
  )
}



#' @export
standard_error.logistf <- function(model, ...) {
  utils::capture.output(s <- summary(model))
  se <- sqrt(diag(s$var))

  .data_frame(
    Parameter = names(s$coefficients),
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
    Parameter = rownames(s),
    SE = as.vector(se)
  )
}



#' @importFrom stats vcov
#' @export
standard_error.lrm <- function(model, ...) {
  se <- sqrt(diag(stats::vcov(model)))

  # psm-models returns vcov-matrix w/o dimnames
  if (is.null(names(se))) names(se) <- names(stats::coef(model))

  .data_frame(
    Parameter = names(se),
    SE = as.vector(se)
  )
}

#' @export
standard_error.ols <- standard_error.lrm

#' @export
standard_error.rms <- standard_error.lrm

#' @export
standard_error.psm <- standard_error.lrm



#' @export
standard_error.betareg <- function(model, ...) {
  cs <- do.call(rbind, stats::coef(summary(model)))
  se <- cs[, 2]

  .data_frame(
    Parameter = names(se),
    SE = as.vector(se)
  )
}



#' @importFrom utils capture.output
#' @export
standard_error.gamlss <- function(model, ...) {
  parms <- insight::get_parameters(model)
  utils::capture.output(cs <- summary(model))

  .data_frame(
    ## TODO change to "$Parameter" and "$Component" once fixed in insight
    Parameter = parms[[1]],
    SE = as.vector(cs[, 2]),
    Component = parms[[3]]
  )
}



#' @export
standard_error.plm <- function(model, ...) {
  se <- stats::coef(summary(model))

  .data_frame(
    Parameter = names(se[, 2]),
    SE = as.vector(se[, 2])
  )
}



#' @export
standard_error.coxme <- function(model, ...) {
  beta <- model$coefficients

  if (length(beta) > 0) {
    .data_frame(
      Parameter = names(beta),
      SE = sqrt(diag(stats::vcov(model)))
    )
  }
}



#' @export
standard_error.coxph <- function(model, ...) {
  cs <- stats::coef(summary(model))
  se <- cs[, 3]

  .data_frame(
    Parameter = names(se),
    SE = as.vector(se)
  )
}



#' @export
standard_error.survreg <- function(model, ...) {
  s <- summary(model)
  se <- s$table[, 2]

  .data_frame(
    Parameter = names(se),
    SE = as.vector(se)
  )
}



#' @export
standard_error.flexsurvreg <- function(model, ...) {
  params <- insight::find_parameters(model, flatten = TRUE)
  se <- model$res[rownames(model$res) %in% params, "se"]

  .data_frame(
    Parameter = names(se),
    SE = as.vector(se)
  )
}



#' @export
standard_error.gam <- function(model, ...) {
  p.table <- summary(model)$p.table
  s.table <- summary(model)$s.table
  n_cond <- nrow(p.table)
  n_smooth <- nrow(s.table)

  .data_frame(
    Parameter = c(rownames(p.table), rownames(s.table)),
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
    Parameter = colnames(parms),
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
    ## TODO fix once insight is updated on CRAN
    Parameter = params[[1]],
    SE = as.vector(se),
    Component = params[[3]]
  )
}



#' @export
standard_error.htest <- function(model, ...) {
}



#' @export
standard_error.vglm <- function(model, ...) {
  if (!requireNamespace("VGAM", quietly = TRUE)) {
    stop("Package `VGAM` required.", call. = FALSE)
  }

  cs <- VGAM::summary(model)@coef3
  se <- cs[, 2]

  .data_frame(
    Parameter = names(se),
    SE = as.vector(se)
  )
}



#' @export
standard_error.gmnl <- function(model, ...) {
  cs <- summary(model)$CoefTable
  se <- cs[, 2]

  pv <- .data_frame(
    Parameter = names(se),
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



#' @export
standard_error.polr <- function(model, ...) {
  smry <- suppressMessages(as.data.frame(stats::coef(summary(model))))
  se <- smry[[2]]
  names(se) <- rownames(smry)

  .data_frame(
    Parameter = names(se),
    SE = as.vector(se)
  )
}



# helper -----------------------------------------------------------------


#' @importFrom stats coef
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

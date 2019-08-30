#' Extract standard errors
#'
#' This function attempts to return standard errors of model parameters.
#'
#' @param model A model.
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


#' @export
standard_error.lm <- function(model, ...) {
  data_frame(
    Parameter = insight::find_parameters(model, effects = "fixed", component = "conditional", flatten = TRUE),
    SE = .get_se_from_summary(model)
  )
}



#' @export
standard_error.lme <- function(model, ...) {
  cs <- stats::coef(summary(model))
  se <- cs[, 2]

  data_frame(
    Parameter = names(se),
    SE = as.vector(se)
  )
}


#' @export
standard_error.gls <- standard_error.lme



#' @export
standard_error.gam <- function(model, ...) {
  p.table <- summary(model)$p.table

  data_frame(
    Parameter = rownames(p.table),
    SE = as.vector(p.table[, 2]),
    Component = "conditional"
  )
}



#' @export
standard_error.glm <- standard_error.lm


#' @export
standard_error.merMod <- standard_error.lm



#' @rdname standard_error
#' @export
standard_error.glmmTMB <- function(model, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)
  if (is.null(.check_component(model, component))) return(NULL)

  cs <- .compact_list(stats::coef(summary(model)))
  x <- lapply(names(cs), function(i) {
    data_frame(
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
  if (is.null(.check_component(model, component))) return(NULL)

  s <- summary(model)
  cs <- list(s$coef_table, s$coef_table_zi)
  names(cs) <- c("conditional", "zero_inflated")
  cs <- .compact_list(cs)
  x <- lapply(names(cs), function(i) {
    data_frame(
      Parameter = insight::find_parameters(model, effects = "fixed", component = i, flatten = TRUE),
      SE = as.vector(cs[[i]][, 2]),
      Component = i
    )
  })

  se <- do.call(rbind, x)
  .filter_component(se, component)
}



#' @export
standard_error.BBmm <- function(model, ...) {
  data_frame(
    Parameter = insight::find_parameters(model, effects = "fixed", component = "conditional", flatten = TRUE),
    SE = as.data.frame(summary(model)$fixed.coefficients)$StdErr
  )
}



#' @export
standard_error.BBreg <- function(model, ...) {
  data_frame(
    Parameter = insight::find_parameters(model, effects = "fixed", component = "conditional", flatten = TRUE),
    SE = as.data.frame(summary(model)$coefficients)$StdErr
  )
}



#' @export
standard_error.wbm <- function(model, ...) {
  data_frame(
    Parameter = insight::find_parameters(model, effects = "fixed", flatten = TRUE),
    SE = as.vector(as.data.frame(model@summ$coeftable, stringsAsFactors = FALSE)[["S.E."]])
  )
}



#' @export
standard_error.htest <- function(model, ...) {
}



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



#' @export
standard_error.vglm <- function(model, ...) {
  if (!requireNamespace("VGAM", quietly = TRUE)) {
    stop("Package `VGAM` required.", call. = FALSE)
  }

  cs <- VGAM::summary(model)@coef3
  se <- cs[, 2]

  data_frame(
    Parameter = names(se),
    SE = as.vector(se)
  )
}



#' @export
standard_error.svyglm.nb <- function(model, ...) {
  if (!isNamespaceLoaded("survey")) {
    requireNamespace("survey", quietly = TRUE)
  }

  se <- sqrt(diag(stats::vcov(model, stderr = "robust")))

  data_frame(
    Parameter = names(se),
    SE = as.vector(se)
  )
}



#' @export
standard_error.svyglm <- function(model, ...) {
  cs <- stats::coef(summary(model))
  se <- cs[, 2]

  data_frame(
    Parameter = names(se),
    SE = as.vector(se)
  )
}



#' @export
standard_error.gmnl <- function(model, ...) {
  cs <- summary(model)$CoefTable
  se <- cs[, 2]

  pv <- data_frame(
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

  data_frame(
    Parameter = names(se),
    SE = as.vector(se)
  )
}



#' @importFrom stats coef
.get_se_from_summary <- function(model, component = NULL) {
  cs <- stats::coef(summary(model))
  if (is.list(cs) && !is.null(component)) cs <- cs[[component]]

  if (!is.null(cs))
    as.vector(cs[, 2])
  else
    NULL
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

#' Extract standard errors
#'
#' This function attempts to return standard errors of a model's parameters.
#'
#' @param model A model.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' model <- lme4::lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
#' standard_error(model)
#'
#' @return A data.frame.
#' @importFrom stats coef vcov setNames
#' @export
standard_error <- function(model, ...) {
  UseMethod("standard_error")
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
standard_error.merMod <- function(model, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' required for this function to work. Please install it by running `install.packages('lme4')`.")
  }

  cc <- stats::coef(model)

  # get names of intercepts
  inames <- names(cc)

  # variances of fixed effects
  fixed.vars <- diag(as.matrix(stats::vcov(model)))

  # extract variances of conditional modes
  r1 <- lme4::ranef(model, condVar = TRUE)

  # we may have multiple random intercepts, iterate all
  se.merMod <- lapply(1:length(cc), function(i) {
    cmode.vars <- t(apply(attr(r1[[i]], "postVar"), 3, diag))
    seVals <- sqrt(sweep(cmode.vars, 2, fixed.vars[names(r1[[i]])], "+", check.margin = FALSE))

    if (length(r1[[i]]) == 1) {
      seVals <- as.data.frame(t(seVals))
      stats::setNames(seVals, names(r1[[i]]))
    } else {
      seVals <- seVals[, 1:2]
      stats::setNames(as.data.frame(seVals), names(r1[[i]]))
    }
  })

  # set names of list
  names(se.merMod) <- inames

  se.merMod
}



#' @export
standard_error.vglm <- function(model, ...) {
  if (!requireNamespace("VGAM", quietly = TRUE)) {
    stop("Package `VGAM` required.", call. = FALSE)
  }

  cs <- VGAM::summary(model)@coef3
  se <- cs[, 2]

  data.frame(
    Parameter = names(se),
    SE = as.vector(se),
    stringsAsFactors = FALSE
  )
}



#' @export
standard_error.svyglm.nb <- function(model, ...) {
  if (!isNamespaceLoaded("survey")) {
    requireNamespace("survey", quietly = TRUE)
  }

  se <- sqrt(diag(stats::vcov(model, stderr = "robust")))

  data.frame(
    Parameter = names(se),
    SE = as.vector(se),
    stringsAsFactors = FALSE
  )
}



#' @export
standard_error.svyglm <- function(model, ...) {
  cs <- stats::coef(summary(model))
  se <- cs[, 2]

  data.frame(
    Parameter = names(se),
    SE = as.vector(se),
    stringsAsFactors = FALSE
  )
}



#' @export
standard_error.gmnl <- function(model, ...) {
  cs <- summary(model)$CoefTable
  se <- cs[, 2]

  pv <- data.frame(
    Parameter = names(se),
    SE = as.vector(se),
    stringsAsFactors = FALSE
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

  data.frame(
    Parameter = names(se),
    SE = as.vector(se),
    stringsAsFactors = FALSE
  )
}


#' @importFrom stats coef
.get_se_from_summary <- function(model) {
  cs <- stats::coef(summary(model))
  cs[, 2]
}

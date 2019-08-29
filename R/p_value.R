#' p-values
#'
#' This function attempts to return, or compute, p-values of a model's parameters. The nature of the p-values is different depending on the model:
#' \itemize{
#' \item Mixed models (lme4): TO BE IMPROVED.
#' }
#'
#' @param model A statistical model.
#' @param ... Arguments passed to or from other methods.
#' @inheritParams model_simulate
#'
#' @examples
#' model <- lme4::lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
#' p_value(model)
#' @return The p-values.
#' @importFrom bayestestR p_direction convert_pd_to_p
#' @importFrom stats coef vcov pnorm
#' @export
p_value <- function(model, ...) {
  UseMethod("p_value")
}


#' @export
p_value.default <- function(model, ...) {
  p <- tryCatch({
    if (grepl("^Zelig-", class(model)[1])) {
      if (!requireNamespace("Zelig", quietly = T))
        stop("Package `Zelig` required. Please install", call. = F)
      unlist(Zelig::get_pvalue(model))
    } else {
      stats::coef(summary(model))[, 4]
    }
  },
  error = function(e) { NULL }
  )

  if (is.null(p)) {
    insight::print_color("\nCould not extract p-values from model object.\n", "red")
  } else {
    data_frame(
      Parameter = names(p),
      p = as.vector(p)
    )
  }
}



#' @export
p_value.rlm <- function(model, ...) {
  cs <- stats::coef(summary(model))
  p <- 2 * stats::pnorm(abs(cs[, 3]), lower.tail = FALSE)

  data_frame(
    Parameter = names(p),
    p = as.vector(p)
  )
}



#' @export
p_value.BBmm <- function(model, ...) {
  data_frame(
    Parameter = insight::find_parameters(model, effects = "fixed", component = "conditional", flatten = TRUE),
    p = as.data.frame(summary(model)$fixed.coefficients)$p.value
  )
}


#' @export
p_value.BBreg <- function(model, ...) {
  data_frame(
    Parameter = insight::find_parameters(model, effects = "fixed", component = "conditional", flatten = TRUE),
    p = as.data.frame(summary(model)$coefficients)$p.value
  )
}


#' @export
p_value.wbm <- function(model, ...) {
  p <- model@summ$coeftable[, "p"]
  data_frame(
    Parameter = gsub(pattern = "`", replacement = "", x = names(p), fixed = TRUE),
    p = as.vector(p)
  )
}



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

  data_frame(
    Parameter = params$Parameter,
    p = params$p
  )
}



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
  data_frame(
    Parameter = names(data),
    p = sapply(data, p_value)
  )
}




#' @export
p_value.anova <- p_value.aov

#' @export
p_value.aovlist <- p_value.aov



#' @export
p_value.brmsfit <- function(model, ...) {
  p <- bayestestR::p_direction(model)

  data_frame(
    Parameter = p$Parameter,
    p = sapply(p$pd, bayestestR::convert_pd_to_p, simplify = TRUE)
  )
}


#' @export
p_value.stanreg <- p_value.brmsfit


#' @export
p_value.BFBayesFactor <- p_value.brmsfit



#' @export
p_value.gam <- function(model, ...) {
  sm <- summary(model)
  lc <- length(sm$p.coeff)
  p <- sm$p.pv[1:lc]

  data_frame(
    Parameter = names(p),
    p = as.vector(p)
  )
}



#' @export
p_value.gls <- function(model, ...) {
  p <- summary(model)$tTable[, 4]
  data_frame(
    Parameter = names(p),
    p = as.vector(p)
  )
}



#' @export
p_value.pggls <- function(model, ...) {
  p <- summary(model)$CoefTable[, 4]
  data_frame(
    Parameter = names(p),
    p = as.vector(p)
  )
}




#' @export
p_value.gmnl <- function(model, ...) {
  cs <- summary(model)$CoefTable
  p <- cs[, 4]
  # se <- cs[, 2]

  pv <- data_frame(
    Parameter = names(p),
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
p_value.lm <- function(model, ...) {
  parms <- as.data.frame(stats::coef(summary(model)))
  data_frame(
    Parameter = rownames(parms),
    p = as.vector(parms[, "Pr(>|t|)", drop = TRUE])
  )
}



#' @rdname p_value
#' @param method For mixed models, can be \link[=p_value_wald]{"wald"} (default) or \link[=p_value_kenward]{"kenward"}.
#' @export
p_value.lmerMod <- function(model, method = "wald", ...) {
  method <- match.arg(method, c("wald", "kr", "kenward"))
  if (method == "wald") {
    p_value_wald(model, ...)
  } else if (method %in% c("kr", "kenward")) {
    p_value_kenward(model, ...)
  }
}



#' @export
p_value.merMod <- function(model, ...) {
  p_value_wald(model, ...)
}



#' @rdname p_value
#' @export
p_value.glmmTMB <- function(model, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)

  cs <- .compact_list(stats::coef(summary(model)))
  x <- lapply(names(cs), function(i) {
    pv <- .p_value_wald(as.data.frame(cs[[i]]))
    pv$Component <- i
    pv
  })

  p <- do.call(rbind, x)
  p$Component <- .rename_values(p$Component, "cond", "conditional")
  p$Component <- .rename_values(p$Component, "zi", "zero_inflated")

  switch(
    component,
    "conditional" = p[p$Component == "conditional", ],
    "zi" = ,
    "zero_inflated" = p[p$Component == "zero_inflated", ],
    p
  )
}



#' @export
p_value.multinom <- function(model, ...) {
  s <- summary(model)
  stat <- s$coefficients / s$standard.errors
  p <- 2 * stats::pnorm(stat, lower.tail = FALSE)

  data_frame(
    Parameter = names(p),
    p = as.vector(p)
  )
}



#' @export
p_value.maxLik <- function(model, ...) {
  p <- summary(model)$estimate[, 4]

  data_frame(
    Parameter = names(p),
    p = as.vector(p)
  )
}


#' @export
p_value.pglm <- function(model, ...) {
  p <- summary(model)$estimate[, 4]

  data_frame(
    Parameter = names(p),
    p = as.vector(p)
  )
}


#' @export
p_value.polr <- function(model, ...) {
  smry <- suppressMessages(as.data.frame(stats::coef(summary(model))))
  tstat <- smry[[3]]
  # se <- smry[[2]]
  p <- 2 * stats::pnorm(abs(tstat), lower.tail = FALSE)
  names(p) <- rownames(smry)

  data_frame(
    Parameter = names(p),
    p = as.vector(p)
  )
}



#' @export
p_value.svyglm <- function(model, ...) {
  cs <- stats::coef(summary(model))
  p <- cs[, 4]

  data_frame(
    Parameter = names(p),
    p = as.vector(p)
  )
}



#' @export
p_value.svyolr <- function(model, ...) {
  cs <- stats::coef(summary(model))
  p <- 2 * stats::pnorm(abs(cs[, 3]), lower.tail = FALSE)

  data_frame(
    Parameter = names(p),
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
  p <- 2 * stats::pnorm(abs(est / se), lower.tail = FALSE)

  names(p) <- gsub("\\beta\\.", "", names(p), fixed = FALSE)

  data_frame(
    Parameter = names(p),
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
  # se <- cs[, 2]

  data_frame(
    Parameter = names(p),
    p = as.vector(p)
  )
}



#' @importFrom stats coef
.get_pval_from_summary <- function(model) {
  cs <- stats::coef(summary(model))
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

  p
}

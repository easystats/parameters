#' Compute p-values
#'
#' This function attempts to return, or compute, p-values of a model's parameters. The nature of the p-values is different depending on the model:
#' \itemize{
#' \item Mixed models (lme4): TO BE IMPROVED.
#' }
#'
#' @param model A statistical model.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' \dontrun{
#' model <- lme4::lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
#' p_value(model)
#' }
#'
#' @importFrom stats coef vcov pnorm
#' @export
p_value <- function(model, ...) {
  UseMethod("p_value")
}




#' @export
p_value.htest <- function(model, ...) {
  model$p.value
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
    params <- params[params$Parameter != "Residuals",]
  }

  if (!"p" %in% names(params)) {
    return(NA)
  }

  data.frame(
    Parameter = params$Parameter,
    p_value = params$p,
    stringsAsFactors = FALSE
  )
}


#' @export
p_value.anova <- p_value.aov

#' @export
p_value.aovlist <- p_value.aov



#' @rdname p_value
#' @param method For mixed models, can be \link[=p_value_wald]{"wald"} (default) or \link[=p_value_kenward]{"kenward"}.
#' @export
p_value.lmerMod <- function(model, method = "wald", ...) {
  method <- match.arg(method, c("wald", "kr", "kenward"))
  if (method == "wald") {
    p_value_wald(model, ...)
  } else if (method == "kr" | method == "kenward") {
    p_value_kenward(model, ...)
  }
}



#' @export
p_value.merMod <- function(model, ...) {
  p_value_wald(model, ...)
}



#' @export
p_value.vglm <- function(model, ...) {
  if (!requireNamespace("VGAM", quietly = TRUE))
    stop("Package `VGAM` required.", call. = FALSE)

  cs <- VGAM::summary(model)@coef3
  p <- cs[, 4]
  # se <- cs[, 2]

  data.frame(
    Parameter = names(p),
    p_value = as.vector(p),
    stringsAsFactors = FALSE
  )
}



#' @export
p_value.svyglm.nb <- function(model, ...) {
  if (!isNamespaceLoaded("survey"))
    requireNamespace("survey", quietly = TRUE)

  est <- stats::coef(model)
  se <- sqrt(diag(stats::vcov(model, stderr = "robust")))
  p <- 2 * stats::pnorm(abs(est / se), lower.tail = FALSE)

  names(p) <- gsub("\\beta\\.", "", names(p), fixed = FALSE)

  data.frame(
    Parameter = names(p),
    p_value = as.vector(p),
    stringsAsFactors = FALSE
  )
}



#' @export
p_value.svyglm <- function(model, ...) {
  cs <- stats::coef(summary(model))
  p <- cs[, 4]
  # se <- cs[, 2]

  data.frame(
    Parameter = names(p),
    p_value = as.vector(p),
    stringsAsFactors = FALSE
  )
}



#' @export
p_value.gmnl <- function(model, ...) {
  cs <- summary(model)$CoefTable
  p <- cs[, 4]
  # se <- cs[, 2]

  pv <- data.frame(
    Parameter = names(p),
    p_value = as.vector(p),
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
p_value.polr <- function(model, ...) {
  smry <- suppressMessages(as.data.frame(stats::coef(summary(model))))
  tstat <- smry[[3]]
  # se <- smry[[2]]
  p <- 2 * stats::pnorm(abs(tstat), lower.tail = FALSE)
  names(p) <- rownames(smry)

  data.frame(
    Parameter = names(p),
    p_value = as.vector(p),
    stringsAsFactors = FALSE
  )
}



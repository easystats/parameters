#' p-values for Mixed Models
#'
#' This function attempts to return, or compute, p-values of mixed models.
#'
#' @param model A statistical model.
#' @param method For mixed models, can be \code{\link[=p_value_wald]{"wald"}} (default), \code{\link[=p_value_ml1]{"ml1"}}, \code{\link[=p_value_betwithin]{"betwithin"}}, \code{\link[=p_value_satterthwaite]{"satterthwaite"}} or \code{\link[=p_value_kenward]{"kenward"}}. For models that are supported by the \pkg{sandwich} or \pkg{clubSandwich} packages, may also be \code{method = "robust"} to compute p-values based ob robust standard errors.
#' @param verbose Toggle warnings and messages.
#' @inheritParams p_value
#' @inheritParams simulate_model
#' @inheritParams standard_error
#' @inheritParams ci.merMod
#'
#' @details By default, p-values are based on Wald-test approximations (see \code{\link{p_value_wald}}). For certain situations, the "m-l-1" rule might be a better approximation. That is, for \code{method = "ml1"}, \code{\link{p_value_ml1}} is called. For \code{lmerMod} objects, if \code{method = "kenward"}, p-values are based on Kenward-Roger approximations, i.e. \code{\link{p_value_kenward}} is called, and \code{method = "satterthwaite"} calls \code{\link{p_value_satterthwaite}}.
#'
#' @return A data frame with at least two columns: the parameter names and the p-values. Depending on the model, may also include columns for model components etc.
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


#' @importFrom insight get_statistic
#' @importFrom stats pnorm
#' @export
p_value.sem <- function(model, ...) {
  if (!.is_semLme(model)) {
    return(NULL)
  }

  stat <- insight::get_statistic(model)
  if (is.null(stat)) {
    return(NULL)
  }

  .data_frame(
    Parameter = stat$Parameter,
    p = 2 * stats::pnorm(abs(stat$Statistic), lower.tail = FALSE)
  )
}



#' @importFrom stats coef
#' @export
p_value.lme <- function(model, ...) {
  cs <- stats::coef(summary(model))
  p <- cs[, 5]

  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(cs)),
    p = as.vector(p)
  )
}



#' @rdname p_value.lmerMod
#' @export
p_value.merMod <- function(model, method = "wald", ...) {
  method <- match.arg(tolower(method), c("wald", "betwithin", "ml1"))
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

#' @export
p_value.HLfit <- p_value.merMod

#' @export
p_value.rlmerMod <- p_value.merMod

#' @export
p_value.merModList <- function(model, ...) {
  dof <- degrees_of_freedom(model)
  p_value_wald(model, dof, ...)
}



#' @importFrom insight find_parameters
#' @importFrom stats coef
#' @rdname p_value.lmerMod
#' @export
p_value.glmmTMB <- function(model, component = c("all", "conditional", "zi", "zero_inflated", "dispersion"), verbose = TRUE, ...) {
  component <- match.arg(component)
  if (is.null(.check_component(model, component, verbose = verbose))) {
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
  p$Component <- .rename_values(p$Component, "disp", "dispersion")

  .filter_component(p, component)
}



#' @importFrom insight find_parameters
#' @rdname p_value.lmerMod
#' @export
p_value.MixMod <- function(model, component = c("all", "conditional", "zi", "zero_inflated"), verbose = TRUE, ...) {
  component <- match.arg(component)
  if (is.null(.check_component(model, component, verbose = verbose))) {
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



#' @rdname p_value.lmerMod
#' @importFrom insight get_parameters
#' @export
p_value.mixor <- function(model, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  stats <- model$Model[, "P(>|z|)"]
  parms <- insight::get_parameters(model, effects = effects)

  .data_frame(
    Parameter = parms$Parameter,
    p = stats[parms$Parameter],
    Effects = parms$Effects
  )
}



#' @importFrom insight get_parameters
#' @export
p_value.glmm <- function(model, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  s <- summary(model)

  out <- insight::get_parameters(model, effects = "all")
  out$p <- c(s$coefmat[, 4], s$nucoefmat[, 4])
  out <- out[, c("Parameter", "p", "Effects")]

  if (effects != "all") {
    out <- out[out$Effects == effects, , drop = FALSE]
    out$Effects <- NULL
  }

  out
}

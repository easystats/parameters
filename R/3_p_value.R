#' p-values
#'
#' This function attempts to return, or compute, p-values of a model's parameters. See the documentation for your object's class:
#' \itemize{
#'  \item{\link[=p_value.lmerMod]{Mixed models} (\pkg{lme4}, \pkg{nlme}, \pkg{glmmTMB}, ...)}
#'  \item{\link[=p_value.BFBayesFactor]{Bayesian models} (\pkg{rstanarm}, \pkg{brms}, \pkg{MCMCglmm}, ...)}
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

#' p-values
#'
#' This function attempts to return, or compute, p-values of a model's parameters. See the documentation for your object's class:
#' \itemize{
#'  \item{[Bayesian models][p_value.BFBayesFactor] (\pkg{rstanarm}, \pkg{brms}, \pkg{MCMCglmm}, ...)}
#'  \item{[Zero-inflated models][p_value.zeroinfl] (`hurdle`, `zeroinfl`, `zerocount`, ...)}
#'  \item{[Marginal effects models][p_value.poissonmfx] (\pkg{mfx})}
#'  \item{[Models with special components][p_value.DirichletRegModel] (`DirichletRegModel`, `clm2`, `cgam`, ...)}
#'  }
#'
#' @param model A statistical model.
#' @param method If `"robust"`, and if model is supported by the \pkg{sandwich} or \pkg{clubSandwich} packages, computes p-values based on robust covariance matrix estimation.
#' @param adjust Character value naming the method used to adjust p-values or confidence intervals. See `?emmeans::summary.emmGrid` for details.
#' @param verbose Toggle warnings and messages.
#' @param ... Arguments passed down to `standard_error_robust()` when confidence intervals or p-values based on robust standard errors should be computed. Only available for models where `method = "robust"` is supported.
#' @inheritParams ci.default
#'
#' @note `p_value_robust()` resp. `p_value(method = "robust")`
#'   rely on the \pkg{sandwich} or \pkg{clubSandwich} package (the latter if
#'   `vcov_estimation = "CR"` for cluster-robust standard errors) and will
#'   thus only work for those models supported by those packages.
#'
#' @return A data frame with at least two columns: the parameter names and the p-values. Depending on the model, may also include columns for model components etc.
#'
#' @examples
#' data(iris)
#' model <- lm(Petal.Length ~ Sepal.Length + Species, data = iris)
#' p_value(model)
#' @export
p_value <- function(model, ...) {
  UseMethod("p_value")
}


# p-Values from Standard Models -----------------------------------------------


#' @rdname p_value
#' @export
p_value.default <- function(model, method = NULL, robust = FALSE, verbose = TRUE, ...) {
  if (!is.null(method)) {
    method <- tolower(method)
  } else {
    method <- "wald"
  }

  p <- NULL

  if (isTRUE(robust)) {
    return(p_value_robust(model, method = method, ...))
  } else if (method == "ml1") {
    return(p_value_ml1(model))
  } else if (method == "betwithin") {
    return(p_value_betwithin(model))
  } else if (method %in% c("residual", "wald", "normal", "satterthwaite", "kenward", "kr")) {
    dof <- degrees_of_freedom(model, method = method)
    return(.p_value_dof(model, dof = dof, method = method))
  } else if (method %in% c("hdi", "eti", "si", "bci", "bcai", "quantile")) {
    return(bayestestR::p_direction(model, ...))
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
        p_from_stat <- 2 * stats::pt(abs(stat$Statistic), df = Inf, lower.tail = FALSE)
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

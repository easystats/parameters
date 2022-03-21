#' p-values
#'
#' This function attempts to return, or compute, p-values of a model's
#' parameters. See the documentation for your object's class:
#' \itemize{
#'  \item{[Bayesian models][p_value.BFBayesFactor] (\pkg{rstanarm}, \pkg{brms}, \pkg{MCMCglmm}, ...)}
#'  \item{[Zero-inflated models][p_value.zeroinfl] (`hurdle`, `zeroinfl`, `zerocount`, ...)}
#'  \item{[Marginal effects models][p_value.poissonmfx] (\pkg{mfx})}
#'  \item{[Models with special components][p_value.DirichletRegModel] (`DirichletRegModel`, `clm2`, `cgam`, ...)}
#'  }
#'
#' @param model A statistical model.
#' @param method For linear mixed models, `method` can be
#'   [`"kenward"`][p_value_kenward] or
#'   [`"satterthwaite"`][p_value_satterthwaite].
#' @param adjust Character value naming the method used to adjust p-values or
#'   confidence intervals. See `?emmeans::summary.emmGrid` for details.
#' @param ... Additional arguments
#' @inheritParams ci.default
#' @inheritParams standard_error.default
#'
#' @return A data frame with at least two columns: the parameter names and the
#'   p-values. Depending on the model, may also include columns for model
#'   components etc.
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
p_value.default <- function(model,
                            dof = NULL,
                            method = NULL,
                            component = "all",
                            vcov = NULL,
                            vcov_args = NULL,
                            verbose = TRUE,
                            ...) {
  dots <- list(...)

  if (is.character(method)) {
    method <- tolower(method)
  } else {
    method <- "wald"
  }

  # robust standard errors with backward compatibility for `robust = TRUE`
  if (!is.null(vcov) || isTRUE(dots[["robust"]])) {
    method <- "robust"
  }

  # default p-value method for profiled or uniroot CI
  if (method %in% c("uniroot", "profile", "likelihood", "boot")) {
    method <- "normal"
  }

  if (method == "ml1") {
    p <- p_value_ml1(model)
    return(p)
  }

  if (method == "betwithin") {
    p <- p_value_betwithin(model)
    return(p)
  }

  if (method %in% c("residual", "wald", "normal", "satterthwaite", "kenward", "kr")) {
    if (is.null(dof)) {
      dof <- degrees_of_freedom(model, method = method, verbose = FALSE)
    }
    p <- .p_value_dof(
      model,
      dof = dof,
      method = method,
      component = component,
      verbose = verbose,
      ...
    )
    return(p)
  }

  if (method %in% c("hdi", "eti", "si", "bci", "bcai", "quantile")) {
    p <- bayestestR::p_direction(model, ...)
    return(p)
  }

  # robust standard errors
  if (method == "robust") {
    co <- insight::get_parameters(model)
    # for polr, we need to fix parameter names
    co$Parameter <- gsub("Intercept: ", "", co$Parameter, fixed = TRUE)
    # this allows us to pass the output of `standard_error()`
    # to the `vcov` argument in order to avoid computing the SE twice.
    if (inherits(vcov, "data.frame") || "SE" %in% colnames(vcov)) {
      se <- vcov
    } else {
      args <- list(model,
        vcov_args = vcov_args,
        vcov = vcov,
        verbose = verbose
      )
      args <- c(args, dots)
      se <- do.call("standard_error", args)
    }

    dof <- degrees_of_freedom(model, method = "wald", verbose = FALSE)
    se <- merge(se, co, sort = FALSE)
    se$Statistic <- se$Estimate / se$SE
    se$p <- 2 * stats::pt(abs(se$Statistic), df = dof, lower.tail = FALSE)
    p <- stats::setNames(se$p, se$Parameter)
  }

  # default 1st try: summary()
  if (is.null(p)) {
    p <- tryCatch(
      {
        # Zelig-models are weird
        if (grepl("Zelig-", class(model)[1], fixed = TRUE)) {
          unlist(model$get_pvalue())
        } else {
          # try to get p-value from classical summary for default models
          .get_pval_from_summary(model)
        }
      },
      error = function(e) NULL
    )
  }

  # default 2nd try: p value from test-statistic
  if (is.null(p)) {
    p <- tryCatch(
      {
        stat <- insight::get_statistic(model)
        p_from_stat <- 2 * stats::pt(abs(stat$Statistic), df = Inf, lower.tail = FALSE)
        names(p_from_stat) <- stat$Parameter
        p_from_stat
      },
      error = function(e) NULL
    )
  }

  # output
  if (!is.null(p)) {
    params <- insight::get_parameters(model, component = component)
    if (length(p) == nrow(params) && "Component" %in% colnames(params)) {
      p <- .data_frame(Parameter = params$Parameter, p = as.vector(p), Component = params$Component)
    } else {
      p <- .data_frame(Parameter = names(p), p = as.vector(p))
    }
    return(p)
  }

  # failure warning
  if (is.null(p) && isTRUE(verbose)) {
    warning("Could not extract p-values from model object.", call. = FALSE)
  }
}


# helper --------------------------------------------------------


.get_pval_from_summary <- function(model, cs = NULL) {
  if (is.null(cs)) cs <- suppressWarnings(stats::coef(summary(model)))
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

#' Standard Errors
#'
#' `standard_error()` attempts to return standard errors of model
#' parameters
#'
#' @param model A model.
#' @param force Logical, if `TRUE`, factors are converted to numerical
#'   values to calculate the standard error, with the lowest level being the
#'   value `1` (unless the factor has numeric levels, which are converted
#'   to the corresponding numeric value). By default, `NA` is returned for
#'   factors or character vectors.
#' @param vcov Variance-covariance matrix used to compute uncertainty estimates
#' (e.g., for robust standard errors). This argument accepts a covariance matrix,
#' a function which returns a covariance matrix, or a string which identifies
#' the function to be used to compute the covariance matrix.
#'  * A covariance matrix
#'  * A function which returns a covariance matrix (e.g., `stats::vcov()`)
#'  * A string which indicates the kind of uncertainty estimates to return.
#'    - Heteroskedasticity-consistent: `"vcovHC"`, `"HC"`, `"HC0"`, `"HC1"`, `"HC2"`, `"HC3"`, `"HC4"`, `"HC4m"`, `"HC5"`. See `?sandwich::vcovHC`.
#'    - Cluster-robust: `"vcovCR"`, `"CR0"`, `"CR1"`, `"CR1p"`, `"CR1S"`, `"CR2"`, `"CR3"`. See `?clubSandwich::vcovCR`.
#'    - Bootstrap: `"vcovBS"`, `"xy"`, `"residual"`, `"wild"`, `"mammen"`, `"webb"`. See `?sandwich::vcovBS`.
#'    - Other `sandwich` package functions: `"vcovHAC"`, `"vcovPC"`, `"vcovCL"`, `"vcovPL"`.
#' @param vcov_args List of arguments to be passed to the function identified by
#'   the `vcov` argument. This function is typically supplied by the *sandwich*
#'   or *clubSandwich* packages. Please refer to their documentation (e.g.,
#'   `?sandwich::vcovHAC`) to see the list of available arguments.
#' @param effects Should standard errors for fixed effects or random effects be
#'   returned? Only applies to mixed models. May be abbreviated. When standard
#'   errors for random effects are requested, for each grouping factor a list of
#'   standard errors (per group level) for random intercepts and slopes is
#'   returned.
#' @inheritParams simulate_model
#' @inheritParams p_value
#' @param ... Arguments passed to or from other methods.
#'
#' @note For Bayesian models (from \pkg{rstanarm} or \pkg{brms}), the standard
#'   error is the SD of the posterior samples.
#'
#' @return A data frame with at least two columns: the parameter names and the
#'   standard errors. Depending on the model, may also include columns for model
#'   components etc.
#'
#' @examples
#' model <- lm(Petal.Length ~ Sepal.Length * Species, data = iris)
#'
#' standard_error(model)
#'
#' standard_error(model, vcov = "HC3")
#'
#' standard_error(model,
#'   vcov = "vcovCL",
#'   vcov_args = list(cluster = iris$Species)
#' )
#' @export
standard_error <- function(model, ...) {
  UseMethod("standard_error")
}



# Default methods ---------------------------------------------------------

#' @rdname standard_error
#' @export
standard_error.default <- function(model,
                                   component = "all",
                                   vcov = NULL,
                                   vcov_args = NULL,
                                   verbose = TRUE,
                                   ...) {
  dots <- list(...)

  se <- NULL

  # vcov: matrix
  if (is.matrix(vcov)) {
    se <- sqrt(diag(vcov))
  }

  # vcov: function which returns a matrix
  if (is.function(vcov)) {
    args <- c(list(model), vcov_args, dots)
    se <- tryCatch(sqrt(diag(do.call("vcov", args))),
      error = function(x) NULL
    )
  }

  # vcov: character (with backward compatibility for `robust = TRUE`)
  if (is.character(vcov) || isTRUE(dots[["robust"]])) {
    args <- list(model,
      vcov_fun = vcov,
      vcov_args = vcov_args,
      verbose = verbose
    )
    args <- c(args, dots)
    .vcov <- do.call(".get_vcov", args)
    se <- sqrt(diag(.vcov))
  }

  # classical se from summary()
  if (is.null(se)) {
    se <- tryCatch(
      {
        if (grepl("Zelig-", class(model)[1], fixed = TRUE)) {
          unlist(model$get_se())
        } else {
          .get_se_from_summary(model)
        }
      },
      error = function(e) NULL
    )
  }

  # classical se from get_varcov()
  if (is.null(se)) {
    se <- tryCatch(
      {
        varcov <- insight::get_varcov(model, component = component)
        se_from_varcov <- sqrt(diag(varcov))
        names(se_from_varcov) <- colnames(varcov)
        se_from_varcov
      },
      error = function(e) NULL
    )
  }

  # output
  if (is.null(se)) {
    if (isTRUE(verbose)) {
      warning("Could not extract standard errors from model object.", call. = FALSE)
    }
  } else {
    params <- insight::get_parameters(model, component = component)
    if (length(se) == nrow(params) && "Component" %in% colnames(params)) {
      se <- .data_frame(Parameter = params$Parameter, SE = as.vector(se), Component = params$Component)
    } else {
      se <- .data_frame(Parameter = names(se), SE = as.vector(se))
    }
  }

  se
}




# helper -----------------------------------------------------------------


.get_se_from_summary <- function(model, component = NULL) {
  cs <- suppressWarnings(stats::coef(summary(model)))
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



.check_vcov_args <- function(robust, ...) {
  dots <- list(...)
  isTRUE(isTRUE(robust) || isTRUE(dots$robust) || ("vcov" %in% names(dots) && !is.null(dots[["vcov"]])))
}


# compute robust vcov ----------------

.get_vcov <- function(x,
                      vcov_fun = "vcovHC",
                      vcov_args = NULL,
                      component = "conditional",
                      method = "any",
                      verbose = TRUE,
                      ...) {
  dots <- list(...)

  # superseded arguments
  if (isTRUE(verbose) && "vcov_type" %in% names(dots)) {
    warning(insight::format_message("The `vcov_type` argument is superseded by the `vcov_args` argument."), call. = FALSE)
  }
  if (isTRUE(verbose) && "robust" %in% names(dots)) {
    warning(insight::format_message("The `robust` argument is superseded by the `vcov` argument."), call. = FALSE)
  }

  if (is.null(vcov_args)) {
    vcov_args <- list()
  }

  # deprecated: `vcov_estimation`
  if (is.null(vcov_fun) && "vcov_estimation" %in% names(dots)) {
    vcov_fun <- dots[["vcov_estimation"]]
  }

  # deprecated: `robust`
  if (isTRUE(dots[["robust"]]) && is.null(vcov_fun)) {
    dots[["robust"]] <- NULL
    vcov_fun <- "HC3"
  }

  # deprecated: `vcov_type`
  if ("vcov_type" %in% names(dots)) {
    if (!"type" %in% names(vcov_args)) {
      vcov_args[["type"]] <- dots[["vcov_type"]]
    }
  }

  # type shortcuts: overwrite only if not supplied explicitly by the user
  if (!"type" %in% names(vcov_args)) {
    if (vcov_fun %in% c(
      "HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5",
      "CR0", "CR1", "CR1p", "CR1S", "CR2", "CR3", "xy",
      "residual", "wild", "mammen", "webb"
    )) {
      vcov_args[["type"]] <- vcov_fun
    }
  }

  if (!grepl("^(vcov|kernHAC|NeweyWest)", vcov_fun)) {
    vcov_fun <- switch(vcov_fun,
      "HC0" = ,
      "HC1" = ,
      "HC2" = ,
      "HC3" = ,
      "HC4" = ,
      "HC4m" = ,
      "HC5" = ,
      "HC" = "vcovHC",
      "CR0" = ,
      "CR1" = ,
      "CR1p" = ,
      "CR1S" = ,
      "CR2" = ,
      "CR3" = ,
      "CR" = "vcovCR",
      "xy" = ,
      "residual" = ,
      "wild" = ,
      "mammen" = ,
      "webb" = ,
      "BS" = "vcovBS",
      "OPG" = "vcovOPG",
      "HAC" = "vcovHAC",
      "PC" = "vcovPC",
      "CL" = "vcovCL",
      "PL" = "vcovPL"
    )
  }

  # check if required package is available
  if (vcov_fun == "vcovCR") {
    insight::check_if_installed("clubSandwich", reason = "to get cluster-robust standard errors")
    fun <- get(vcov_fun, asNamespace("clubSandwich"))
  } else {
    insight::check_if_installed("sandwich", reason = "to get robust standard errors")
    fun <- try(get(vcov_fun, asNamespace("sandwich")), silent = TRUE)
    if (!is.function(fun)) {
      stop(sprintf("`%s` is not a function exported by the `sandwich` package.", vcov_fun))
    }
  }

  # extract variance-covariance matrix
  .vcov <- do.call(fun, c(list(x), vcov_args))

  .vcov
}



# .ranef_se <- function(x) {
# insight::check_if_installed("lme4")
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

#' Standard Errors
#'
#' `standard_error()` attempts to return standard errors of model
#' parameters, while `standard_error_robust()` attempts to return robust
#' standard errors.
#'
#' @param model A model.
#' @param force Logical, if `TRUE`, factors are converted to numerical
#'   values to calculate the standard error, with the lowest level being the
#'   value `1` (unless the factor has numeric levels, which are converted
#'   to the corresponding numeric value). By default, `NA` is returned for
#'   factors or character vectors.
#' @param method If `"robust"`, robust standard errors are computed by
#'   calling [`standard_error_robust()`][standard_error_robust].
#'   `standard_error_robust()`, in turn, calls one of the
#'   `vcov*()`-functions from the \pkg{sandwich} or \pkg{clubSandwich}
#'   package for robust covariance matrix estimators. For certain mixed models,
#'   `method` may also be one of `"wald"`,
#'   [`"ml1"()`][p_value_ml1],
#'   [`"betwithin"()`][p_value_betwithin],
#'   [`"satterthwaite"()`][p_value_satterthwaite] or
#'   [`"kenward"()`][p_value_kenward].
#' @param ... Arguments passed to or from other methods. For
#'   `standard_error()`, if `method = "robust"`, arguments
#'   `vcov_estimation`, `vcov_type` and `vcov_args` can be passed
#'   down to [`standard_error_robust()`][standard_error_robust].
#' @param effects Should standard errors for fixed effects or random effects be
#'   returned? Only applies to mixed models. May be abbreviated. When standard
#'   errors for random effects are requested, for each grouping factor a list of
#'   standard errors (per group level) for random intercepts and slopes is
#'   returned.
#' @inheritParams simulate_model
#' @inheritParams p_value
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
#' standard_error(model)
#' @export
standard_error <- function(model, ...) {
  UseMethod("standard_error")
}



# Default methods ---------------------------------------------------------


#' @rdname standard_error
#' @export
standard_error.default <- function(model, method = NULL, verbose = TRUE, ...) {
  if (!is.null(method)) {
    method <- tolower(method)
  } else {
    method <- "wald"
  }

  if (method == "robust") {
    standard_error_robust(model, ...)
  } else if (method == "ml1") {
    se_ml1(model)
  } else if (method == "betwithin") {
    se_betwithin(model)
  } else {
    se <- tryCatch(
      {
        if (grepl("^Zelig-", class(model)[1])) {
          unlist(model$get_se())
        } else {
          .get_se_from_summary(model)
        }
      },
      error = function(e) {
        NULL
      }
    )

    # if all fails, try to get se from varcov
    if (is.null(se)) {
      se <- tryCatch(
        {
          varcov <- insight::get_varcov(model)
          se_from_varcov <- sqrt(diag(varcov))
          names(se_from_varcov) <- colnames(varcov)
          se_from_varcov
        },
        error = function(e) {
          NULL
        }
      )
    }


    if (is.null(se)) {
      if (isTRUE(verbose)) {
        insight::print_color("\nCould not extract standard errors from model object.\n", "red")
      }
    } else {
      .data_frame(
        Parameter = names(se),
        SE = as.vector(se)
      )
    }
  }
}


# helper -----------------------------------------------------------------


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

  names(se) <- .remove_backticks_from_string(names(se))
  se
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

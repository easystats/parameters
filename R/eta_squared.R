#' (Partial) Eta Squared Squared.
#'
#' Computation of (Partial) Eta Squared for ANOVAs.
#'
#' @param model an ANOVA object.
#' @param partial Return partial eta squared.
#' @param ci Scalar between 0 and 1. If not \code{NULL}, lower and upper
#'   confidence intervals are returned as well.
#'
#' @inheritParams model_bootstrap
#'
#' @examples
#' library(parameters)
#'
#' df <- iris
#' df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")
#'
#' model <- aov(Sepal.Length ~ Sepal.Big, data = df)
#' eta_squared(model)
#'
#' model <- anova(lm(Sepal.Length ~ Sepal.Big * Species, data = df))
#' eta_squared(model)
#'
#' \donttest{
#' # Don't work for now
#' model <- aov(Sepal.Length ~ Sepal.Big + Error(Species), data = df)
#' eta_squared(model)
#' }
#'
#' @return Eta squared values.
#'
#' @export
eta_squared <- function(model, partial = TRUE, ci = NULL, iterations = 1000) {
  UseMethod("eta_squared")
}



#' @export
eta_squared.aov <- function(model, partial = TRUE, ci = NULL, iterations = 1000) {
  m <- .eta_squared(model, partial = partial, ci = ci, iterations = iterations)
  class(m) <- c(ifelse(isTRUE(partial), "partial_eta_squared", "eta_squared"), class(m))
  m
}

#' @export
eta_squared.anova <- eta_squared.aov


#' @export
eta_squared.aovlist <- function(model, partial = TRUE, ci = NULL, iterations = 1000) {
  stop("Eta squared not implemented yet for repeated-measures ANOVAs.")

  # params <- .extract_parameters_anova(model)
  # values <- .values_aov(params)
  #
  # if (!"Residuals" %in% params$Parameter) {
  #   stop("No residuals data found. Omega squared can only be computed for simple `aov` models.")
  # }
  #
  # mapply(function(p, v) {
  #   .extract_eta_squared(p, v, partial)
  # }, split(params, params$Group), values, SIMPLIFY = FALSE)
}



#' @keywords internal
.eta_squared <- function(model, partial, ci, iterations) {
  params <- .extract_parameters_anova(model)
  values <- .values_aov(params)

  if (!"Residuals" %in% params$Parameter) {
    stop("No residuals data found. Eta squared can only be computed for simple `aov` models.")
  }

  eff_size <- .extract_eta_squared(params, values, partial)

  .ci_eta_squared(
    x = eff_size,
    partial = partial,
    ci.lvl = ci,
    df = params[["df"]],
    statistic = params[["F"]],
    model = model,
    iterations = iterations
  )
}



.extract_eta_squared <- function(params, values, partial) {
  if (partial == FALSE) {
    params[params$Parameter == "Residuals", "Eta_Sq"] <- NA
    params$Eta_Sq <- params$Sum_Squares / values$Sum_Squares_total
  } else {
    params$Eta_Sq_partial <- params$Sum_Squares / (params$Sum_Squares + values$Sum_Squares_residuals)
    params[params$Parameter == "Residuals", "Eta_Sq_partial"] <- NA
  }

  params[, intersect(c("Group", "Parameter", "Eta_Sq", "Eta_Sq_partial"), names(params)), drop = FALSE]
}



.ci_eta_squared <- function(x, partial, ci.lvl, df, statistic, model, iterations) {
  if (is.null(ci.lvl) || is.na(ci.lvl)) return(x)

  if (isTRUE(partial)) {
    ci_eta <- lapply(
      1:nrow(x),
      function(.x) {
        if (!is.na(statistic[.x])) {
          ci <- .ci_partial_eta_squared(
            F.value = statistic[.x],
            df1 = df[.x],
            df2 = df[nrow(x)],
            conf.level = ci.lvl
          )
          ci.low <- ci$LL
          ci.high <- ci$UL
        } else {
          ci.low <- ci.high <- NA
        }

        data.frame(
          CI_low = ci.low,
          CI_high = ci.high
        )
      }
    )

    cbind(x, do.call(rbind, ci_eta))
  } else {
    if (inherits(model, "anova") || is.data.frame(model)) {
      warning("Confidence intervals can't be computed for data frames or objects of class 'anova'.")
      return(x)
    }

    if (!requireNamespace("boot", quietly = TRUE)) {
      stop("Package 'boot' needed for this function to work. Please install it.")
    }

    dat <- insight::get_data(model)
    f <- insight::find_formula(model)

    boot_function <- function(model, data, indices) {
      d <- data[indices, ] # allows boot to select sample
      fit <- stats::aov(f$conditional, data = d)
      params <- .extract_parameters_anova(fit)
      values <- .values_aov(params)
      osq <- .extract_eta_squared(params, values, partial = FALSE)
      return(osq[["Eta_Sq"]])
    }

    results <- boot::boot(
      data = dat,
      statistic = boot_function,
      R = iterations,
      model = model,
      parallel = "multicore"
    )

    df <- as.data.frame(results$t)
    x$CI_low <- sapply(df[, 1:ncol(df)], stats::quantile, probs = (1 - ci.lvl) / 2, na.rm = TRUE)
    x$CI_high <- sapply(df[, 1:ncol(df)], stats::quantile, probs = (1 + ci.lvl) / 2, na.rm = TRUE)
    x
  }
}
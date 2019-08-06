#' @rdname eta_squared
#' @export
omega_squared <- function(model, partial = TRUE, ci = NULL, iterations = 1000) {
  UseMethod("omega_squared")
}



#' @export
omega_squared.aov <- function(model, partial = TRUE, ci = NULL, iterations = 1000) {
  m <- .omega_squared(model, partial = partial, ci = ci, iterations = iterations)
  class(m) <- c(ifelse(isTRUE(partial), "partial_omega_squared", "omega_squared"), class(m))
  m
}

#' @export
omega_squared.anova <- omega_squared.aov


#' @export
omega_squared.aovlist <- function(model, partial = TRUE, ci = NULL, iterations = 1000) {
  stop("Omega squared not implemented yet for repeated-measures ANOVAs.")

  # params <- .extract_parameters_anova(model)
  # values <- .values_aov(params)
  #
  # if (!"Residuals" %in% params$Parameter) {
  #   stop("No residuals data found. Omega squared can only be computed for simple `aov` models.")
  # }
  #
  # mapply(function(p, v) {
  #   .extract_omega_squared(p, v, partial)
  # }, split(params, params$Group), values, SIMPLIFY = FALSE)
}



#' @keywords internal
.omega_squared <- function(model, partial, ci, iterations) {
  params <- .extract_parameters_anova(model)
  values <- .values_aov(params)

  if (!"Residuals" %in% params$Parameter) {
    stop("No residuals data found. Omega squared can only be computed for simple `aov` models.")
  }

  eff_size <- .extract_omega_squared(params, values, partial)

  .ci_omega_squared(
    x = eff_size,
    partial = partial,
    ci.lvl = ci,
    df = params[["df"]],
    statistic = params[["F"]],
    model = model,
    iterations = iterations
  )
}

#' @keywords internal
.extract_omega_squared <- function(params, values, partial) {
  if (partial == FALSE) {
    params$Omega_Sq <- (params$Sum_Squares - params$df * values$Mean_Square_residuals) / (values$Sum_Squares_total + values$Mean_Square_residuals)
    params[params$Parameter == "Residuals", "Omega_Sq"] <- NA
  } else {
    params$Omega_Sq_partial <- (params$df * (params$Mean_Square - values$Mean_Square_residuals)) / (params$df * params$Mean_Square + (values$n - params$df) * values$Mean_Square_residuals)
    params[params$Parameter == "Residuals", "Omega_Sq_partial"] <- NA
  }

  params[, intersect(c("Group", "Parameter", "Omega_Sq", "Omega_Sq_partial"), names(params)), drop = FALSE]
}



#' @importFrom stats aov quantile
#' @importFrom insight get_data find_formula
#' @keywords internal
.ci_omega_squared <- function(x, partial, ci.lvl, df, statistic, model, iterations) {
  if (is.null(ci.lvl) || is.na(ci.lvl)) return(x)
  N <- sum(df) + 1

  if (partial == FALSE) {
    ci_omega <- lapply(
      1:nrow(x),
      function(.x) {
        if (!is.na(statistic[.x])) {
          ci <- .confint_ncg(
            F.value = statistic[.x],
            conf.level = ci.lvl,
            df.1 = df[.x],
            df.2 = df[nrow(x)]
          )
          ci.low <- ci$Lower.Limit / (ci$Lower.Limit + N)
          ci.high <- ci$Upper.Limit / (ci$Upper.Limit + N)
        } else {
          ci.low <- ci.high <- NA
        }

        data.frame(
          CI_low = ci.low,
          CI_high = ci.high
        )
      }
    )
    cbind(x, do.call(rbind, ci_omega))
  } else {
    if (inherits(model, "anova") || is.data.frame(model)) {
      warning("Confidence intervals can't be computed for data frames or objects of class 'anova'.")
      return(x)
    }

    if (!requireNamespace("boot", quietly = TRUE)) {
      stop("Package 'boot' needed for this function to work. Please install it.")
    }

    dat <- insight::get_data(model)

    boot_function <- function(model, data, indices) {
      d <- data[indices, ] # allows boot to select sample
      fit <- stats::aov(insight::find_formula(model)$conditional, data = d)
      params <- .extract_parameters_anova(fit)
      values <- .values_aov(params)
      osq <- .extract_omega_squared(params, values, partial = TRUE)
      return(osq[["Omega_Sq_partial"]])
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
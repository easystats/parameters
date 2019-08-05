#' (Partial) Omega Squared.
#'
#' Computation of (Partial) Omega Squared for ANOVAs.
#'
#' @param partial Return partial omega squared.
#' @inheritParams eta_squared
#'
#' @examples
#' library(parameters)
#'
#' df <- iris
#' df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")
#'
#' model <- aov(Sepal.Length ~ Sepal.Big, data = df)
#' omega_squared(model)
#'
#' model <- anova(lm(Sepal.Length ~ Sepal.Big * Species, data = df))
#' omega_squared(model)
#'
#' \donttest{
#' # Don't work for now
#' model <- aov(Sepal.Length ~ Sepal.Big + Error(Species), data = df)
#' omega_squared(model)
#' }
#'
#' @return Omega squared values.
#'
#' @export
omega_squared <- function(model, partial = TRUE, ci = NULL) {
  UseMethod("omega_squared")
}



#' @export
omega_squared.aov <- function(model, partial = TRUE, ci = NULL) {
  m <- .omega_squared(model, partial = partial, ci = ci)
  class(m) <- c(ifelse(isTRUE(partial), "partial_omega_squared", "omega_squared"), class(m))
  m
}

#' @export
omega_squared.anova <- omega_squared.aov


#' @export
omega_squared.aovlist <- function(model, partial = TRUE, ci = NULL) {
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
.omega_squared <- function(model, partial, ci) {
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
    model = model
  )
}


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



.ci_omega_squared <- function(x, partial, ci.lvl, df, statistic, model, iterations = 1000) {
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
    f <- insight::find_formula(model)

    boot_function <- function(model, data, indices) {
      d <- data[indices, ] # allows boot to select sample
      fit <- stats::aov(f$conditional, data = dat)
      params <- .extract_parameters_anova(fit)
      values <- .values_aov(params)
      osq <- .extract_omega_squared(params, values, partial = TRUE)
      return(osq[["Omega_Sq_partial"]])
    }

    results <- boot::boot(data = dat, statistic = boot_function, R = iterations, model = model)

    df <- as.data.frame(results$t)
    x
  }
}
#' ANOVA Effect Size (Omega Squared, Eta Squared, Epsilon Squared)
#'
#' Set of functions to compute (partial) indices for ANOVAs, such as omega squared, the eta squared or the epsilon squared (Kelly, 1935). They are measures of effect size, or the degree of association for a population. They are an estimate of how much variance in the response variables are accounted for by the explanatory variables.
#'
#' @param model An ANOVA object.
#' @param partial If \code{TRUE}, return partial indices (adjusted for sample size).
#' @param ci Confidence Interval (CI) level computed via boostrap.
#' @inheritParams model_bootstrap
#'
#' @details
#' \subsection{Omega Squared}{
#' Omega squared is considered as a lesser biased alternative to eta-squared, especially when sample sizes are small (Albers \& Lakens, 2018). Field (2013) suggests the following interpretation heuristics:
#' \itemize{
#'   \item Omega Squared = 0 - 0.01: Very small
#'   \item Omega Squared = 0.01 - 0.06: Small
#'   \item Omega Squared = 0.06 - 0.14: Medium
#'   \item Omega Squared > 0.14: Large
#' }
#'
#' } \subsection{Epsilon Squared}{
#' It is one of the least common measures of effect sizes: omega squared and eta squared are used more frequently. Altough having a different name and a formula in appearance different, this index is equivalent to the adjusted R2 (Allen, 2017, p. 382).
#'
#' } \subsection{Cohen's f}{
#' Cohen's f statistic is one appropriate effect size index to use for a oneway analysis of variance (ANOVA). Cohen's f can take on values between zero, when the population means are all equal, and an indefinitely large number as standard deviation of means increases relative to the average standard deviation within each group. Cohen has suggested that the values of 0.10, 0.25, and 0.40 represent small, medium, and large effect sizes, respectively.
#' }
#'
#' @examples
#' library(parameters)
#'
#' df <- iris
#' df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")
#'
#' model <- aov(Sepal.Length ~ Sepal.Big, data = df)
#' omega_squared(model)
#' eta_squared(model)
#' epsilon_squared(model)
#' cohens_f(model)
#'
#' model <- anova(lm(Sepal.Length ~ Sepal.Big * Species, data = df))
#' omega_squared(model)
#' eta_squared(model)
#' epsilon_squared(model)
#' \donttest{
#' # Don't work for now
#' model <- aov(Sepal.Length ~ Sepal.Big + Error(Species), data = df)
#' omega_squared(model)
#' eta_squared(model)
#' epsilon_squared(model)
#' }
#'
#' @return Data.frame containig the effect size values.
#'
#'
#' @references \itemize{
#'   \item Albers, C., \& Lakens, D. (2018). When power analyses based on pilot data are biased: Inaccurate effect size estimators and follow-up bias. Journal of experimental social psychology, 74, 187-195.
#'   \item Allen, R. (2017). Statistics and Experimental Design for Psychologists: A Model Comparison Approach. World Scientific Publishing Company.
#'   \item Field, A. (2013). Discovering statistics using IBM SPSS statistics. sage.
#'   \item Kelley, K. (2007). Methods for the behavioral, educational, and social sciences: An R package. Behavior Research Methods, 39(4), 979-984.
#'   \item Kelley, T. (1935) An unbiased correlation ratio measure. Proceedings of the National Academy of Sciences. 21(9). 554-559.
#' }
#'
#' The computation of CIs is based on the implementation done by Stanley (2018) in the \code{ApaTables} package and Kelley (2007) in the \code{MBESS} package. All credits go to them.
#'
#' @export
eta_squared <- function(model, partial = TRUE, ci = NULL, ...) {
  UseMethod("eta_squared")
}


#' @importFrom stats anova
#' @rdname eta_squared
#' @export
eta_squared.aov <- function(model, partial = TRUE, ci = NULL, iterations = 1000, ...) {
  if (!inherits(model, c("Gam", "aov", "anova", "anova.rms", "aovlist"))) model <- stats::anova(model)
  out <- .eta_squared(model, partial = partial, ci = ci, iterations = iterations)
  class(out) <- c(ifelse(isTRUE(partial), "partial_eta_squared", "eta_squared"), class(out))
  out
}


#' @export
eta_squared.lm <- eta_squared.aov

#' @export
eta_squared.glm <- eta_squared.aov

#' @export
eta_squared.anova <- eta_squared.aov


#' @export
eta_squared.aovlist <- function(model, partial = TRUE, ci = NULL, ...) {
  if (isFALSE(partial)) {
    warning("Currently only supports partial eta squared for repeated-measures ANOVAs.")
  }


  par_table <- .extract_parameters_anova(model)
  par_table <- split(par_table, par_table$Group)
  par_table <- lapply(par_table, function(.data) {
    .data$df2 <- .data$df[.data$Parameter == "Residuals"]
    .data
  })
  par_table <- do.call(rbind, par_table)
  .eta_square_from_F(par_table, ci = ci)
}




#' @export
eta_squared.merMod <- function(model, partial = TRUE, ci = NULL, ...) {
  if (isFALSE(partial)) {
    warning("Currently only supports partial eta squared for moxed models.")
  }

  if (!requireNamespace("lmerTest", quietly = TRUE)) {
    stop("Package 'lmerTest' required for this function to work. Please install it by running `install.packages('lmerTest')`.")
  }


  model <- lmerTest::as_lmerModLmerTest(model)
  par_table <- anova(model)
  par_table <- cbind(Parameter = rownames(par_table), par_table)
  colnames(par_table)[4:6] <- c("df", "df2", "F")
  .eta_square_from_F(par_table, ci = ci)
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






#' @keywords internal
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







#' @keywords internal
.ci_eta_squared <- function(x, partial, ci.lvl, df, statistic, model, iterations) {
  if (is.null(ci.lvl) || is.na(ci.lvl)) {
    return(x)
  }

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

    boot_function <- function(model, data, indices) {
      d <- data[indices, ] # allows boot to select sample
      fit <- stats::aov(insight::find_formula(model)$conditional, data = d)
      params <- .extract_parameters_anova(fit)
      values <- .values_aov(params)
      osq <- .extract_eta_squared(params, values, partial = FALSE)
      osq[["Eta_Sq"]]
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





#' @keywords internal
.eta_square_from_F <- function(.data, ci = NULL) {
  .data$Eta_Sq_partial <- .data$`F` * .data$df /
    (.data$`F` * .data$df + .data$df2)

  if (is.numeric(ci)) {
    .data$CI_high <- .data$CI_low <- NA
    for (i in seq_len(nrow(.data))) {
      if (!is.na(.data$`F`[i])) {
        cl <- .ci_partial_eta_squared(
          F.value = .data$`F`[i],
          df1 = .data$df[i],
          df2 = .data$df2[i],
          conf.level = ci
        )
        .data$CI_low[i] <- cl$LL
        .data$CI_high[i] <- cl$UL
      }
    }
  }

  rownames(.data) <- NULL
  .data <- .data[, colnames(.data) %in% c("Parameter", "Eta_Sq_partial", "CI_high", "CI_low")]
  class(.data) <- c("partial_eta_squared", class(.data))
  .data
}

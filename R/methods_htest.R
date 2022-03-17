#' Parameters from hypothesis tests
#'
#' Parameters of h-tests (correlations, t-tests, chi-squared, ...).
#'
#' @param model Object of class `htest` or `pairwise.htest`.
#' @param bootstrap Should estimates be bootstrapped?
#' @param cramers_v,phi Compute Cramer's V or phi as index of effect size.
#'   Can be `"raw"` or `"adjusted"` (effect size will be bias-corrected).
#'   Only applies to objects from `chisq.test()`.
#' @param cohens_g If `TRUE`, compute Cohen's g as index of effect size.
#'   Only applies to objects from `mcnemar.test()`.
#' @param standardized_d If `TRUE`, compute standardized d as index of
#'   effect size. Only applies to objects from `t.test()`. Calculation of
#'   `d` is based on the t-value (see [effectsize::t_to_d()])
#'   for details.
#' @param hedges_g If `TRUE`, compute Hedge's g as index of effect size.
#'   Only applies to objects from `t.test()`.
#' @param omega_squared,eta_squared,epsilon_squared Logical, if `TRUE`,
#'   returns the non-partial effect size Omega, Eta or Epsilon squared. Only
#'   applies to objects from `oneway.test()`.
#' @param rank_biserial If `TRUE`, compute the rank-biserial correlation as
#'   effect size measure. Only applies to objects from `wilcox.test()`.
#' @param rank_epsilon_squared If `TRUE`, compute the rank epsilon squared
#'   as effect size measure. Only applies to objects from `kruskal.test()`.
#' @param kendalls_w If `TRUE`, compute the Kendall's coefficient of
#'   concordance as effect size measure. Only applies to objects from
#'   `friedman.test()`.
#' @param ci Level of confidence intervals for effect size statistic. Currently
#'   only applies to objects from `chisq.test()` or `oneway.test()`.
#' @inheritParams model_parameters.default
#' @inheritParams model_parameters.aov
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#'
#' model <- cor.test(mtcars$mpg, mtcars$cyl, method = "pearson")
#' model_parameters(model)
#'
#' model <- t.test(iris$Sepal.Width, iris$Sepal.Length)
#' model_parameters(model, hedges_g = TRUE)
#'
#' model <- t.test(mtcars$mpg ~ mtcars$vs)
#' model_parameters(model, hedges_g = TRUE)
#'
#' model <- t.test(iris$Sepal.Width, mu = 1)
#' model_parameters(model, standardized_d = TRUE)
#'
#' data(airquality)
#' airquality$Month <- factor(airquality$Month, labels = month.abb[5:9])
#' model <- pairwise.t.test(airquality$Ozone, airquality$Month)
#' model_parameters(model)
#'
#' smokers <- c(83, 90, 129, 70)
#' patients <- c(86, 93, 136, 82)
#' model <- pairwise.prop.test(smokers, patients)
#' model_parameters(model)
#'
#' model <- stats::chisq.test(table(mtcars$am, mtcars$cyl))
#' model_parameters(model, cramers_v = "adjusted")
#'
#' @return A data frame of indices related to the model's parameters.
#'
#' @export
model_parameters.htest <- function(model,
                                   cramers_v = NULL,
                                   phi = NULL,
                                   standardized_d = NULL,
                                   hedges_g = NULL,
                                   omega_squared = NULL,
                                   eta_squared = NULL,
                                   epsilon_squared = NULL,
                                   cohens_g = NULL,
                                   rank_biserial = NULL,
                                   rank_epsilon_squared = NULL,
                                   kendalls_w = NULL,
                                   ci = .95,
                                   alternative = NULL,
                                   bootstrap = FALSE,
                                   verbose = TRUE,
                                   ...) {
  if (bootstrap) {
    stop("Bootstrapped h-tests are not yet implemented.")
  } else {
    parameters <- .extract_parameters_htest(
      model,
      cramers_v = cramers_v,
      phi = phi,
      standardized_d = standardized_d,
      hedges_g = hedges_g,
      omega_squared = omega_squared,
      eta_squared = eta_squared,
      epsilon_squared = epsilon_squared,
      cohens_g = cohens_g,
      rank_biserial = rank_biserial,
      rank_epsilon_squared = rank_epsilon_squared,
      kendalls_w = kendalls_w,
      ci = ci,
      alternative = alternative,
      verbose = verbose,
      ...
    )
  }

  if (!is.null(parameters$Method)) {
    parameters$Method <- trimws(gsub("with continuity correction", "", parameters$Method))
  }

  # save alternative
  parameters$Alternative <- model$alternative

  parameters <- .add_htest_parameters_attributes(parameters, model, ci, ...)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}


#' @export
standard_error.htest <- function(model, ...) {
  NULL
}


#' @export
p_value.htest <- function(model, ...) {
  model$p.value
}


# .pairwise.htest --------------------


#' @rdname model_parameters.htest
#' @export
model_parameters.pairwise.htest <- function(model, verbose = TRUE, ...) {
  m <- model$p.value
  parameters <-
    data.frame(
      Group1 = rep(rownames(m), each = ncol(m)),
      Group2 = rep(colnames(m), times = nrow(m)),
      p = as.numeric(t(m)),
      stringsAsFactors = FALSE
    )

  parameters <- stats::na.omit(parameters)

  parameters <- .add_htest_attributes(parameters, model, p_adjust = model$p.adjust.method)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}



# ==== extract parameters ====


#' @keywords internal
.extract_parameters_htest <- function(model,
                                      cramers_v = NULL,
                                      phi = NULL,
                                      standardized_d = NULL,
                                      hedges_g = NULL,
                                      omega_squared = NULL,
                                      eta_squared = NULL,
                                      epsilon_squared = NULL,
                                      cohens_g = NULL,
                                      rank_biserial = NULL,
                                      rank_epsilon_squared = NULL,
                                      kendalls_w = NULL,
                                      ci = 0.95,
                                      alternative = NULL,
                                      verbose = TRUE,
                                      ...) {
  m_info <- insight::model_info(model, verbose = FALSE, no_terms = TRUE)

  if (m_info$is_correlation) {

    # correlation ---------
    out <- .extract_htest_correlation(model)
  } else if (.is_levenetest(model)) {

    # levene's test ---------
    out <- .extract_htest_levenetest(model)
  } else if (m_info$is_ttest) {

    # t-test -----------
    out <- .extract_htest_ttest(model)
    out <- .add_effectsize_ttest(model,
      out,
      standardized_d,
      hedges_g,
      ci = ci,
      alternative = alternative,
      verbose = verbose,
      ...
    )
  } else if (m_info$is_ranktest) {

    # rank-test (kruskal / wilcox / friedman) -----------
    out <- .extract_htest_ranktest(model)

    if (grepl("^Wilcox", model$method)) {
      out <- .add_effectsize_rankbiserial(model,
        out,
        rank_biserial,
        ci = ci,
        verbose = verbose,
        ...
      )
    }

    if (grepl("^Kruskal", model$method)) {
      out <- .add_effectsize_rankepsilon(model,
        out,
        rank_epsilon_squared,
        ci = ci,
        verbose = verbose,
        ...
      )
    }

    if (grepl("^Friedman", model$method)) {
      out <- .add_effectsize_kendalls_w(model,
        out,
        kendalls_w,
        ci = ci,
        verbose = verbose,
        ...
      )
    }
  } else if (m_info$is_onewaytest) {

    # one-way test -----------
    out <- .extract_htest_oneway(model)
    out <- .add_effectsize_oneway(
      model,
      out,
      omega_squared,
      eta_squared,
      epsilon_squared,
      ci = ci,
      verbose = verbose
    )
  } else if (m_info$is_chi2test) {

    # chi2- and mcnemar-test -----------
    out <- .extract_htest_chi2(model)
    if (grepl("^McNemar", model$method)) {
      out <- .add_effectsize_mcnemar(model,
        out,
        cohens_g = cohens_g,
        ci = ci,
        verbose = verbose
      )
    } else {
      out <- .add_effectsize_chi2(
        model,
        out,
        cramers_v = cramers_v,
        phi = phi,
        ci = ci,
        alternative = alternative,
        verbose = verbose
      )
    }
  } else if (m_info$is_proptest) {

    # test of proportion --------------
    out <- .extract_htest_prop(model)
  } else if (m_info$is_binomtest) {

    # exact binomial test --------------
    out <- .extract_htest_binom(model)
  } else {
    stop("model_parameters not implemented for such h-tests yet.")
  }

  row.names(out) <- NULL
  out
}




# extract htest correlation ----------------------


.extract_htest_correlation <- function(model) {
  names <- unlist(strsplit(model$data.name, " (and|by) "))
  out <- data.frame(
    "Parameter1" = names[1],
    "Parameter2" = names[2],
    stringsAsFactors = FALSE
  )

  if (model$method == "Pearson's Chi-squared test") {
    out$Chi2 <- model$statistic
    out$df_error <- model$parameter
    out$p <- model$p.value
  } else if (grepl("Pearson", model$method, fixed = TRUE)) {
    out$r <- model$estimate
    out$t <- model$statistic
    out$df_error <- model$parameter
    out$p <- model$p.value
    out$CI_low <- model$conf.int[1]
    out$CI_high <- model$conf.int[2]
  } else if (grepl("Spearman", model$method, fixed = TRUE)) {
    out$rho <- model$estimate
    out$S <- model$statistic
    out$df_error <- model$parameter
    out$p <- model$p.value
  } else {
    out$tau <- model$estimate
    out$z <- model$statistic
    out$df_error <- model$parameter
    out$p <- model$p.value
  }

  out$Method <- model$method

  # reorder
  col_order <- c(
    "Parameter1", "Parameter2", "Parameter", "r", "rho", "tau", "CI_low", "CI_high",
    "t", "z", "S", "df_error", "p", "Method", "method"
  )

  out <- out[col_order[col_order %in% names(out)]]
  out
}




# extract htest ranktest ----------------------


.extract_htest_ranktest <- function(model) {
  # survey
  if (grepl("design-based", tolower(model$method), fixed = TRUE)) {
    names <- gsub("~", "", unlist(strsplit(model$data.name, " + ", fixed = TRUE)), fixed = TRUE)
    out <- data.frame(
      "Parameter1" = names[1],
      "Parameter2" = names[2],
      "Statistic" = model$statistic[[1]],
      "df_error" = model$parameter[[1]],
      "Method" = model$method,
      "p" = model$p.value[[1]],
      stringsAsFactors = FALSE
    )
    out$Method <- gsub("KruskalWallis", "Kruskal-Wallis", out$Method, fixed = TRUE)
    colnames(out)[colnames(out) == "Statistic"] <- names(model$statistic)[1]
  } else {
    if (grepl(" (and|by) ", model$data.name)) {
      names <- unlist(strsplit(model$data.name, " (and|by) "))
      out <- data.frame(
        "Parameter1" = names[1],
        "Parameter2" = names[2],
        stringsAsFactors = FALSE
      )
    } else {
      out <- data.frame(
        "Parameter" = model$data.name,
        stringsAsFactors = FALSE
      )
    }

    if (grepl("Wilcoxon", model$method, fixed = TRUE)) {
      out$W <- model$statistic[[1]]
      out$df_error <- model$parameter[[1]]
      out$p <- model$p.value[[1]]
    } else if (grepl("Kruskal-Wallis", model$method, fixed = TRUE) ||
      grepl("Friedman", model$method, fixed = TRUE)) {
      out$Chi2 <- model$statistic[[1]]
      out$df_error <- model$parameter[[1]]
      out$p <- model$p.value[[1]]
    }

    out$Method <- model$method
  }

  out
}




# extract htest leveneTest ----------------------


.extract_htest_levenetest <- function(model) {
  data.frame(
    "df" = model$Df[1],
    "df_error" = model$Df[2],
    `F` = model$`F value`[1],
    p = model$`Pr(>F)`[1],
    Method = "Levene's Test for Homogeneity of Variance",
    stringsAsFactors = FALSE
  )
}




# extract htest ttest ----------------------


.extract_htest_ttest <- function(model, standardized_d = NULL, hedges_g = NULL) {
  # survey
  if (grepl("design-based", tolower(model$method), fixed = TRUE)) {
    names <- unlist(strsplit(model$data.name, " ~ "))
    out <- data.frame(
      "Parameter1" = names[1],
      "Parameter2" = names[2],
      "Difference" = model$estimate[[1]],
      "t" = model$statistic[[1]],
      "df_error" = model$parameter[[1]],
      "Method" = model$method,
      "p" = model$p.value[[1]],
      stringsAsFactors = FALSE
    )
    out$Method <- gsub("KruskalWallis", "Kruskal-Wallis", out$Method, fixed = TRUE)
    colnames(out)[colnames(out) == "Statistic"] <- names(model$statistic)[1]
  } else {
    paired_test <- grepl("^Paired", model$method) && length(model$estimate) == 1
    if (grepl(" and ", model$data.name) && isFALSE(paired_test)) {
      names <- unlist(strsplit(model$data.name, " and ", fixed = TRUE))
      out <- data.frame(
        "Parameter1" = names[1],
        "Parameter2" = names[2],
        "Mean_Parameter1" = model$estimate[1],
        "Mean_Parameter2" = model$estimate[2],
        "Difference" = model$estimate[1] - model$estimate[2],
        "CI_low" = model$conf.int[1],
        "CI_high" = model$conf.int[2],
        "t" = model$statistic,
        "df_error" = model$parameter,
        "p" = model$p.value,
        "Method" = model$method,
        stringsAsFactors = FALSE
      )
      attr(out, "mean_group_values") <- gsub("mean in group ", "", names(model$estimate), fixed = TRUE)
    } else if (isTRUE(paired_test)) {
      names <- unlist(strsplit(model$data.name, " (and|by) "))
      out <- data.frame(
        "Parameter" = names[1],
        "Group" = names[2],
        "Difference" = model$estimate,
        "t" = model$statistic,
        "df_error" = model$parameter,
        "p" = model$p.value,
        "CI_low" = model$conf.int[1],
        "CI_high" = model$conf.int[2],
        "Method" = model$method,
        stringsAsFactors = FALSE
      )
    } else if (grepl(" by ", model$data.name, fixed = TRUE)) {
      if (length(model$estimate) == 1) {
        names <- unlist(strsplit(model$data.name, " by ", fixed = TRUE))
        out <- data.frame(
          "Parameter" = names[1],
          "Group" = names[2],
          "Difference" = model$estimate,
          "CI" = .95,
          "CI_low" = as.vector(model$conf.int[, 1]),
          "CI_high" = as.vector(model$conf.int[, 2]),
          "t" = model$statistic,
          "df_error" = model$parameter,
          "p" = model$p.value,
          "CI_low" = model$conf.int[1],
          "CI_high" = model$conf.int[2],
          "Method" = model$method,
          stringsAsFactors = FALSE
        )
      } else {
        names <- unlist(strsplit(model$data.name, " by ", fixed = TRUE))
        out <- data.frame(
          "Parameter" = names[1],
          "Group" = names[2],
          "Mean_Group1" = model$estimate[1],
          "Mean_Group2" = model$estimate[2],
          "Difference" = model$estimate[1] - model$estimate[2],
          "CI_low" = model$conf.int[1],
          "CI_high" = model$conf.int[2],
          "t" = model$statistic,
          "df_error" = model$parameter,
          "p" = model$p.value,
          "Method" = model$method,
          stringsAsFactors = FALSE
        )
        attr(out, "mean_group_values") <- gsub("mean in group ", "", names(model$estimate), fixed = TRUE)
      }
    } else {
      out <- data.frame(
        "Parameter" = model$data.name,
        "Mean" = model$estimate,
        "mu" = model$null.value,
        "Difference" = model$estimate - model$null.value,
        "CI_low" = model$conf.int[1],
        "CI_high" = model$conf.int[2],
        "t" = model$statistic,
        "df_error" = model$parameter,
        "p" = model$p.value,
        "Method" = model$method,
        stringsAsFactors = FALSE
      )
    }
  }

  attr(out, "htest_type") <- "ttest"
  out
}




# extract htest oneway ----------------------


.extract_htest_oneway <- function(model) {
  data.frame(
    "F" = model$statistic,
    "df" = model$parameter[1],
    "df_error" = model$parameter[2],
    "p" = model$p.value,
    "Method" = model$method,
    stringsAsFactors = FALSE
  )
}




# extract htest chi2 ----------------------


.extract_htest_chi2 <- function(model) {
  # survey-chisq-test
  if ((any("observed" %in% names(model)) && inherits(model$observed, "svytable")) ||
    any(grepl("^svychisq", model$data.name))) {
    if (grepl("Pearson's X", model$method, fixed = TRUE)) {
      model$method <- gsub("(Pearson's X\\^2: )(.*)", "Pearson's Chi2 \\(\\2\\)", model$method)
    }
    if (names(model$statistic) == "F") {
      data.frame(
        "F" = model$statistic,
        "df" = model$parameter[1],
        "df_error" = model$parameter[2],
        "p" = model$p.value,
        "Method" = model$method,
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        "Chi2" = model$statistic,
        "df" = model$parameter,
        "p" = model$p.value,
        "Method" = model$method,
        stringsAsFactors = FALSE
      )
    }
  } else {
    if (!is.null(model$estimate) && identical(names(model$estimate), "odds ratio")) {
      data.frame(
        "Odds Ratio" = model$estimate,
        # "CI" = attributes(model$conf.int)$conf.level,
        "CI_low" = model$conf.int[1],
        "CI_high" = model$conf.int[2],
        "p" = model$p.value,
        "Method" = model$method,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    } else {
      data.frame(
        "Chi2" = model$statistic,
        "df" = model$parameter,
        "p" = model$p.value,
        "Method" = model$method,
        stringsAsFactors = FALSE
      )
    }
  }
}




# extract htest prop ----------------------


.extract_htest_prop <- function(model) {
  out <- data.frame(
    Proportion = paste0(insight::format_value(model$estimate, as_percent = TRUE), collapse = " / "),
    stringsAsFactors = FALSE
  )
  if (length(model$estimate) == 2) {
    out$Difference <- insight::format_value(
      abs(model$estimate[1] - model$estimate[2]),
      as_percent = TRUE
    )
  }
  if (!is.null(model$conf.int)) {
    out$CI_low <- model$conf.int[1]
    out$CI_high <- model$conf.int[2]
  }
  out$Chi2 <- model$statistic
  out$df <- model$parameter[1]
  out$Null_value <- model$null.value
  out$p <- model$p.value
  out$Method <- model$method
  out
}




# extract htest binom ----------------------


.extract_htest_binom <- function(model) {
  out <- data.frame(
    "Probability" = model$estimate,
    "CI_low" = model$conf.int[1],
    "CI_high" = model$conf.int[2],
    "Success" = model$statistic,
    "Trials" = model$parameter,
    stringsAsFactors = FALSE
  )
  out$Null_value <- model$null.value
  out$p <- model$p.value
  out$Method <- model$method
  out
}





# ==== effectsizes =====


# effectsize Chi2 (Phi / Cramer's V) ----

.add_effectsize_chi2 <- function(model,
                                 out,
                                 cramers_v = NULL,
                                 phi = NULL,
                                 ci = .95,
                                 alternative = NULL,
                                 verbose = TRUE) {
  if (!requireNamespace("effectsize", quietly = TRUE) || (is.null(cramers_v) && is.null(phi))) {
    return(out)
  }

  if (!is.null(cramers_v)) {
    # Cramers V
    es <- effectsize::effectsize(
      model,
      type = "cramers_v",
      ci = ci,
      alternative = alternative,
      adjust = identical(cramers_v, "adjusted"),
      verbose = verbose
    )
    es$CI <- NULL
    ci_cols <- grepl("^CI", names(es))
    names(es)[ci_cols] <- paste0("Cramers_", names(es)[ci_cols])
    out <- cbind(out, es)
  }

  if (!is.null(phi)) {
    # Phi
    es <- effectsize::effectsize(
      model,
      type = "phi",
      ci = ci,
      alternative = alternative,
      adjust = identical(phi, "adjusted"),
      verbose = verbose
    )
    es$CI <- NULL
    ci_cols <- grepl("^CI", names(es))
    names(es)[ci_cols] <- paste0("phi_", names(es)[ci_cols])
    out <- cbind(out, es)
  }

  # reorder
  col_order <- c(
    "Chi2", "df", "df_error", "Cramers_v", "Cramers_v_adjusted", "Cramers_CI_low",
    "Cramers_CI_high", "phi", "phi_adjusted", "phi_CI_low",
    "phi_CI_high", "p", "Method", "method"
  )
  out <- out[col_order[col_order %in% names(out)]]
  out
}




# effectsize Chi2 (McNemar) ----

.add_effectsize_mcnemar <- function(model,
                                    out,
                                    cohens_g = NULL,
                                    ci = .95,
                                    verbose = TRUE) {
  if (is.null(cohens_g)) {
    return(out)
  }

  if (requireNamespace("effectsize", quietly = TRUE)) {
    es <- effectsize::effectsize(model, type = "cohens_g", ci = ci, verbose = verbose)
    es$CI <- NULL
    ci_cols <- grepl("^CI", names(es))
    names(es)[ci_cols] <- paste0("Cohens_", names(es)[ci_cols])
    out <- cbind(out, es)
  }

  # reorder
  col_order <- c(
    "Chi2", "df", "df_error", "Cohens_g", "g", "Cohens_CI_low",
    "Cohens_CI_high", "p", "Method", "method"
  )
  out <- out[col_order[col_order %in% names(out)]]
  out
}




# effectsize Cohen's d / Hedges g (t-test) ----

.add_effectsize_ttest <- function(model,
                                  out,
                                  standardized_d = NULL,
                                  hedges_g = NULL,
                                  ci = .95,
                                  alternative = NULL,
                                  verbose = TRUE,
                                  ...) {
  if (is.null(standardized_d) && is.null(hedges_g)) {
    return(out)
  }

  if (requireNamespace("effectsize", quietly = TRUE)) {

    # standardized d
    if (!is.null(standardized_d)) {
      es <- effectsize::effectsize(
        model,
        type = "cohens_d",
        ci = ci,
        alternative = alternative,
        verbose = verbose,
        ...
      )
      es$CI <- NULL
      ci_cols <- grepl("^CI", names(es))
      names(es)[ci_cols] <- paste0("d_", names(es)[ci_cols])
      out <- cbind(out, es)
    }

    # Hedge's g
    if (!is.null(hedges_g)) {
      es <- effectsize::effectsize(
        model,
        type = "hedges_g",
        ci = ci,
        alternative = alternative,
        verbose = verbose,
        ...
      )
      es$CI <- NULL
      ci_cols <- grepl("^CI", names(es))
      names(es)[ci_cols] <- paste0("g_", names(es)[ci_cols])
      out <- cbind(out, es)
    }
  }

  # reorder
  col_order <- c(
    "Parameter1", "Parameter2", "Parameter", "Group",
    "Mean_Parameter1", "Mean_Parameter2", "Mean_Group1", "Mean_Group2",
    "mu", "Difference", "CI_low", "CI_high",
    "t", "df_error",
    "d", "Cohens_d", "d_CI_low", "d_CI_high",
    "g", "Hedges_g", "g_CI_low", "g_CI_high",
    "p", "Method", "method"
  )

  out <- out[col_order[col_order %in% names(out)]]
  out
}




# effectsize r (rank biserial) ----

.add_effectsize_rankbiserial <- function(model,
                                         out,
                                         rank_biserial = NULL,
                                         ci = .95,
                                         verbose = TRUE,
                                         ...) {
  if (is.null(rank_biserial)) {
    return(out)
  }

  if (requireNamespace("effectsize", quietly = TRUE)) {
    es <- effectsize::effectsize(model,
      type = "rank_biserial",
      ci = ci,
      verbose = verbose,
      ...
    )
    es$CI <- NULL
    ci_cols <- grepl("^CI", names(es))
    names(es)[ci_cols] <- paste0("rank_biserial_", names(es)[ci_cols])
    out <- cbind(out, es)
  }

  # reorder
  col_order <- c(
    "Parameter1", "Parameter2", "Parameter", "W", "r_rank_biserial", "CI",
    "rank_biserial_CI_low", "rank_biserial_CI_high", "p", "Method", "method"
  )

  out <- out[col_order[col_order %in% names(out)]]
  out
}



# effectsize rank Epsilon2 ----

.add_effectsize_rankepsilon <- function(model,
                                        out,
                                        rank_epsilon_squared = NULL,
                                        ci = .95,
                                        verbose = TRUE,
                                        ...) {
  if (is.null(rank_epsilon_squared)) {
    return(out)
  }

  if (requireNamespace("effectsize", quietly = TRUE)) {
    es <- effectsize::effectsize(model,
      type = "rank_epsilon_squared",
      ci = ci,
      verbose = verbose,
      ...
    )
    es$CI <- NULL
    ci_cols <- grepl("^CI", names(es))
    names(es)[ci_cols] <- paste0("rank_epsilon_squared_", names(es)[ci_cols])
    out <- cbind(out, es)
  }

  # reorder
  col_order <- c(
    "Parameter1", "Parameter2", "Parameter", "Chi2", "df_error",
    "rank_epsilon_squared", "CI", "rank_epsilon_squared_CI_low", "rank_epsilon_squared_CI_high",
    "p", "Method", "method"
  )

  out <- out[col_order[col_order %in% names(out)]]
  out
}




# effectsize Kendall's W ----

.add_effectsize_kendalls_w <- function(model,
                                       out,
                                       kendalls_w = NULL,
                                       ci = .95,
                                       verbose = TRUE,
                                       ...) {
  if (is.null(kendalls_w)) {
    return(out)
  }

  if (requireNamespace("effectsize", quietly = TRUE)) {
    es <- effectsize::effectsize(model,
      type = "kendalls_w",
      ci = ci,
      verbose = verbose,
      ...
    )
    es$CI <- NULL
    ci_cols <- grepl("^CI", names(es))
    names(es)[ci_cols] <- paste0("Kendalls_W_", names(es)[ci_cols])
    out <- cbind(out, es)
  }

  # reorder
  col_order <- c(
    "Parameter1", "Parameter2", "Parameter", "Chi2", "df_error", "Kendalls_W", "CI",
    "Kendalls_W_CI_low", "Kendalls_W_CI_high", "p", "Method", "method"
  )

  out <- out[col_order[col_order %in% names(out)]]
  out
}




# effectsize One-way Anova (Omega, Eta, ...) ----

.add_effectsize_oneway <- function(model,
                                   out,
                                   omega_squared = NULL,
                                   eta_squared = NULL,
                                   epsilon_squared = NULL,
                                   ci = .95,
                                   verbose = TRUE) {
  if (is.null(omega_squared) && is.null(eta_squared) && is.null(epsilon_squared)) {
    return(out)
  }

  if (requireNamespace("effectsize", quietly = TRUE)) {
    # omega_squared
    if (!is.null(omega_squared)) {
      es <- effectsize::effectsize(model, ci = ci, type = "omega", partial = TRUE, verbose = verbose)
      es$CI <- NULL
      ci_cols <- grepl("^CI", names(es))
      names(es)[ci_cols] <- paste0("Omega2_", names(es)[ci_cols])
      out <- cbind(out, es)
    }

    # eta squared
    if (!is.null(eta_squared)) {
      es <- effectsize::effectsize(model, ci = ci, type = "eta", partial = TRUE, verbose = verbose)
      es$CI <- NULL
      ci_cols <- grepl("^CI", names(es))
      names(es)[ci_cols] <- paste0("Eta2_", names(es)[ci_cols])
      out <- cbind(out, es)
    }

    # epsilon squared
    if (!is.null(epsilon_squared)) {
      es <- effectsize::effectsize(model, ci = ci, type = "epsilon", partial = TRUE, verbose = verbose)
      es$CI <- NULL
      ci_cols <- grepl("^CI", names(es))
      names(es)[ci_cols] <- paste0("Epsilon2_", names(es)[ci_cols])
      out <- cbind(out, es)
    }
  }

  # reorder
  col_order <- c(
    "F", "df", "df_error", "Eta2", "Eta2_CI_low", "Eta2_CI_high",
    "Omega2", "Omega2_CI_low", "Omega2_CI_high", "Epsilon2",
    "Epsilon2_CI_low", "Epsilon2_CI_high", "p", "Method", "method"
  )
  out <- out[col_order[col_order %in% names(out)]]
  out
}




# ==== add attributes ====


.add_htest_parameters_attributes <- function(params, model, ci = 0.95, ...) {
  attr(params, "title") <- unique(params$Method)
  attr(params, "model_class") <- class(model)
  attr(params, "alternative") <- model$alternative

  if (!is.null(model$alternative)) {
    h1_text <- "Alternative hypothesis: "
    if (!is.null(model$null.value)) {
      if (length(model$null.value) == 1L) {
        alt.char <- switch(model$alternative,
          two.sided = "not equal to",
          less = "less than",
          greater = "greater than"
        )
        h1_text <- paste0(h1_text, "true ", names(model$null.value), " is ", alt.char, " ", model$null.value)
      } else {
        h1_text <- paste0(h1_text, model$alternative)
      }
    } else {
      h1_text <- paste0(h1_text, model$alternative)
    }
    attr(params, "text_alternative") <- h1_text
  }

  dot.arguments <- lapply(match.call(expand.dots = FALSE)$`...`, function(x) x)
  if ("digits" %in% names(dot.arguments)) {
    attr(params, "digits") <- eval(dot.arguments[["digits"]])
  } else {
    attr(params, "digits") <- 2
  }

  if ("ci_digits" %in% names(dot.arguments)) {
    attr(params, "ci_digits") <- eval(dot.arguments[["ci_digits"]])
  } else {
    attr(params, "ci_digits") <- 2
  }

  if ("p_digits" %in% names(dot.arguments)) {
    attr(params, "p_digits") <- eval(dot.arguments[["p_digits"]])
  } else {
    attr(params, "p_digits") <- 3
  }

  attr(params, "ci") <- ci
  attr(params, "ci_test") <- attributes(model$conf.int)$conf.level

  # add CI, and reorder
  if (!"CI" %in% colnames(params) && length(ci) == 1) {
    ci_pos <- grep("CI_low", colnames(params), fixed = TRUE)
    if (length(ci_pos)) {
      if (length(ci_pos) > 1) {
        ci_pos <- ci_pos[1]
      }
      params$CI <- ci
      a <- attributes(params)
      params <- params[c(1:(ci_pos - 1), ncol(params), ci_pos:(ncol(params) - 1))]
      attributes(params) <- utils::modifyList(a, attributes(params))
    }
  }

  params
}




#' @keywords internal
.add_htest_attributes <- function(params,
                                  model,
                                  p_adjust = NULL,
                                  verbose = TRUE,
                                  ...) {
  dot.arguments <- lapply(match.call(expand.dots = FALSE)$`...`, function(x) x)

  attr(params, "p_adjust") <- p_adjust
  attr(params, "model_class") <- class(model)
  attr(params, "title") <- params$Method

  if ("digits" %in% names(dot.arguments)) {
    attr(params, "digits") <- eval(dot.arguments[["digits"]])
  } else {
    attr(params, "digits") <- 2
  }

  if ("ci_digits" %in% names(dot.arguments)) {
    attr(params, "ci_digits") <- eval(dot.arguments[["ci_digits"]])
  } else {
    attr(params, "ci_digits") <- 2
  }

  if ("p_digits" %in% names(dot.arguments)) {
    attr(params, "p_digits") <- eval(dot.arguments[["p_digits"]])
  } else {
    attr(params, "p_digits") <- 3
  }

  if ("s_value" %in% names(dot.arguments)) {
    attr(params, "s_value") <- eval(dot.arguments[["s_value"]])
  }

  params
}

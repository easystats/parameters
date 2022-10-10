#' Parameters from hypothesis tests
#'
#' Parameters of h-tests (correlations, t-tests, chi-squared, ...).
#'
#' @param model Object of class `htest` or `pairwise.htest`.
#' @param bootstrap Should estimates be bootstrapped?
#' @param ci Level of confidence intervals for effect size statistic. Currently
#'   only applies to objects from `chisq.test()` or `oneway.test()`.
#' @inheritParams model_parameters.default
#' @inheritParams model_parameters.aov
#' @param cramers_v,phi,cohens_g,standardized_d,hedges_g,omega_squared,eta_squared,epsilon_squared,rank_biserial,rank_epsilon_squared,kendalls_w Deprecated. Please use `effectsize_type`.
#'
#' @inherit effectsize::effectsize details
#'
#' @examples
#'
#' model <- cor.test(mtcars$mpg, mtcars$cyl, method = "pearson")
#' model_parameters(model)
#'
#' model <- t.test(iris$Sepal.Width, iris$Sepal.Length)
#' model_parameters(model, effectsize_type = "hedges_g")
#'
#' model <- t.test(mtcars$mpg ~ mtcars$vs)
#' model_parameters(model, effectsize_type = "hedges_g")
#'
#' model <- t.test(iris$Sepal.Width, mu = 1)
#' model_parameters(model, effectsize_type = "cohens_d")
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
#' model_parameters(model, effectsize_type = "cramers_v")
#'
#' @return A data frame of indices related to the model's parameters.
#'
#' @export
model_parameters.htest <- function(model,
                                   ci = 0.95,
                                   alternative = NULL,
                                   bootstrap = FALSE,
                                   effectsize_type = NULL,
                                   verbose = TRUE,
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
                                   ...) {
  ## TODO: remove in a later update
  # handle deprected arguments ------
  if (!is.null(cramers_v)) effectsize_type <- "cramers_v"
  if (!is.null(phi)) effectsize_type <- "phi"
  if (!is.null(standardized_d)) effectsize_type <- "standardized_d"
  if (!is.null(hedges_g)) effectsize_type <- "hedges_g"
  if (!is.null(omega_squared)) effectsize_type <- "omega_squared"
  if (!is.null(eta_squared)) effectsize_type <- "eta_squared"
  if (!is.null(epsilon_squared)) effectsize_type <- "epsilon_squared"
  if (!is.null(cohens_g)) effectsize_type <- "cohens_g"
  if (!is.null(rank_biserial)) effectsize_type <- "rank_biserial"
  if (!is.null(rank_epsilon_squared)) effectsize_type <- "rank_epsilon_squared"
  if (!is.null(kendalls_w)) effectsize_type <- "rank_epsilon_squared"


  if (bootstrap) {
    stop("Bootstrapped h-tests are not yet implemented.", call. = FALSE)
  } else {
    parameters <- .extract_parameters_htest(
      model,
      effectsize_type = effectsize_type,
      ci = ci,
      alternative = alternative,
      verbose = verbose,
      ...
    )
  }

  if (!is.null(parameters$Method)) {
    parameters$Method <- insight::trim_ws(gsub("with continuity correction", "", parameters$Method, fixed = TRUE))
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



# survey-table --------------------

#' @export
model_parameters.svytable <- function(model, verbose = TRUE, ...) {
  model_parameters(summary(model)$statistic, verbose = verbose, ...)
}



# ==== extract parameters ====

#' @keywords internal
.extract_parameters_htest <- function(model,
                                      effectsize_type = NULL,
                                      ci = 0.95,
                                      alternative = NULL,
                                      verbose = TRUE,
                                      ...) {
  m_info <- insight::model_info(model, verbose = FALSE)

  if (m_info$is_correlation) {
    # correlation ---------
    out <- .extract_htest_correlation(model)
  } else if (.is_levenetest(model)) {
    # levene's test ---------
    out <- .extract_htest_levenetest(model)
  } else if (m_info$is_ttest) {
    # t-test -----------
    out <- .extract_htest_ttest(model)
  } else if (m_info$is_ranktest) {
    # rank-test (kruskal / wilcox / friedman) -----------
    out <- .extract_htest_ranktest(model)
  } else if (m_info$is_onewaytest) {
    # one-way test -----------
    out <- .extract_htest_oneway(model)
  } else if (m_info$is_chi2test) {
    # chi2- and mcnemar-test -----------
    out <- .extract_htest_chi2(model)
  } else if (m_info$is_proptest) {
    # test of proportion --------------
    out <- .extract_htest_prop(model)
  } else if (m_info$is_binomtest) {
    # exact binomial test --------------
    out <- .extract_htest_binom(model)
  } else if (m_info$is_ftest) {
    # F test for equal variances --------------
    out <- .extract_htest_vartest(model)
  } else {
    insight::format_error("`model_parameters()` not implemented for such h-tests yet.")
  }

  out <- .add_effectsize_htest(model,
    out,
    effectsize_type = effectsize_type,
    ci = ci,
    alternative = alternative,
    verbose = verbose,
    ...
  )

  row.names(out) <- NULL
  out
}




# extract htest correlation ----------------------

#' @keywords internal
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

#' @keywords internal
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

#' @keywords internal
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




# extract htest var.test ----------------------

#' @keywords internal
.extract_htest_vartest <- function(model) {
  data.frame(
    "Parameter" = model$data.name,
    "Estimate" = model$estimate,
    "df" = model$parameter[1],
    "df_error" = model$parameter[2],
    `F` = model$statistic,
    "CI_low" = model$conf.int[1],
    "CI_high" = model$conf.int[2],
    p = model$p.value,
    Method = "F test to compare two variances",
    stringsAsFactors = FALSE
  )
}




# extract htest ttest ----------------------

#' @keywords internal
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
    paired_test <- startsWith(model$method, "Paired") && length(model$estimate) == 1
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

#' @keywords internal
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

#' @keywords internal
.extract_htest_chi2 <- function(model) {
  # survey-chisq-test
  if ((any("observed" %in% names(model)) && inherits(model$observed, "svytable")) ||
    any(startsWith(model$data.name, "svychisq"))) {
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

#' @keywords internal
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

#' @keywords internal
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

.add_effectsize_htest <- function(model,
                                  out,
                                  effectsize_type = NULL,
                                  ci = 0.95,
                                  alternative = NULL,
                                  verbose = TRUE,
                                  ...) {
  # check if effect sizes are requested
  if (!requireNamespace("effectsize", quietly = TRUE) || is.null(effectsize_type)) {
    return(out)
  }

  # try to extract effectsize
  es <- tryCatch(
    {
      effectsize::effectsize(
        model,
        type = effectsize_type,
        ci = ci,
        alternative = alternative,
        verbose = verbose,
        ...
      )
    },
    error = function(e) {
      if (verbose) {
        msg <- c(
          paste0("Could not compute effectsize ", effectsize::get_effectsize_label(effectsize_type), "."),
          paste0("Possible reason: ", e$message)
        )
        insight::format_alert(msg)
      }
      NULL
    }
  )
  # return if not successful
  if (is.null(es)) {
    return(out)
  }

  ## TODO: check if effectsize prefixes are correct @mattansb

  # Find prefix for CI-columns
  prefix <- switch(effectsize_type,
    "cohens_g" = "Cohens_",
    "cramers_v" = "Cramers_",
    "phi" = "phi_",
    "cohens_d" = "d_",
    "hedges_g" = "g_",
    "rank_biserial" = "rank_biserial_",
    "rank_epsilon_squared" = "rank_epsilon_squared_",
    "kendalls_w" = "W_",
    "omega" = "Omega2_",
    "eta" = "Eta2_",
    "epsilon" = "Epsilon2_"
  )

  es$CI <- NULL
  ci_cols <- startsWith(names(es), "CI")
  es_ci_cols <- paste0(prefix, names(es)[ci_cols])
  names(es)[ci_cols] <- es_ci_cols
  out <- cbind(out, es)

  # compose effect size columns
  es_columns <- unique(c(effectsize::get_effectsize_name(colnames(es)), es_ci_cols))

  # reorder
  col_order <- c(
    "Parameter1", "Parameter2", "Parameter", "F", "Chi2", "Group",
    "Mean_Parameter1", "Mean_Parameter2", "Mean_Group1", "Mean_Group2", "mu",
    "Difference", "W", "CI_low", "CI_high", es_columns, "t", "df", "df_error",
    "p", "Method", "method"
  )
  out <- out[col_order[col_order %in% names(out)]]
  out
}




# ==== add attributes ====

#' @keywords internal
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

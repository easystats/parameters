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
#' @inherit effectsize::effectsize details
#'
#' @examples
#'
#' model <- cor.test(mtcars$mpg, mtcars$cyl, method = "pearson")
#' model_parameters(model)
#'
#' model <- t.test(iris$Sepal.Width, iris$Sepal.Length)
#' model_parameters(model, es_type = "hedges_g")
#'
#' model <- t.test(mtcars$mpg ~ mtcars$vs)
#' model_parameters(model, es_type = "hedges_g")
#'
#' model <- t.test(iris$Sepal.Width, mu = 1)
#' model_parameters(model, es_type = "cohens_d")
#'
#' data(airquality)
#' airquality$Month <- factor(airquality$Month, labels = month.abb[5:9])
#' model <- pairwise.t.test(airquality$Ozone, airquality$Month)
#' model_parameters(model)
#'
#' smokers <- c(83, 90, 129, 70)
#' patients <- c(86, 93, 136, 82)
#' model <- suppressWarnings(pairwise.prop.test(smokers, patients))
#' model_parameters(model)
#'
#' model <- suppressWarnings(chisq.test(table(mtcars$am, mtcars$cyl)))
#' model_parameters(model, es_type = "cramers_v")
#'
#' @return A data frame of indices related to the model's parameters.
#'
#' @export
model_parameters.htest <- function(
  model,
  ci = 0.95,
  alternative = NULL,
  bootstrap = FALSE,
  es_type = NULL,
  verbose = TRUE,
  ...
) {
  if (bootstrap) {
    insight::format_error("Bootstrapped h-tests are not yet implemented.")
  }

  # Extract test-specific parameters based on the type of htest
  parameters <- .extract_parameters_htest(
    model,
    es_type = es_type,
    ci = ci,
    alternative = alternative,
    verbose = verbose,
    ...
  )

  # Clean up the method name for better printing
  if (!is.null(parameters$Method)) {
    parameters$Method <- insight::trim_ws(gsub(
      "with continuity correction",
      "",
      parameters$Method,
      fixed = TRUE
    ))
  }

  # Save alternative hypothesis direction
  parameters$Alternative <- model$alternative

  # Add necessary attributes for printing and further processing
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

#' @export
model_parameters.pairwise.htest <- function(model, verbose = TRUE, ...) {
  m <- model$p.value

  # Convert the pairwise p-value matrix into a long-format data frame
  parameters <- data.frame(
    Group1 = rep(rownames(m), each = ncol(m)),
    Group2 = rep(colnames(m), times = nrow(m)),
    p = as.numeric(t(m)),
    stringsAsFactors = FALSE
  )

  # Remove NA comparisons (e.g., upper/lower triangle elements of the matrix)
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
.extract_parameters_htest <- function(
  model,
  es_type = NULL,
  ci = 0.95,
  alternative = NULL,
  verbose = TRUE,
  ...
) {
  # insight::model_info identifies the specific type of the htest
  m_info <- insight::model_info(model, verbose = FALSE)

  # Route to the appropriate extraction function based on test type
  if (!is.null(model$method) && startsWith(model$method, "Box-")) {
    out <- .extract_htest_boxpierce(model)
  } else if (m_info$is_correlation) {
    out <- .extract_htest_correlation(model)
  } else if (.is_levenetest(model)) {
    out <- .extract_htest_levenetest(model)
  } else if (m_info$is_ttest) {
    out <- .extract_htest_ttest(model)
  } else if (isTRUE(m_info$is_ztest)) {
    # requires insight > 1.5.0
    out <- .extract_htest_ztest(model)
  } else if (m_info$is_ranktest) {
    out <- .extract_htest_ranktest(model)
  } else if (m_info$is_onewaytest) {
    out <- .extract_htest_oneway(model)
  } else if (m_info$is_chi2test) {
    out <- .extract_htest_chi2(model)
  } else if (m_info$is_proptest) {
    out <- .extract_htest_prop(model)
  } else if (m_info$is_binomtest) {
    out <- .extract_htest_binom(model)
  } else if (m_info$is_ftest) {
    out <- .extract_htest_vartest(model)
  } else {
    insight::format_error("`model_parameters()` not implemented for such h-tests yet.")
  }

  # Compute and attach effect size if requested and available
  out <- .add_effectsize_htest(
    model,
    out,
    es_type = es_type,
    ci = ci,
    alternative = alternative,
    verbose = verbose,
    ...
  )

  row.names(out) <- NULL
  out
}


# extract htest Box-Pierce ----------------------

#' @keywords internal
.extract_htest_boxpierce <- function(model) {
  out <- data.frame(
    Parameter = model$data.name,
    Chi2 = model$statistic,
    df_error = model$parameter,
    p = model$p.value,
    Method = model$method,
    stringsAsFactors = FALSE
  )

  attr(out, "htest_type") <- "boxpiercetest"
  out
}


# extract htest correlation ----------------------

#' @keywords internal
.extract_htest_correlation <- function(model) {
  # Parse the variables involved in the correlation
  data_names <- unlist(strsplit(model$data.name, " (and|by) "))
  out <- data.frame(
    Parameter1 = data_names[1],
    Parameter2 = data_names[2],
    stringsAsFactors = FALSE
  )

  # Assign appropriate statistic names and estimates based on correlation method
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
    # Kendall's tau
    out$tau <- model$estimate
    out$z <- model$statistic
    out$df_error <- model$parameter
    out$p <- model$p.value
  }

  out$Method <- model$method

  # Standardize column order
  col_order <- c(
    "Parameter1",
    "Parameter2",
    "Parameter",
    "r",
    "rho",
    "tau",
    "CI_low",
    "CI_high",
    "t",
    "z",
    "S",
    "df_error",
    "p",
    "Method",
    "method"
  )

  out <- out[col_order[col_order %in% names(out)]]
  attr(out, "htest_type") <- "cortest"
  out
}


# extract htest ranktest ----------------------

#' @keywords internal
.extract_htest_ranktest <- function(model) {
  # Handle design-based tests from survey package
  if (grepl("design-based", tolower(model$method), fixed = TRUE)) {
    data_names <- gsub(
      "~",
      "",
      unlist(strsplit(model$data.name, " + ", fixed = TRUE)),
      fixed = TRUE
    )
    out <- data.frame(
      Parameter1 = data_names[1],
      Parameter2 = data_names[2],
      Statistic = model$statistic[[1]],
      df_error = model$parameter[[1]],
      Method = model$method,
      p = model$p.value[[1]],
      stringsAsFactors = FALSE
    )
    out$Method <- gsub("KruskalWallis", "Kruskal-Wallis", out$Method, fixed = TRUE)
    colnames(out)[colnames(out) == "Statistic"] <- names(model$statistic)[1]
  } else {
    # Standard rank tests
    if (grepl(" (and|by) ", model$data.name)) {
      data_names <- unlist(strsplit(model$data.name, " (and|by) "))
      out <- data.frame(
        Parameter1 = data_names[1],
        Parameter2 = data_names[2],
        stringsAsFactors = FALSE
      )
    } else {
      out <- data.frame(Parameter = model$data.name, stringsAsFactors = FALSE)
    }

    if (grepl("Wilcoxon", model$method, fixed = TRUE)) {
      out$W <- model$statistic[[1]]
      out$df_error <- model$parameter[[1]]
      out$p <- model$p.value[[1]]
    } else if (
      grepl("Kruskal-Wallis", model$method, fixed = TRUE) ||
        grepl("Friedman", model$method, fixed = TRUE)
    ) {
      out$Chi2 <- model$statistic[[1]]
      out$df_error <- model$parameter[[1]]
      out$p <- model$p.value[[1]]
    }

    out$Method <- model$method
  }

  attr(out, "htest_type") <- "ranktest"
  out
}


# extract htest leveneTest ----------------------

#' @keywords internal
.extract_htest_levenetest <- function(model) {
  out <- data.frame(
    df = model$Df[1],
    df_error = model$Df[2],
    `F` = model$`F value`[1], # nolint
    p = model$`Pr(>F)`[1],
    Method = "Levene's Test for Homogeneity of Variance",
    stringsAsFactors = FALSE
  )

  attr(out, "htest_type") <- "levenetest"
  out
}


# extract htest var.test ----------------------

#' @keywords internal
.extract_htest_vartest <- function(model) {
  out <- data.frame(
    Parameter = model$data.name,
    Estimate = model$estimate,
    df = model$parameter[1],
    df_error = model$parameter[2],
    `F` = model$statistic, # nolint
    CI_low = model$conf.int[1],
    CI_high = model$conf.int[2],
    p = model$p.value,
    Method = "F test to compare two variances",
    stringsAsFactors = FALSE
  )

  attr(out, "htest_type") <- "vartest"
  out
}


# extract htest ttest ----------------------

#' @keywords internal
.extract_htest_ttest <- function(model, standardized_d = NULL, hedges_g = NULL) {
  # Package BSDA names the element "parameters" instead of "parameter", so we
  # pick the right name here, to avoid partial matching warnings/issues.
  if ("parameters" %in% names(model)) {
    model$parameter <- model$parameters
  }

  # Survey-weighted t-tests
  if (grepl("design-based", tolower(model$method), fixed = TRUE)) {
    data_names <- unlist(strsplit(model$data.name, " ~ ", fixed = TRUE))
    out <- data.frame(
      Parameter1 = data_names[1],
      Parameter2 = data_names[2],
      Difference = model$estimate[[1]],
      t = model$statistic[[1]],
      df_error = model$parameter[[1]],
      Method = model$method,
      p = model$p.value[[1]],
      stringsAsFactors = FALSE
    )
    out$Method <- gsub("KruskalWallis", "Kruskal-Wallis", out$Method, fixed = TRUE)
    colnames(out)[colnames(out) == "Statistic"] <- names(model$statistic)[1]
  } else {
    paired_test <- startsWith(model$method, "Paired") && length(model$estimate) == 1

    # Independent t-test with two vectors (x and y)
    if (grepl(" and ", model$data.name, fixed = TRUE) && isFALSE(paired_test)) {
      data_names <- unlist(strsplit(model$data.name, " and ", fixed = TRUE))
      out <- data.frame(
        Parameter1 = data_names[1],
        Parameter2 = data_names[2],
        Mean_Parameter1 = model$estimate[1],
        Mean_Parameter2 = model$estimate[2],
        Difference = model$estimate[1] - model$estimate[2],
        CI_low = model$conf.int[1],
        CI_high = model$conf.int[2],
        t = model$statistic,
        df_error = model$parameter,
        p = model$p.value,
        Method = model$method,
        stringsAsFactors = FALSE
      )
      attr(out, "mean_group_values") <- gsub(
        "mean in group ",
        "",
        names(model$estimate),
        fixed = TRUE
      )

      # Paired t-test
    } else if (isTRUE(paired_test)) {
      data_names <- unlist(strsplit(model$data.name, " (and|by) "))
      out <- data.frame(
        Parameter = data_names[1],
        Group = data_names[2],
        Difference = model$estimate,
        t = model$statistic,
        df_error = model$parameter,
        p = model$p.value,
        CI_low = model$conf.int[1],
        CI_high = model$conf.int[2],
        Method = model$method,
        stringsAsFactors = FALSE
      )

      # Independent t-test using formula (y ~ x)
    } else if (grepl(" by ", model$data.name, fixed = TRUE)) {
      data_names <- unlist(strsplit(model$data.name, " by ", fixed = TRUE))

      # If only the difference is estimated
      if (length(model$estimate) == 1) {
        out <- data.frame(
          Parameter = data_names[1],
          Group = data_names[2],
          Difference = model$estimate,
          CI = 0.95,
          CI_low = as.vector(model$conf.int[, 1]),
          CI_high = as.vector(model$conf.int[, 2]),
          t = model$statistic,
          df_error = model$parameter,
          p = model$p.value,
          Method = model$method,
          stringsAsFactors = FALSE
        )
        # If both group means are estimated
      } else {
        out <- data.frame(
          Parameter = data_names[1],
          Group = data_names[2],
          Mean_Group1 = model$estimate[1],
          Mean_Group2 = model$estimate[2],
          Difference = model$estimate[1] - model$estimate[2],
          CI_low = model$conf.int[1],
          CI_high = model$conf.int[2],
          t = model$statistic,
          df_error = model$parameter,
          p = model$p.value,
          Method = model$method,
          stringsAsFactors = FALSE
        )
        attr(out, "mean_group_values") <- gsub(
          "mean in group ",
          "",
          names(model$estimate),
          fixed = TRUE
        )
      }

      # One-sample t-test
    } else {
      out <- data.frame(
        Parameter = model$data.name,
        Mean = model$estimate,
        mu = model$null.value,
        Difference = model$estimate - model$null.value,
        CI_low = model$conf.int[1],
        CI_high = model$conf.int[2],
        t = model$statistic,
        df_error = model$parameter,
        p = model$p.value,
        Method = model$method,
        stringsAsFactors = FALSE
      )
    }
  }

  attr(out, "htest_type") <- "ttest"
  out
}


# extract htest ztest ----------------------

#' @keywords internal
.extract_htest_ztest <- function(model, standardized_d = NULL, hedges_g = NULL) {
  # Differentiate between two-sample and one-sample z-tests
  if (startsWith(tolower(model$method), "two-sample")) {
    out <- data.frame(
      Parameter = model$data.name,
      Mean_Parameter1 = model$estimate[1],
      Mean_Parameter2 = model$estimate[2],
      Difference = model$estimate[1] - model$estimate[2],
      CI_low = model$conf.int[1],
      CI_high = model$conf.int[2],
      z = model$statistic,
      p = model$p.value,
      Method = model$method,
      stringsAsFactors = FALSE
    )
    attr(out, "mean_group_values") <- c("x", "y")
  } else {
    out <- data.frame(
      Parameter = model$data.name,
      Mean = model$estimate,
      mu = model$null.value,
      Difference = model$estimate - model$null.value,
      CI_low = model$conf.int[1],
      CI_high = model$conf.int[2],
      z = model$statistic,
      p = model$p.value,
      Method = model$method,
      stringsAsFactors = FALSE
    )
  }

  attr(out, "htest_type") <- "ztest"
  out
}


# extract htest oneway ----------------------

#' @keywords internal
.extract_htest_oneway <- function(model) {
  out <- data.frame(
    `F` = model$statistic, # nolint
    df = model$parameter[1],
    df_error = model$parameter[2],
    p = model$p.value,
    Method = model$method,
    stringsAsFactors = FALSE
  )

  attr(out, "htest_type") <- "onewaytest"
  out
}


# extract htest chi2 ----------------------

#' @keywords internal
.extract_htest_chi2 <- function(model) {
  # Handle design-based tests (svytable or svychisq)
  if (
    (any("observed" %in% names(model)) && inherits(model$observed, "svytable")) ||
      any(startsWith(model$data.name, "svychisq"))
  ) {
    if (grepl("Pearson's X", model$method, fixed = TRUE)) {
      model$method <- gsub(
        "(Pearson's X\\^2: )(.*)",
        "Pearson's Chi2 \\(\\2\\)",
        model$method
      )
    }

    # Check if the statistic is F or Chi2
    if (names(model$statistic) == "F") {
      out <- data.frame(
        `F` = model$statistic, # nolint
        df = model$parameter[1],
        df_error = model$parameter[2],
        p = model$p.value,
        Method = model$method,
        stringsAsFactors = FALSE
      )
    } else {
      out <- data.frame(
        Chi2 = model$statistic,
        df = model$parameter,
        p = model$p.value,
        Method = model$method,
        stringsAsFactors = FALSE
      )
    }

    # Check if model estimates an odds ratio (like fisher.test or similar exact tests)
  } else if (!is.null(model$estimate) && identical(names(model$estimate), "odds ratio")) {
    out <- data.frame(
      `Odds Ratio` = model$estimate,
      CI_low = model$conf.int[1],
      CI_high = model$conf.int[2],
      p = model$p.value,
      Method = model$method,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    # Standard Chi-squared test
  } else {
    # Create data as list - we might have NULL for some values, which we want
    # to fill with NA
    out <- list(
      Chi2 = model$statistic,
      df = model$parameter,
      p = model$p.value,
      Method = model$method
    )
    out <- data.frame(
      lapply(out, function(i) if (is.null(i)) NA else i),
      stringsAsFactors = FALSE
    )
  }

  attr(out, "htest_type") <- "chi2test"
  out
}


# extract htest prop ----------------------

#' @keywords internal
.extract_htest_prop <- function(model) {
  # We may have multiple estimates. If so, we want to keep each one in an own
  # column. Examples:
  # smokers <- c(83, 90, 129, 70)
  # patients <- c(86, 93, 136, 82)
  # prop.test(smokers, patients) |> parameters::model_parameters()
  out <- as.data.frame(do.call(cbind, as.list(model$estimate)))
  if (ncol(out) > 1) {
    colnames(out) <- paste("Estimate", seq_len(ncol(out)), sep = "_")
  } else {
    colnames(out) <- "Estimate"
  }

  # If we have exactly two estimates, we may be interested in their difference
  if (length(model$estimate) == 2) {
    out$Difference <- abs(model$estimate[1] - model$estimate[2])
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

  attr(out, "htest_type") <- "proptest"
  out
}


# extract htest binom ----------------------

#' @keywords internal
.extract_htest_binom <- function(model) {
  out <- data.frame(
    Probability = model$estimate,
    CI_low = model$conf.int[1],
    CI_high = model$conf.int[2],
    Success = model$statistic,
    Trials = model$parameter,
    stringsAsFactors = FALSE
  )
  out$Null_value <- model$null.value
  out$p <- model$p.value
  out$Method <- model$method

  attr(out, "htest_type") <- "binomtest"
  out
}


# ==== effectsizes =====

.add_effectsize_htest <- function(
  model,
  out,
  es_type = NULL,
  ci = 0.95,
  alternative = NULL,
  verbose = TRUE,
  ...
) {
  # Check if effect sizes are requested and the package is available
  if (!requireNamespace("effectsize", quietly = TRUE) || is.null(es_type)) {
    return(out)
  }

  # Return on invalid options. We may have partial matching with argument
  # `effects` for `es_type`, and thus all "effects" options should be ignored.
  if (es_type %in% c("fixed", "random", "all")) {
    return(out)
  }

  # Try to extract effectsize using the effectsize package
  es <- tryCatch(
    {
      effectsize::effectsize(
        model,
        type = es_type,
        ci = ci,
        alternative = alternative,
        verbose = verbose,
        ...
      )
    },
    error = function(e) {
      if (verbose) {
        msg <- c(
          paste0(
            "Could not compute effectsize ",
            effectsize::get_effectsize_label(es_type),
            "."
          ),
          paste0("Possible reason: ", e$message)
        )
        insight::format_alert(msg)
      }
      NULL
    }
  )

  # Return original output if effect size computation failed
  if (is.null(es)) {
    return(out)
  }

  ## TODO: check if effectsize prefixes are correct @mattansb

  # Find correct prefix for the CI-columns based on the effect size type
  prefix <- switch(
    es_type,
    cohens_g = "Cohens_",
    cramers_v = "Cramers_",
    phi = "phi_",
    cohens_d = "d_",
    hedges_g = "g_",
    rank_biserial = "rank_biserial_",
    rank_epsilon_squared = "rank_epsilon_squared_",
    kendalls_w = "W_",
    omega = "Omega2_",
    eta = "Eta2_",
    epsilon = "Epsilon2_"
  )

  # Rename CI columns from the effectsize package to include the prefix
  es$CI <- NULL
  ci_cols <- startsWith(names(es), "CI")
  es_ci_cols <- paste0(prefix, names(es)[ci_cols])
  names(es)[ci_cols] <- es_ci_cols

  # Combine parameters and effect sizes
  out <- cbind(out, es)

  # Compose effect size columns to be used in column reordering
  es_columns <- unique(c(effectsize::get_effectsize_name(colnames(es)), es_ci_cols))

  # Reorder columns to a consistent and logical layout
  col_order <- c(
    "Parameter1",
    "Parameter2",
    "Parameter",
    "F",
    "Chi2",
    "Group",
    "Mean_Parameter1",
    "Mean_Parameter2",
    "Mean_Group1",
    "Mean_Group2",
    "mu",
    "Difference",
    "W",
    "CI_low",
    "CI_high",
    es_columns,
    "t",
    "df",
    "df_error",
    "p",
    "Method",
    "method"
  )
  out <- out[col_order[col_order %in% names(out)]]
  out
}


# ==== add attributes ====

#' @keywords internal
.add_htest_parameters_attributes <- function(params, model, ci = 0.95, ...) {
  # Add metadata to parameters data frame
  attr(params, "title") <- unique(params$Method)
  attr(params, "model_class") <- class(model)
  attr(params, "alternative") <- model$alternative

  # Build a readable string for the alternative hypothesis
  if (!is.null(model$alternative)) {
    h1_text <- "Alternative hypothesis: "
    if (is.null(model$null.value)) {
      h1_text <- paste0(h1_text, model$alternative)
    } else if (length(model$null.value) == 1L) {
      alt.char <- switch(
        model$alternative,
        two.sided = "not equal to",
        less = "less than",
        greater = "greater than"
      )
      h1_text <- paste0(
        h1_text,
        "true ",
        names(model$null.value),
        " is ",
        alt.char,
        " ",
        model$null.value
      )
    } else {
      h1_text <- paste0(h1_text, model$alternative)
    }
    attr(params, "text_alternative") <- h1_text
  }

  # Extract dot-arguments to safely pass formatting options
  dot.arguments <- lapply(match.call(expand.dots = FALSE)[["..."]], function(x) x)

  # Set default formatting attributes if not provided
  if ("digits" %in% names(dot.arguments)) {
    attr(params, "digits") <- eval(dot.arguments[["digits"]])
  } else {
    attr(params, "digits") <- 2
  }

  if ("ci_digits" %in% names(dot.arguments)) {
    attr(params, "ci_digits") <- eval(dot.arguments[["ci_digits"]])
  } else {
    attr(params, "ci_digits") <- NULL
  }

  if ("p_digits" %in% names(dot.arguments)) {
    attr(params, "p_digits") <- eval(dot.arguments[["p_digits"]])
  } else {
    attr(params, "p_digits") <- 3
  }

  attr(params, "ci") <- ci
  attr(params, "ci_test") <- attributes(model$conf.int)$conf.level

  # Add CI column if requested and missing, then reorder to place it correctly
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
.add_htest_attributes <- function(params, model, p_adjust = NULL, verbose = TRUE, ...) {
  # Extract dot-arguments safely
  dot.arguments <- lapply(match.call(expand.dots = FALSE)[["..."]], function(x) x)

  # Attach basic attributes
  attr(params, "p_adjust") <- p_adjust
  attr(params, "model_class") <- class(model)
  attr(params, "title") <- params$Method

  # Set display properties
  if ("digits" %in% names(dot.arguments)) {
    attr(params, "digits") <- eval(dot.arguments[["digits"]])
  } else {
    attr(params, "digits") <- 2
  }

  if ("ci_digits" %in% names(dot.arguments)) {
    attr(params, "ci_digits") <- eval(dot.arguments[["ci_digits"]])
  } else {
    attr(params, "ci_digits") <- NULL
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

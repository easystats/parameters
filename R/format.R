# usual models ---------------------------------

#' @inheritParams print.parameters_model
#' @rdname print.parameters_model
#' @export
format.parameters_model <- function(
  x,
  pretty_names = TRUE,
  split_components = TRUE,
  select = NULL,
  digits = 2,
  ci_digits = digits,
  p_digits = 3,
  ci_width = NULL,
  ci_brackets = NULL,
  zap_small = FALSE,
  format = NULL,
  groups = NULL,
  include_reference = FALSE,
  ...
) {
  # 1. Retrieve Model Metadata -------------------------------------------------
  # Extract attributes that dictate how the model output should be structured
  coef_name <- attributes(x)$coefficient_name
  coef_name2 <- attributes(x)$coefficient_name2
  s_value <- attributes(x)$s_value
  m_class <- attributes(x)$model_class
  htest_type <- attributes(x)$htest_type
  mixed_model <- attributes(x)$mixed_model
  random_variances <- isTRUE(attributes(x)$ran_pars)
  dist_params <- isTRUE(attributes(x)$dpars)
  mean_group_values <- attributes(x)$mean_group_values

  # 2. Process Arguments -------------------------------------------------------
  # Handle 'select' argument parsing (glue-syntax implies a style preset)
  style <- NULL
  if (
    !is.null(select) &&
      length(select) == 1 &&
      is.character(select) &&
      (grepl("{", select, fixed = TRUE) || select %in% .style_shortcuts)
  ) {
    style <- select
    select <- NULL
  }

  # Fallback to grouped parameters from attributes if not explicitly provided
  if (is.null(groups) && !is.null(attributes(x)$coef_groups)) {
    groups <- attributes(x)$coef_groups
  }

  # 3. Model-Specific Column Clean-ups -----------------------------------------
  # Standardize random effect names for Bayesian packages
  if (
    isTRUE(random_variances) && any(c("brmsfit", "stanreg", "stanmvreg") %in% m_class)
  ) {
    x <- .format_stan_parameters(x, dist_params)
  }

  # HTML table rendering via 'gt' requires unified column headers across sub-tables.
  # We strip specific coefficient attributes to prevent header mismatches.
  if (identical(format, "html")) {
    coef_name <- NULL
    coef_name2 <- NULL
    attr(x, "coefficient_name") <- NULL
    attr(x, "coefficient_name2") <- NULL
    attr(x, "zi_coefficient_name") <- NULL
  }

  # Strip methodological meta-columns for hypothesis tests (moved to footer later)
  models_without_methods <- c(
    "BFBayesFactor",
    "htest",
    "rma",
    "t1way",
    "yuen",
    "PMCMR",
    "osrt",
    "trendPMCMR",
    "anova",
    "afex_aov"
  )

  if (!is.null(m_class) && any(m_class %in% models_without_methods)) {
    x$Method <- NULL
    x$Alternative <- NULL
  }

  # Remove specific columns for exotic model types to declutter output
  if (!is.null(m_class) && any(m_class == "mvord")) {
    x$Response <- NULL
  }

  if (!is.null(m_class) && any(m_class == "nestedLogit")) {
    x$Component <- NULL
    if (insight::has_single_value(x$Response, remove_na = TRUE)) {
      x$Response <- NULL
    }
  }

  if (!is.null(m_class) && any(m_class == "comparisons")) {
    x$Type <- NULL
  }

  # 4. Format Hypothesis Tests (t-tests, prop-tests) ---------------------------
  if (!is.null(htest_type)) {
    # Dynamically rename mean columns to show the actual grouping variable names
    if (
      htest_type == "ttest" &&
        !is.null(mean_group_values) &&
        all(c("Mean_Group1", "Mean_Group2") %in% colnames(x))
    ) {
      colnames(x)[which(colnames(x) == "Mean_Group1")] <- paste0(
        x$Group,
        " = ",
        mean_group_values[1]
      )
      colnames(x)[which(colnames(x) == "Mean_Group2")] <- paste0(
        x$Group,
        " = ",
        mean_group_values[2]
      )
    }

    # Condense estimates into a single 'Proportion' column for readability
    if (htest_type == "proptest") {
      est_cols <- which(startsWith(colnames(x), "Estimate"))
      if (length(est_cols) > 1) {
        x$Proportion <- paste(
          insight::format_value(x[est_cols], as_percent = TRUE),
          collapse = " / "
        )
      } else {
        x$Proportion <- insight::format_value(x[[est_cols]], as_percent = TRUE)
      }
      if (!is.null(x$Difference)) {
        x$Difference <- insight::format_value(x$Difference, as_percent = TRUE)
      }
      x[est_cols] <- NULL
      x <- x[c("Proportion", setdiff(colnames(x), "Proportion"))]
    }
  }

  # Sanitize '$' from parameter names as it breaks LaTeX/Markdown rendering
  if (!is.null(htest_type)) {
    if ("Parameter" %in% colnames(x) && grepl("$", x$Parameter, fixed = TRUE)) {
      x$Parameter <- gsub("(.*)\\$(.*)", "\\2", x$Parameter)
    }
    if ("Group" %in% colnames(x) && grepl("$", x$Group, fixed = TRUE)) {
      x$Group <- gsub("(.*)\\$(.*)", "\\2", x$Group)
    }
  }

  # 5. Core Data Formatting ----------------------------------------------------
  # Merge group column into parameter column for random effects
  if (isTRUE(random_variances)) {
    x <- .format_ranef_parameters(x)
  }

  # Drop empty columns and subset based on user 'select' preferences
  x <- .prepare_x_for_print(x, select, coef_name, s_value)

  # Determine if we need sub-tables (e.g., fixed vs random, count vs zero-inflation)
  split_by <- .prepare_splitby_for_print(x)

  if (split_components && !is.null(split_by) && length(split_by)) {
    # Format and create captions for multiple model components
    formatted_table <- .format_columns_multiple_components(
      x,
      pretty_names,
      split_column = split_by,
      digits = digits,
      ci_digits = ci_digits,
      p_digits = p_digits,
      coef_column = coef_name,
      format = format,
      ci_width = ci_width,
      ci_brackets = ci_brackets,
      zap_small = zap_small,
      include_reference = include_reference,
      style = style,
      ...
    )
  } else {
    # Standard formatting for a single flat table
    formatted_table <- .format_columns_single_component(
      x,
      pretty_names = pretty_names,
      digits = digits,
      ci_width = ci_width,
      ci_brackets = ci_brackets,
      ci_digits = ci_digits,
      p_digits = p_digits,
      format = format,
      coef_name = coef_name,
      zap_small = zap_small,
      include_reference = include_reference,
      style = style,
      ...
    )
  }

  # 6. Final Layout Adjustments ------------------------------------------------
  # Strip columns that carry no differentiating information
  if (insight::has_single_value(formatted_table$Component, remove_na = TRUE)) {
    formatted_table$Component <- NULL
  }
  if (insight::has_single_value(formatted_table$Effects, remove_na = TRUE)) {
    formatted_table$Effects <- NULL
  }
  if (
    insight::has_single_value(formatted_table$Group, remove_na = TRUE) &&
      isTRUE(mixed_model)
  ) {
    formatted_table$Group <- NULL
  }
  if (
    !is.null(formatted_table$CI) &&
      insight::has_single_value(formatted_table$CI, remove_na = TRUE)
  ) {
    formatted_table$CI <- NULL
  }

  # Attach grouping info for indented printing downstream
  attr(formatted_table, "indent_rows") <- groups

  # Transpose table if vertical layout is requested
  if (identical(list(...)$layout, "vertical")) {
    if ("Parameter" %in% colnames(formatted_table)) {
      new_colnames <- c("", formatted_table$Parameter)
      formatted_table$Parameter <- NULL
    } else {
      new_colnames <- c("Type", paste0("Value ", seq_len(nrow(formatted_table))))
    }
    formatted_table <- datawizard::rownames_as_column(
      as.data.frame(t(formatted_table)),
      "Type"
    )
    colnames(formatted_table) <- new_colnames
  }

  formatted_table
}

#' @export
format.parameters_simulate <- format.parameters_model

#' @export
format.parameters_brms_meta <- format.parameters_model

#' @export
format.parameters_coef <- function(x, format = NULL, ...) {
  insight::format_table(x, format = format, ...)
}


# Compare parameters ----------------------

#' @rdname print.compare_parameters
#' @inheritParams print.parameters_model
#' @export
format.compare_parameters <- function(
  x,
  split_components = TRUE,
  select = NULL,
  digits = 2,
  ci_digits = digits,
  p_digits = 3,
  ci_width = NULL,
  ci_brackets = NULL,
  zap_small = FALSE,
  format = NULL,
  groups = NULL,
  ...
) {
  m_class <- attributes(x)$model_class
  x$Method <- NULL

  if (!is.null(m_class) && any(m_class == "mvord")) {
    x$Response <- NULL
  }

  # Initialize the scaffold for the merged output
  out <- data.frame(
    Parameter = x$Parameter,
    Effects = x$Effects,
    Component = x$Component,
    stringsAsFactors = FALSE
  )

  # Strip component suffix from parameter names if we are splitting by components anyway
  if (isTRUE(split_components)) {
    out$Parameter <- insight::trim_ws(gsub(" (zi)", "", out$Parameter, fixed = TRUE))
    out$Effects <- NULL
  }

  models <- attributes(x)$model_names
  parameters_attributes <- attributes(x)$all_attributes

  if (is.null(groups) && !is.null(parameters_attributes[[1]]$coef_groups)) {
    groups <- parameters_attributes[[1]]$coef_groups
  }

  # 1. Merge Strategy: Process Each Model Sequentially -------------------------
  ran_pars <- which(x$Effects == "random")

  if (is.null(x$Group)) {
    ran_groups <- NULL
    ran_group_rows <- NULL
  } else {
    ran_groups <- unique(insight::compact_character(x$Group))
    ran_group_rows <- which(nzchar(x$Group, keepNA = TRUE))
  }

  for (i in models) {
    # Isolate columns belonging to the current model using regex matching on the suffix
    pattern <- paste0("\\.\\Q", i, "\\E$")
    cols <- x[grepl(pattern, colnames(x))]
    colnames(cols) <- gsub(pattern, "", colnames(cols))

    coef_column <- which(colnames(cols) %in% c(.all_coefficient_types, "Coefficient"))
    valid_rows <- which(!is.na(cols[[coef_column]]))

    ran_pars_rows <- NULL
    if (
      length(ran_pars) && length(ran_group_rows) && any(ran_group_rows %in% valid_rows)
    ) {
      # Filter to ensure we only apply formatting to rows valid for THIS model
      ran_pars_rows <- intersect(valid_rows, intersect(ran_pars, ran_group_rows))
    }

    # Prettify Random Effects labels (SD and Cor) by appending the group name
    if (!is.null(ran_pars_rows) && length(ran_pars_rows)) {
      stddevs <- startsWith(out$Parameter[ran_pars_rows], "SD (")
      fixed_name <- unlist(lapply(
        ran_groups,
        grep,
        x = out$Parameter[ran_pars_rows[stddevs]],
        fixed = TRUE
      ))
      if (length(fixed_name)) {
        stddevs[fixed_name] <- FALSE
      }

      if (length(stddevs)) {
        out$Parameter[ran_pars_rows[stddevs]] <- paste0(
          gsub("(.*)\\)", "\\1", out$Parameter[ran_pars_rows[stddevs]]),
          ": ",
          x$Group[ran_pars_rows[stddevs]],
          ")"
        )
      }

      corrs <- startsWith(out$Parameter[ran_pars_rows], "Cor (")
      fixed_name <- unlist(lapply(
        ran_groups,
        grep,
        x = out$Parameter[ran_pars_rows[corrs]],
        fixed = TRUE
      ))
      if (length(fixed_name)) {
        corrs[fixed_name] <- FALSE
      }

      if (length(corrs)) {
        out$Parameter[ran_pars_rows[corrs]] <- paste0(
          gsub("(.*)\\)", "\\1", out$Parameter[ran_pars_rows[corrs]]),
          ": ",
          x$Group[ran_pars_rows[corrs]],
          ")"
        )
      }
      out$Parameter[out$Parameter == "SD (Observations: Residual)"] <- "SD (Residual)"
    }

    # Apply standard numerical formatting
    attributes(cols)$coef_name <- colnames(cols)[coef_column]
    cols <- insight::format_table(
      cols,
      digits = digits,
      ci_width = ci_width,
      ci_brackets = ci_brackets,
      ci_digits = ci_digits,
      p_digits = p_digits,
      zap_small = zap_small,
      select = select,
      ...
    )

    # Append the model name back to the column headers to distinguish them
    if (ncol(cols) > 1) {
      colnames(cols) <- paste0(colnames(cols), " (", i, ")")
    } else {
      colnames(cols) <- i
    }

    out <- cbind(out, cols)
  }

  # 2. Final Output Sorting and Splitting --------------------------------------
  out$Group <- NULL
  x$Group <- NULL

  if (isFALSE(split_components)) {
    out <- datawizard::data_arrange(out, c("Effects", "Component"))
  }

  split_by <- split_column <- .prepare_splitby_for_print(x)

  # Break table into chunks based on components (e.g. conditional vs zero-inflated)
  if (length(split_by) > 0L && isTRUE(split_components)) {
    if (length(split_column) > 1L) {
      split_by <- lapply(split_column, function(i) x[[i]])
    } else {
      split_by <- list(x[[split_column]])
    }
    names(split_by) <- split_column

    formatted_table <- split(out, f = split_by)
    formatted_table <- lapply(names(formatted_table), function(tab) {
      i <- formatted_table[[tab]]
      if (nrow(i) == 0L) {
        return(NULL)
      }

      if (insight::has_single_value(i$Component, remove_na = TRUE)) {
        i$Component <- NULL
      }
      if (insight::has_single_value(i$Effects, remove_na = TRUE)) {
        i$Effects <- NULL
      }

      # Generate descriptive headers for each sub-table
      table_caption <- .format_model_component_header(
        x,
        type = tab,
        split_column = tab,
        is_zero_inflated = FALSE,
        is_ordinal_model = FALSE,
        is_multivariate = FALSE,
        ran_pars = FALSE,
        formatted_table = i
      )

      # Attach captions based on the desired output format
      if (identical(format, "html")) {
        i$Component <- table_caption$name
      } else if (identical(format, "md") || identical(format, "markdown")) {
        attr(i, "table_caption") <- table_caption$name
      } else {
        attr(i, "table_caption") <- c(paste("#", table_caption$name), "blue")
      }
      i
    })

    formatted_table <- insight::compact_list(formatted_table)

    if (identical(format, "html")) {
      formatted_table <- do.call(rbind, .fix_nonmatching_columns(formatted_table))
    }
  } else {
    formatted_table <- out
    if (insight::has_single_value(formatted_table$Component, remove_na = TRUE)) {
      formatted_table$Component <- NULL
    }
    if (insight::has_single_value(formatted_table$Effects, remove_na = TRUE)) {
      formatted_table$Effects <- NULL
    }

    formatted_table <- .add_obs_row(
      formatted_table,
      parameters_attributes,
      style = select
    )
  }

  attr(formatted_table, "indent_rows") <- groups
  formatted_table
}

# sem-models ---------------------------------

#' @export
format.parameters_sem <- function(
  x,
  digits = 2,
  ci_digits = digits,
  p_digits = 3,
  format = NULL,
  ci_width = NULL,
  ci_brackets = TRUE,
  pretty_names = TRUE,
  ...
) {
  # Fallbacks based on object attributes
  if (missing(digits)) {
    digits <- .additional_arguments(x, "digits", 2)
  }
  if (missing(ci_digits)) {
    ci_digits <- .additional_arguments(x, "ci_digits", digits)
  }
  if (missing(p_digits)) {
    p_digits <- .additional_arguments(x, "p_digits", 3)
  }

  .format_columns_multiple_components(
    x,
    pretty_names = TRUE,
    split_column = "Component",
    digits = digits,
    ci_digits = ci_digits,
    p_digits = p_digits,
    format = format,
    ci_width = ci_width,
    ci_brackets = ci_brackets,
    ...
  )
}


# footer functions ------------------

.format_footer <- function(
  x,
  digits = 3,
  verbose = TRUE,
  show_sigma = FALSE,
  show_formula = FALSE,
  show_r2 = FALSE,
  show_rmse = FALSE,
  format = "text"
) {
  footer <- NULL
  type <- tolower(format)

  # Fetch parameters needed for footer calculations
  sigma_value <- attributes(x)$sigma
  r2 <- attributes(x)$r2
  rmse <- attributes(x)$rmse
  residual_df <- attributes(x)$residual_df
  p_adjust <- attributes(x)$p_adjust
  model_formula <- attributes(x)$model_formula
  anova_test <- attributes(x)$anova_test
  anova_type <- attributes(x)$anova_type
  prediction_type <- attributes(x)$prediction_type
  footer_text <- attributes(x)$footer_text
  text_alternative <- attributes(x)$text_alternative
  n_obs <- attributes(x)$n_obs

  # Sequentially build footer text
  if (isTRUE(show_formula)) {
    footer <- .add_footer_formula(footer, model_formula, n_obs, type)
  }
  if (isTRUE(show_sigma)) {
    footer <- .add_footer_sigma(footer, digits, sigma_value, residual_df, type)
  }
  if (isTRUE(show_rmse)) {
    footer <- .add_footer_values(footer, digits, value = rmse, text = "RMSE ", type)
  }
  if (isTRUE(show_r2)) {
    footer <- .add_footer_r2(footer, digits, r2, type)
  }

  if (
    "p" %in% colnames(x) && isTRUE(verbose) && !is.null(p_adjust) && p_adjust != "none"
  ) {
    footer <- .add_footer_text(
      footer,
      text = paste("p-value adjustment method:", format_p_adjust(p_adjust))
    )
  }

  if (!is.null(anova_test)) {
    footer <- .add_footer_text(footer, text = sprintf("%s test statistic", anova_test))
  }
  if (!is.null(anova_type)) {
    footer <- .add_footer_text(
      footer,
      text = sprintf("Anova Table (Type %s tests)", anova_type)
    )
  }
  if (!is.null(prediction_type)) {
    footer <- .add_footer_text(
      footer,
      text = sprintf("Prediction type: %s", prediction_type)
    )
  }
  if (!is.null(text_alternative)) {
    footer <- .add_footer_text(footer, text = text_alternative)
  }
  if (!is.null(footer_text)) {
    footer <- .add_footer_text(footer, footer_text, type)
  }

  # Cleanup trailing newlines for text outputs
  if (identical(type, "text") && !is.null(footer) && endsWith(footer[1], "\n\n")) {
    footer[1] <- substr(footer[1], 0, nchar(x) - 1)
  }

  footer
}


# core helper to generate the footer string
.append_footer_string <- function(footer, string, type = "text") {
  if (is.null(string) || length(string) == 0) {
    return(footer)
  }

  if (type %in% c("text", "markdown")) {
    # for text/markdown: add initial line break if footer is empty
    fill <- if (is.null(footer)) "\n" else ""
    footer <- paste0(footer, fill, string, "\n")
  } else if (type == "html") {
    # for HTML: remove line breaks
    clean_string <- insight::trim_ws(gsub("\n", "", string, fixed = TRUE))
    footer <- c(footer, clean_string)
  }

  footer
}


# footer: generic text
.add_footer_text <- function(footer = NULL, text = NULL, type = "text") {
  if (is.null(text) || length(text) == 0) {
    return(footer)
  }

  .append_footer_string(footer, text, type)
}


# footer: generic values
.add_footer_values <- function(
  footer = NULL,
  digits = 3,
  value = NULL,
  text = NULL,
  type = "text"
) {
  if (is.null(value) || is.null(text)) {
    return(footer)
  }

  string <- sprintf("%s: %s", text, insight::format_value(value, digits = digits))
  .append_footer_string(footer, string, type)
}


# footer: residual standard deviation
.add_footer_sigma <- function(
  footer = NULL,
  digits = 3,
  sigma = NULL,
  residual_df = NULL,
  type = "text"
) {
  if (is.null(sigma)) {
    return(footer)
  }

  res_df <- if (is.null(residual_df)) "" else paste0(" (df = ", residual_df, ")")
  string <- sprintf("Sigma: %.*f%s", digits, sigma, res_df)

  .append_footer_string(footer, string, type)
}


# footer: r-squared
.add_footer_r2 <- function(footer = NULL, digits = 3, r2 = NULL, type = "text") {
  if (is.null(r2)) {
    return(footer)
  }

  rsq <- .safe(paste(
    unlist(lapply(r2, function(i) {
      paste0(attributes(i)$names, ": ", insight::format_value(i, digits = digits))
    })),
    collapse = "; "
  ))

  if (is.null(rsq)) {
    return(footer)
  }

  .append_footer_string(footer, rsq, type)
}


# footer: model formula
.add_footer_formula <- function(
  footer = NULL,
  model_formula = NULL,
  n_obs = NULL,
  type = "text"
) {
  if (is.null(model_formula)) {
    return(footer)
  }

  n <- if (is.null(n_obs)) "" else paste0(" (", n_obs, " Observations)")
  string <- paste0("Model: ", model_formula, n)

  .append_footer_string(footer, string, type)
}


# footer: type of uncertainty interval
.print_footer_cimethod <- function(x) {
  if (!isTRUE(getOption("parameters_cimethod", TRUE))) {
    return()
  }

  ci_method <- .additional_arguments(x, "ci_method", NULL)
  if (is.null(ci_method)) {
    return()
  }

  test_statistic <- .additional_arguments(x, "test_statistic", NULL)
  bootstrap <- .additional_arguments(x, "bootstrap", FALSE)
  is_bayesian <- .additional_arguments(x, "is_bayesian", FALSE)
  simulated <- .additional_arguments(x, "simulated", FALSE)
  residual_df <- .additional_arguments(x, "residual_df", NULL)
  random_variances <- .additional_arguments(x, "ran_pars", FALSE)
  model_class <- .additional_arguments(x, "model_class", NULL)

  ci_method <- tolower(ci_method)

  # only random effects? no message for fixed effects ci-approximation
  if (!is.null(x$Effects) && all(x$Effects == "random")) {
    msg <- "\n"
    string_method <- ""

    # here we have fixed effects only, or fixed and random effects
  } else {
    is_test_statistic_t <- ci_method == "residual" &&
      test_statistic == "z-statistic" &&
      !is.null(residual_df) &&
      !is.infinite(residual_df) &&
      !is.na(residual_df)

    if (is_test_statistic_t) {
      test_statistic <- "t-statistic"
    }

    string_tailed <- switch(
      ci_method,
      hdi = "highest-density",
      uniroot = ,
      profile = "profile-likelihood",
      "equal-tailed"
    )

    if (isTRUE(bootstrap)) {
      sampling_method <- ifelse(
        isTRUE(.unicode_symbols()),
        "na\u0131ve bootstrap",
        "naive bootstrap"
      )
    } else if (isTRUE(simulated)) {
      sampling_method <- "simulated multivariate normal"
    } else {
      sampling_method <- "MCMC"
    }

    string_method <- switch(
      ci_method,
      bci = ,
      bcai = "bias-corrected accelerated bootstrap",
      si = ,
      ci = ,
      quantile = ,
      eti = ,
      hdi = sampling_method,
      normal = "Wald normal",
      boot = "parametric bootstrap",
      "Wald"
    )

    if (
      toupper(ci_method) %in%
        c("KENWARD", "KR", "KENWARD-ROGER", "KENWARD-ROGERS", "SATTERTHWAITE")
    ) {
      string_approx <- paste0(
        "with ",
        format_df_adjust(ci_method, approx_string = "", dof_string = ""),
        " "
      )
    } else {
      string_approx <- ""
    }

    if (!is.null(test_statistic) && ci_method != "normal" && !isTRUE(bootstrap)) {
      string_statistic <- switch(
        tolower(test_statistic),
        `t-statistic` = "t",
        `chi-squared statistic` = ,
        `z-statistic` = "z",
        ""
      )
      string_method <- paste0(string_method, " ", string_statistic, "-")
    } else {
      string_method <- paste0(string_method, " ")
    }

    if (isTRUE(bootstrap)) {
      msg <- paste0(
        "\nUncertainty intervals (",
        string_tailed,
        ") are ",
        string_method,
        "intervals."
      )
    } else if (isTRUE(is_bayesian)) {
      msg <- paste0(
        "\nUncertainty intervals (",
        string_tailed,
        ") computed using a ",
        string_method,
        "distribution ",
        string_approx,
        "approximation."
      )
    } else {
      msg <- paste0(
        "\nUncertainty intervals (",
        string_tailed,
        ") and p-values (two-tailed) computed using a ",
        string_method,
        "distribution ",
        string_approx,
        "approximation."
      )
    }
  }

  # Append warning for random effect variances from specific packages
  show_re_msg <- (identical(model_class, "glmmTMB") &&
    (!string_method %in% c("Wald z-", "Wald normal") ||
      !ci_method %in% c("wald", "normal"))) ||
    ((identical(model_class, "lmerMod") || identical(model_class, "glmerMod")) &&
      !ci_method %in% c("wald", "normal", "profile", "boot"))

  if (
    show_re_msg &&
      isTRUE(random_variances) &&
      !is.null(x$Effects) &&
      "random" %in% x$Effects
  ) {
    msg <- paste(
      msg,
      "Uncertainty intervals for random effect variances computed using a Wald z-distribution approximation."
    )
  }

  insight::format_alert(insight::color_text(msg, "yellow"))
}


.print_footer_exp <- function(x) {
  if (!isTRUE(getOption("parameters_exponentiate", TRUE))) {
    return()
  }

  msg <- NULL
  coef_column <- intersect(colnames(x), .all_coefficient_names)

  if (length(coef_column) && "Parameter" %in% colnames(x)) {
    spurious_coefficients <- abs(x[[coef_column[1]]][!.in_intercepts(x$Parameter)])
  } else {
    spurious_coefficients <- NULL
  }

  exponentiate <- .additional_arguments(x, "exponentiate", FALSE)

  # Check if model should be exponentiated but user didn't request it
  if (!.is_valid_exponentiate_argument(exponentiate)) {
    if (isTRUE(.additional_arguments(x, "log_link", FALSE))) {
      msg <- "The model has a log- or logit-link. Consider using `exponentiate = TRUE` to interpret coefficients as ratios."
      if (!is.null(spurious_coefficients)) {
        spurious_coefficients <- exp(spurious_coefficients)
      }
    } else if (isTRUE(.additional_arguments(x, "log_response", FALSE))) {
      msg <- "The model has a log-transformed response variable. Consider using `exponentiate = TRUE` to interpret coefficients as ratios."
      spurious_coefficients <- NULL
    }
  } else if (
    .is_valid_exponentiate_argument(exponentiate) &&
      isTRUE(.additional_arguments(x, "log_response", FALSE))
  ) {
    spurious_coefficients <- NULL
  }

  # Test for complete separation (extremely large estimates) in logit models
  msg <- c(msg, .check_complete_separation(x, spurious_coefficients))

  if (!is.null(msg) && isTRUE(getOption("parameters_warning_exponentiate", TRUE))) {
    insight::format_alert(paste0("\n", msg))
    options(parameters_warning_exponentiate = FALSE)
  }
}


.check_complete_separation <- function(x, coefs = NULL) {
  is_logit <- isTRUE(.additional_arguments(x, "logit_link", FALSE)) ||
    isTRUE(attributes(x)$coefficient_name %in% c("Log-Odds", "Odds Ratio"))

  if (!is.null(coefs)) {
    coefs <- coefs[!is.na(coefs) & !is.infinite(coefs)]
  }
  if (!is_logit || is.null(coefs) || length(coefs) == 0) {
    return(NULL)
  }
  if (any(coefs > 50)) {
    return(
      "Some coefficients are very large, which may indicate issues with complete separation."
    )
  }
  if (any(coefs > 15)) {
    return(
      "Some coefficients seem to be rather large, which may indicate issues with (quasi) complete separation. Consider using bias-corrected or penalized regression models."
    )
  }
  return(NULL)
}

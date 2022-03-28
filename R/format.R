# usual models ---------------------------------

#' @inheritParams print.parameters_model
#' @rdname display.parameters_model
#' @export
format.parameters_model <- function(x,
                                    pretty_names = TRUE,
                                    split_components = TRUE,
                                    select = NULL,
                                    digits = 2,
                                    ci_digits = 2,
                                    p_digits = 3,
                                    ci_width = NULL,
                                    ci_brackets = NULL,
                                    zap_small = FALSE,
                                    format = NULL,
                                    groups = NULL,
                                    ...) {
  # save attributes
  coef_name <- attributes(x)$coefficient_name
  coef_name2 <- attributes(x)$coefficient_name2
  s_value <- attributes(x)$s_value
  m_class <- attributes(x)$model_class
  htest_type <- attributes(x)$htest_type
  mixed_model <- attributes(x)$mixed_model
  random_variances <- isTRUE(attributes(x)$ran_pars)
  mean_group_values <- attributes(x)$mean_group_values

  # is information about grouped parameters stored as attribute?
  if (is.null(groups) && !is.null(attributes(x)$coef_groups)) {
    groups <- attributes(x)$coef_groups
  }

  if (identical(format, "html")) {
    coef_name <- NULL
    coef_name2 <- NULL
    attr(x, "coefficient_name") <- NULL
    attr(x, "coefficient_name2") <- NULL
    attr(x, "zi_coefficient_name") <- NULL
  }

  # remove method for htest
  if (!is.null(m_class) && any(m_class %in% c("BFBayesFactor", "htest", "rma", "t1way", "yuen", "PMCMR", "osrt", "trendPMCMR", "anova", "afex_aov"))) {
    x$Method <- NULL
    x$Alternative <- NULL
  }

  # remove response for mvord
  if (!is.null(m_class) && any(m_class == "mvord")) {
    x$Response <- NULL
  }

  # rename columns for t-tests
  if (!is.null(htest_type) && htest_type == "ttest" && !is.null(mean_group_values)) {
    if (all(c("Mean_Group1", "Mean_Group2") %in% colnames(x))) {
      colnames(x)[which(colnames(x) == "Mean_Group1")] <- paste0(x$Group, " = ", mean_group_values[1])
      colnames(x)[which(colnames(x) == "Mean_Group2")] <- paste0(x$Group, " = ", mean_group_values[2])
    }
  }

  # Special print for mcp from WRS2
  if (!is.null(m_class) && any(m_class %in% c("mcp1", "mcp2"))) {
    x$Group1 <- paste(x$Group1, x$Group2, sep = " vs. ")
    x$Group2 <- NULL
    colnames(x)[1] <- "Group"
  }

  # check if we have mixed models with random variance parameters
  # in such cases, we don't need the group-column, but we rather
  # merge it with the parameter column
  if (isTRUE(random_variances)) {
    if (!is.null(x$Group) && !is.null(x$Effects)) {
      ran_pars <- which(x$Effects == "random")
      stddevs <- grepl("^SD \\(", x$Parameter[ran_pars])
      x$Parameter[ran_pars[stddevs]] <- paste0(gsub("(.*)\\)", "\\1", x$Parameter[ran_pars[stddevs]]), ": ", x$Group[ran_pars[stddevs]], ")")
      x$Parameter[x$Parameter == "SD (Observations: Residual)"] <- "SD (Residual)"
      x$Group <- NULL
    }
  }

  # group parameters
  if (!is.null(groups)) {
    x <- .parameter_groups(x, groups)
  }
  indent_groups <- attributes(x)$indent_groups
  indent_rows <- attributes(x)$indent_rows

  # prepare output, to have in shape for printing
  x <- .prepare_x_for_print(x, select, coef_name, s_value)

  # check whether to split table by certain factors/columns (like component, response...)
  split_by <- .prepare_splitby_for_print(x)

  # print everything now...
  if (split_components && !is.null(split_by) && length(split_by)) {
    formatted_table <- .format_columns_multiple_components(x, pretty_names, split_column = split_by, digits = digits, ci_digits = ci_digits, p_digits = p_digits, coef_column = coef_name, format = format, ci_width = ci_width, ci_brackets = ci_brackets, zap_small = zap_small, ...)
  } else {
    formatted_table <- .format_columns_single_component(x, pretty_names = pretty_names, digits = digits, ci_width = ci_width, ci_brackets = ci_brackets, ci_digits = ci_digits, p_digits = p_digits, format = format, coef_name = coef_name, zap_small = zap_small, ...)
  }

  # remove unique columns
  if (insight::n_unique(formatted_table$Component) == 1) formatted_table$Component <- NULL
  if (insight::n_unique(formatted_table$Effects) == 1) formatted_table$Effects <- NULL
  if (insight::n_unique(formatted_table$Group) == 1 && isTRUE(mixed_model)) formatted_table$Group <- NULL

  # no column with CI-level in output
  if (!is.null(formatted_table$CI) && insight::n_unique(formatted_table$CI) == 1) {
    formatted_table$CI <- NULL
  }

  if (!is.null(indent_rows)) {
    attr(formatted_table, "indent_rows") <- indent_rows
    attr(formatted_table, "indent_groups") <- NULL
  } else if (!is.null(indent_groups)) {
    attr(formatted_table, "indent_groups") <- indent_groups
  }

  formatted_table
}

#' @export
format.parameters_simulate <- format.parameters_model

#' @export
format.parameters_brms_meta <- format.parameters_model





# Compare parameters ----------------------


#' @inheritParams print.parameters_model
#' @export
format.compare_parameters <- function(x,
                                      style = NULL,
                                      split_components = TRUE,
                                      digits = 2,
                                      ci_digits = 2,
                                      p_digits = 3,
                                      ci_width = NULL,
                                      ci_brackets = NULL,
                                      zap_small = FALSE,
                                      format = NULL,
                                      groups = NULL,
                                      ...) {
  m_class <- attributes(x)$model_class
  x$Method <- NULL

  # remove response for mvord
  if (!is.null(m_class) && any(m_class == "mvord")) {
    x$Response <- NULL
  }

  out <- data.frame(
    Parameter = x$Parameter,
    Component = x$Component,
    stringsAsFactors = FALSE
  )

  # save model names
  models <- attributes(x)$model_names

  # save model parameters attributes
  parameters_attributes <- attributes(x)$all_attributes

  # is information about grouped parameters stored as attribute?
  if (is.null(groups) && !is.null(parameters_attributes[[1]]$coef_groups)) {
    groups <- parameters_attributes[[1]]$coef_groups
  }

  for (i in models) {
    # each column is suffixed with ".model_name", so we extract
    # columns for each model separately here
    pattern <- paste0("\\.", i, "$")
    cols <- x[grepl(pattern, colnames(x))]
    # since we now have the columns for a single model, we clean the
    # column names (i.e. remove suffix), so we can use "format_table" function
    colnames(cols) <- gsub(pattern, "", colnames(cols))
    # save p-stars in extra column
    cols$p_stars <- insight::format_p(cols$p, stars = TRUE, stars_only = TRUE)
    cols <- insight::format_table(cols, digits = digits, ci_width = ci_width, ci_brackets = ci_brackets, ci_digits = ci_digits, p_digits = p_digits, zap_small = zap_small)
    out <- cbind(out, .format_output_style(cols, style, format, i))
  }

  # group parameters
  if (!is.null(groups)) {
    out <- .parameter_groups(out, groups)
  }
  indent_groups <- attributes(x)$indent_groups
  indent_rows <- attributes(x)$indent_rows


  # check whether to split table by certain factors/columns (like component, response...)
  split_by <- split_column <- .prepare_splitby_for_print(x)

  if (length(split_by) > 0) {
    # set up split-factor
    if (length(split_column) > 1) {
      split_by <- lapply(split_column, function(i) x[[i]])
    } else {
      split_by <- list(x[[split_column]])
    }
    names(split_by) <- split_column

    # make sure we have correct sorting here...
    formatted_table <- split(out, f = split_by)
    formatted_table <- lapply(formatted_table, function(i) {
      # remove unique columns
      if (insight::n_unique(i$Component) == 1) i$Component <- NULL
      if (insight::n_unique(i$Effects) == 1) i$Effects <- NULL
      i
    })
  } else {
    formatted_table <- out
    # remove unique columns
    if (insight::n_unique(formatted_table$Component) == 1) formatted_table$Component <- NULL
    if (insight::n_unique(formatted_table$Effects) == 1) formatted_table$Effects <- NULL
    # add line with info about observations
    formatted_table <- .add_obs_row(formatted_table, parameters_attributes, style)
  }

  formatted_table
}



# stan models ----------------------------

#' @export
format.parameters_stan <- function(x,
                                   split_components = TRUE,
                                   select = NULL,
                                   digits = 2,
                                   ci_digits = 2,
                                   p_digits = 3,
                                   ci_width = NULL,
                                   ci_brackets = NULL,
                                   zap_small = FALSE,
                                   format = NULL,
                                   table_caption = NULL,
                                   ...) {
  cp <- attributes(x)$parameter_info
  att <- attributes(x)
  final_table <- list()

  if (!split_components || is.null(cp)) {
    NextMethod()
  } else {
    if (!is.null(select)) {
      if (all(select == "minimal")) {
        select <- c("Parameter", "Coefficient", "Median", "Mean", "CI", "CI_low", "CI_high", "pd")
      } else if (all(select == "short")) {
        select <- c("Parameter", "Coefficient", "Median", "Mean", "MAD", "SD", "pd")
      } else {
        if (is.numeric(select)) select <- colnames(x)[select]
      }
      select <- union(select, c("Parameter", "Component", "Effects", "Response", "Subgroup"))
      to_remove <- setdiff(colnames(x), select)
      x[to_remove] <- NULL
    }

    # restore group labels for print - this is a little hack. we want to have
    # the columns "Group" and "Level" for random effect groups for Stan models
    # when calling "as.data.frame()" on the returned object from "model_parameters()"
    # However, to make printing work, the "Group" column in "cp" must be formatted
    # in the way as returned by "insight::clean_parameters()". Since the latter
    # function uses a different pattern for the values in "Group" than what we
    # need for "as.data.frame()", we need to fix this here. This is currently
    # easier than changing the behaviour of "insight::clean_parameters()".
    x$Group <- x$Grouplabel

    # remove redundant columns
    x$Level <- NULL
    x$Grouplabel <- NULL

    out <- insight::print_parameters(cp, x, keep_parameter_column = FALSE, format = format)

    final_table <- lapply(out, function(i) {
      if (identical(format, "markdown")) {
        attr(i, "table_caption") <- attributes(i)$main_title
      }
      attributes(i) <- utils::modifyList(att, attributes(i))
      param_table <- insight::format_table(
        i,
        ci_width = ci_width,
        ci_brackets = ci_brackets,
        zap_small = zap_small,
        digits = digits,
        ci_digits = ci_digits,
        p_digits = p_digits,
        preserve_attributes = TRUE
      )
      param_table$Group <- NULL
      param_table$Response <- NULL
      param_table$Function <- NULL
      param_table
    })
  }

  final_table <- datawizard::compact_list(final_table)

  # modify table title, if requested
  if (length(final_table) == 1 && !is.null(table_caption)) {
    attr(final_table[[1]], "table_caption") <- table_caption
  } else if (length(final_table) == 1 && attr(final_table[[1]], "table_caption")[1] == "# Fixed effects") {
    attr(final_table[[1]], "table_caption") <- ""
  }

  final_table
}




# sem-models ---------------------------------

#' @export
format.parameters_sem <- function(x,
                                  digits = 2,
                                  ci_digits = 2,
                                  p_digits = 3,
                                  format = NULL,
                                  ci_width = NULL,
                                  ci_brackets = TRUE,
                                  pretty_names = TRUE,
                                  ...) {
  if (missing(digits)) digits <- .additional_arguments(x, "digits", 2)
  if (missing(ci_digits)) ci_digits <- .additional_arguments(x, "ci_digits", 2)
  if (missing(p_digits)) p_digits <- .additional_arguments(x, "p_digits", 3)
  .format_columns_multiple_components(x, pretty_names = TRUE, split_column = "Component", digits = digits, ci_digits = ci_digits, p_digits = p_digits, format = format, ci_width = ci_width, ci_brackets = ci_brackets, ...)
}



# footer functions ------------------

.format_footer <- function(x,
                           digits = 3,
                           verbose = TRUE,
                           show_sigma = FALSE,
                           show_formula = FALSE,
                           show_r2 = FALSE,
                           format = "text") {
  # prepare footer
  footer <- NULL
  type <- tolower(format)

  sigma <- attributes(x)$sigma
  r2 <- attributes(x)$r2
  residual_df <- attributes(x)$residual_df
  p_adjust <- attributes(x)$p_adjust
  model_formula <- attributes(x)$model_formula
  anova_test <- attributes(x)$anova_test
  anova_type <- attributes(x)$anova_type
  footer_text <- attributes(x)$footer_text
  text_alternative <- attributes(x)$text_alternative
  n_obs <- attributes(x)$n_obs

  # footer: model formula
  if (isTRUE(show_formula)) {
    footer <- .add_footer_formula(footer, model_formula, n_obs, type)
  }

  # footer: residual standard deviation
  if (isTRUE(show_sigma)) {
    footer <- .add_footer_sigma(footer, digits, sigma, residual_df, type)
  }

  # footer: r-squared
  if (isTRUE(show_r2)) {
    footer <- .add_footer_r2(footer, digits, r2, type)
  }

  # footer: p-adjustment
  if ("p" %in% colnames(x) && isTRUE(verbose)) {
    footer <- .add_footer_padjust(footer, p_adjust, type)
  }

  # footer: anova test
  if (!is.null(anova_test)) {
    footer <- .add_footer_anova_test(footer, anova_test, type)
  }

  # footer: anova test
  if (!is.null(anova_type)) {
    footer <- .add_footer_anova_type(footer, anova_type, type)
  }

  # footer: htest alternative
  if (!is.null(text_alternative)) {
    footer <- .add_footer_alternative(footer, text_alternative, type)
  }

  # footer: generic text
  if (!is.null(footer_text)) {
    footer <- .add_footer_text(footer, footer_text, type)
  }

  # add color code, if we have a footer
  if (!is.null(footer) && type == "text") {
    footer <- c(footer, "blue")
  }

  # if we have two trailing newlines, remove one
  if (identical(type, "text") && !is.null(footer) && grepl("\n\n$", footer[1])) {
    footer[1] <- substr(footer[1], 0, nchar(x) - 1)
  }

  footer
}


# footer: generic text
.add_footer_text <- function(footer = NULL, text, type = "text") {
  if (!is.null(text)) {
    if (type == "text") {
      if (is.null(footer)) {
        fill <- "\n"
      } else {
        fill <- ""
      }
      footer <- paste0(footer, sprintf("%s%s\n", fill, text))
    } else if (type == "html") {
      footer <- c(footer, gsub("\n", "", text))
    }
  }
  footer
}


# footer: residual standard deviation
.add_footer_sigma <- function(footer = NULL, digits, sigma, residual_df = NULL, type = "text") {
  if (!is.null(sigma)) {

    # format residual df
    if (!is.null(residual_df)) {
      res_df <- paste0(" (df = ", residual_df, ")")
    } else {
      res_df <- ""
    }

    if (type == "text") {
      if (is.null(footer)) {
        fill <- "\n"
      } else {
        fill <- ""
      }
      footer <- paste0(footer, sprintf("%sResidual standard deviation: %.*f%s\n", fill, digits, sigma, res_df))
    } else if (type == "html") {
      footer <- c(footer, insight::trim_ws(sprintf("Residual standard deviation: %.*f%s", digits, sigma, res_df)))
    }
  }
  footer
}


# footer: r-squared
.add_footer_r2 <- function(footer = NULL, digits, r2 = NULL, type = "text") {
  if (!is.null(r2)) {
    rsq <- tryCatch(
      {
        paste0(unlist(lapply(r2, function(i) {
          paste0(attributes(i)$names, ": ", insight::format_value(i, digits = digits))
        })), collapse = "; ")
      },
      error = function(e) {
        NULL
      }
    )
    if (!is.null(rsq)) {
      if (type == "text") {
        if (is.null(footer)) {
          fill <- "\n"
        } else {
          fill <- ""
        }
        footer <- paste0(footer, fill, rsq, "\n")
      } else if (type == "html") {
        footer <- c(footer, rsq)
      }
    }
  }
  footer
}


# footer: anova type
.add_footer_anova_type <- function(footer = NULL, aov_type, type = "text") {
  if (!is.null(aov_type)) {
    if (type == "text") {
      if (is.null(footer)) {
        fill <- "\n"
      } else {
        fill <- ""
      }
      footer <- paste0(footer, sprintf("%sAnova Table (Type %s tests)\n", fill, aov_type))
    } else if (type == "html") {
      footer <- c(footer, sprintf("Anova Table (Type %s tests)", aov_type))
    }
  }
  footer
}


# footer: anova test
.add_footer_anova_test <- function(footer = NULL, test, type = "text") {
  if (!is.null(test)) {
    if (type == "text") {
      if (is.null(footer)) {
        fill <- "\n"
      } else {
        fill <- ""
      }
      footer <- paste0(footer, sprintf("%s%s test statistic\n", fill, test))
    } else if (type == "html") {
      footer <- c(footer, sprintf("%s test statistic", test))
    }
  }
  footer
}


# footer: htest alternative
.add_footer_alternative <- function(footer = NULL, text_alternative, type = "text") {
  if (!is.null(text_alternative)) {
    if (type == "text") {
      if (is.null(footer)) {
        fill <- "\n"
      } else {
        fill <- ""
      }
      footer <- paste0(footer, sprintf("%s%s\n", fill, text_alternative))
    } else if (type == "html") {
      footer <- c(footer, text_alternative)
    }
  }
  footer
}


# footer: p-adjustment
.add_footer_padjust <- function(footer = NULL, p_adjust, type = "text") {
  if (!is.null(p_adjust) && p_adjust != "none") {
    if (type == "text") {
      if (is.null(footer)) {
        fill <- "\n"
      } else {
        fill <- ""
      }
      footer <- paste0(footer, fill, "p-value adjustment method: ", format_p_adjust(p_adjust), "\n")
    } else if (type == "html") {
      footer <- c(footer, paste0("p-value adjustment method: ", format_p_adjust(p_adjust)))
    }
  }
  footer
}


# footer: model formula
.add_footer_formula <- function(footer = NULL, model_formula, n_obs = NULL, type = "text") {
  if (!is.null(model_formula)) {

    # format n of observations
    if (!is.null(n_obs)) {
      n <- paste0(" (", n_obs, " Observations)")
    } else {
      n <- ""
    }

    if (type == "text") {
      if (is.null(footer)) {
        fill <- "\n"
      } else {
        fill <- ""
      }
      footer <- paste0(footer, fill, "Model: ", model_formula, n, "\n")
    } else if (type == "html") {
      footer <- c(footer, insight::trim_ws(paste0("Model: ", model_formula, n)))
    }
  }
  footer
}


# footer: type of uncertainty interval
.print_footer_cimethod <- function(x) {

  # get attributes
  ci_method <- .additional_arguments(x, "ci_method", NULL)
  test_statistic <- .additional_arguments(x, "test_statistic", NULL)
  bootstrap <- .additional_arguments(x, "bootstrap", FALSE)
  residual_df <- .additional_arguments(x, "residual_df", NULL)
  random_variances <- .additional_arguments(x, "ran_pars", FALSE)
  model_class <- .additional_arguments(x, "model_class", NULL)

  # prepare strings
  if (!is.null(ci_method)) {

    # in case of glm's that have df.residual(), and where residual df where requested
    if (ci_method == "residual" && test_statistic == "z-statistic" && !is.null(residual_df) && !is.infinite(residual_df) && !is.na(residual_df)) {
      test_statistic <- "t-statistic"
    }

    string_tailed <- switch(toupper(ci_method),
      "HDI" = "highest-density",
      "UNIROOT" = ,
      "PROFILE" = "profile-likelihood",
      "equal-tailed"
    )

    string_method <- switch(toupper(ci_method),
      "BCI" = ,
      "BCAI" = "bias-corrected accelerated bootstrap",
      "SI" = ,
      "CI" = ,
      "QUANTILE" = ,
      "ETI" = ,
      "HDI" = ifelse(isTRUE(bootstrap), "na\u0131ve bootstrap", "MCMC"),
      "NORMAL" = "Wald normal",
      "BOOT" = "parametric bootstrap",
      "Wald"
    )

    if (toupper(ci_method) %in% c("KENWARD", "KR", "KENWARD-ROGER", "KENWARD-ROGERS", "SATTERTHWAITE")) {
      string_approx <- paste0("with ", format_df_adjust(ci_method, approx_string = "", dof_string = ""), " ")
    } else {
      string_approx <- ""
    }

    if (!is.null(test_statistic) && !ci_method %in% c("normal") && !isTRUE(bootstrap)) {
      string_statistic <- switch(tolower(test_statistic),
        "t-statistic" = "t",
        "chi-squared statistic" = ,
        "z-statistic" = "z",
        ""
      )
      string_method <- paste0(string_method, " ", string_statistic, "-")
    } else {
      string_method <- paste0(string_method, " ")
    }

    # bootstrapped intervals
    if (isTRUE(bootstrap)) {
      msg <- paste0("\nUncertainty intervals (", string_tailed, ") are ", string_method, "intervals.")
    } else {
      msg <- paste0("\nUncertainty intervals (", string_tailed, ") and p-values (two-tailed) computed using a ", string_method, "distribution ", string_approx, "approximation.")
    }

    # do we have random effect variances from lme4/glmmTMB?
    # must be glmmTMB
    show_re_msg <- (identical(model_class, "glmmTMB") &&
      # and not Wald-CIs
      (string_method != "Wald z-" || ci_method != "wald")) ||
      # OR must be merMod
      ((identical(model_class, "lmerMod") || identical(model_class, "glmerMod")) &&
        # and not Wald CIs
        !ci_method %in% c("wald", "residual", "normal"))
    if (show_re_msg && isTRUE(random_variances) && !is.null(x$Effects) && "random" %in% x$Effects) {
      msg <- paste(msg, "Uncertainty intervals for random effect variances computed using a Wald z-distribution approximation.")
    }

    message(insight::format_message(msg))
  }
}

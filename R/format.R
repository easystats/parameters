# usual models ---------------------------------

#' @inheritParams print.parameters_model
#' @rdname print.parameters_model
#' @export
format.parameters_model <- function(x,
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
                                    ...) {
  # save attributes
  coef_name <- attributes(x)$coefficient_name
  coef_name2 <- attributes(x)$coefficient_name2
  s_value <- attributes(x)$s_value
  m_class <- attributes(x)$model_class
  htest_type <- attributes(x)$htest_type
  mixed_model <- attributes(x)$mixed_model
  random_variances <- isTRUE(attributes(x)$ran_pars)
  dist_params <- isTRUE(attributes(x)$dpars)
  mean_group_values <- attributes(x)$mean_group_values

  # process selection of columns
  style <- NULL
  if (!is.null(select) &&
    # glue-like syntax, so we switch to "style" argument here
    length(select) == 1 &&
    is.character(select) &&
    (grepl("{", select, fixed = TRUE) || select %in% .style_shortcuts)) {
    style <- select
    select <- NULL
  }

  # is information about grouped parameters stored as attribute?
  if (is.null(groups) && !is.null(attributes(x)$coef_groups)) {
    groups <- attributes(x)$coef_groups
  }

  # rename random effect parameters names for stan models
  if (isTRUE(random_variances) && any(c("brmsfit", "stanreg", "stanmvreg") %in% m_class)) {
    x <- .format_stan_parameters(x, dist_params)
  }

  # for the current HTML backend we use (package "gt"), we cannot change
  # the column header for subtables, so we need to remove the attributes
  # for the "Coefficient" column here, which else allows us to use different
  # column labels for subtables by model components
  if (identical(format, "html")) {
    coef_name <- NULL
    coef_name2 <- NULL
    attr(x, "coefficient_name") <- NULL
    attr(x, "coefficient_name2") <- NULL
    attr(x, "zi_coefficient_name") <- NULL
  }

  # remove method columns for htest and friends - this should be printed as footer
  if (!is.null(m_class) &&
    any(m_class %in% c(
      "BFBayesFactor", "htest", "rma", "t1way", "yuen",
      "PMCMR", "osrt", "trendPMCMR", "anova", "afex_aov"
    ))) {
    x$Method <- NULL
    x$Alternative <- NULL
  }

  # remove response for mvord
  if (!is.null(m_class) && any(m_class == "mvord")) {
    x$Response <- NULL
  }

  # remove component for nestedLogit
  if (!is.null(m_class) && any(m_class == "nestedLogit")) {
    x$Component <- NULL
    if (insight::has_single_value(x$Response, remove_na = TRUE)) {
      x$Response <- NULL
    }
  }

  # remove type for comparisons()
  if (!is.null(m_class) && any(m_class == "comparisons")) {
    x$Type <- NULL
  }

  # rename columns for t-tests
  if (!is.null(htest_type) &&
    htest_type == "ttest" &&
    !is.null(mean_group_values) &&
    all(c("Mean_Group1", "Mean_Group2") %in% colnames(x))) {
    colnames(x)[which(colnames(x) == "Mean_Group1")] <- paste0(x$Group, " = ", mean_group_values[1])
    colnames(x)[which(colnames(x) == "Mean_Group2")] <- paste0(x$Group, " = ", mean_group_values[2])
  }

  # for htests, remove "$" from variable name, since this can make troubles
  # when rendering into different output formats
  if (!is.null(htest_type)) {
    if ("Parameter" %in% colnames(x) && grepl("$", x$Parameter, fixed = TRUE)) {
      x$Parameter <- gsub("(.*)\\$(.*)", "\\2", x$Parameter)
    }
    if ("Group" %in% colnames(x) && grepl("$", x$Group, fixed = TRUE)) {
      x$Group <- gsub("(.*)\\$(.*)", "\\2", x$Group)
    }
  }

  # check if we have mixed models with random variance parameters
  # in such cases, we don't need the group-column, but we rather
  # merge it with the parameter column
  if (isTRUE(random_variances)) {
    x <- .format_ranef_parameters(x)
  }

  # prepare output, to have in shape for printing. this function removes
  # empty columns, or selects only those columns that should be printed
  x <- .prepare_x_for_print(x, select, coef_name, s_value)

  # check whether to split table by certain factors/columns (like component, response...)
  split_by <- .prepare_splitby_for_print(x)

  # format everything now...
  if (split_components && !is.null(split_by) && length(split_by)) {
    # this function mainly sets the appropriate column names for each
    # "sub table" (i.e. we print a table for each model component, like count,
    # zero-inflation, smooth, random, ...) and formats some parameter labels.
    # moreover, insight::format_table() is called to do the final formatting
    # and .format_model_component_header() is called to set captions for each
    # "sub table".
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
    # for tables that don't have multiple components, formatting is rather
    # easy, since we don't need to split the data frame into "sub tables"
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

  # remove unique columns
  if (insight::has_single_value(formatted_table$Component, remove_na = TRUE)) formatted_table$Component <- NULL
  if (insight::has_single_value(formatted_table$Effects, remove_na = TRUE)) formatted_table$Effects <- NULL
  if (insight::has_single_value(formatted_table$Group, remove_na = TRUE) && isTRUE(mixed_model)) formatted_table$Group <- NULL

  # no column with CI-level in output
  if (!is.null(formatted_table$CI) && insight::has_single_value(formatted_table$CI, remove_na = TRUE)) {
    formatted_table$CI <- NULL
  }

  # information about indention / row groups
  attr(formatted_table, "indent_rows") <- groups

  # vertical layout possible, if these have just one row
  if (identical(list(...)$layout, "vertical")) {
    if ("Parameter" %in% colnames(formatted_table)) {
      new_colnames <- c("", formatted_table$Parameter)
      formatted_table$Parameter <- NULL
    } else {
      new_colnames <- c("Type", paste0("Value ", seq_len(nrow(formatted_table))))
    }
    formatted_table <- datawizard::rownames_as_column(as.data.frame(t(formatted_table)), "Type")
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
format.compare_parameters <- function(x,
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
                                      engine = NULL,
                                      ...) {
  m_class <- attributes(x)$model_class
  x$Method <- NULL

  # remove response for mvord
  if (!is.null(m_class) && any(m_class == "mvord")) {
    x$Response <- NULL
  }

  out <- data.frame(
    Parameter = x$Parameter,
    Effects = x$Effects,
    Component = x$Component,
    stringsAsFactors = FALSE
  )

  # remove zi-suffix if we split components anyway
  if (isTRUE(split_components)) {
    out$Parameter <- insight::trim_ws(gsub(" (zi)", "", out$Parameter, fixed = TRUE))
    out$Effects <- NULL
  }

  # save model names
  models <- attributes(x)$model_names

  # save model parameters attributes
  parameters_attributes <- attributes(x)$all_attributes

  # is information about grouped parameters stored as attribute?
  if (is.null(groups) && !is.null(parameters_attributes[[1]]$coef_groups)) {
    groups <- parameters_attributes[[1]]$coef_groups
  }

  # locate random effects rows
  ran_pars <- which(x$Effects == "random")

  # find all random effect groups
  if (is.null(x$Group)) {
    ran_groups <- NULL
    ran_group_rows <- NULL
  } else {
    ran_groups <- unique(insight::compact_character(x$Group))
    ran_group_rows <- which(nzchar(x$Group, keepNA = TRUE))
  }

  for (i in models) {
    # each column is suffixed with ".model_name", so we extract
    # columns for each model separately here
    pattern <- paste0("\\.\\Q", i, "\\E$")
    cols <- x[grepl(pattern, colnames(x))]
    # since we now have the columns for a single model, we clean the
    # column names (i.e. remove suffix), so we can use "format_table" function
    colnames(cols) <- gsub(pattern, "", colnames(cols))
    # find coefficient column, check which rows have non-NA values
    # since we merged all models together, and we only have model-specific
    # columns for estimates, CI etc. but not for Effects and Component, we
    # extract "valid" rows via non-NA values in the coefficient column
    coef_column <- which(colnames(cols) %in% c(.all_coefficient_types, "Coefficient"))
    valid_rows <- which(!is.na(cols[[coef_column]]))
    # check if we have mixed models with random variance parameters
    # in such cases, we don't need the group-column, but we rather
    # merge it with the parameter column
    ran_pars_rows <- NULL
    if (length(ran_pars) && length(ran_group_rows) && any(ran_group_rows %in% valid_rows)) {
      # ran_pars has row indices for *all* models in this function -
      # make sure we have only valid rows for this particular model
      ran_pars_rows <- intersect(valid_rows, intersect(ran_pars, ran_group_rows))
    }
    if (!is.null(ran_pars_rows) && length(ran_pars_rows)) {
      # find SD random parameters
      stddevs <- startsWith(out$Parameter[ran_pars_rows], "SD (")
      # check if we already fixed that name in a previous loop
      fixed_name <- unlist(lapply(
        ran_groups,
        grep,
        x = out$Parameter[ran_pars_rows[stddevs]],
        fixed = TRUE
      ))
      if (length(fixed_name)) {
        stddevs[fixed_name] <- FALSE
      }
      # collapse parameter name with RE grouping factor
      if (length(stddevs)) {
        out$Parameter[ran_pars_rows[stddevs]] <- paste0(
          gsub("(.*)\\)", "\\1", out$Parameter[ran_pars_rows[stddevs]]),
          ": ",
          x$Group[ran_pars_rows[stddevs]],
          ")"
        )
      }
      # same for correlations
      corrs <- startsWith(out$Parameter[ran_pars_rows], "Cor (")
      # check if we already fixed that name in a previous loop
      fixed_name <- unlist(lapply(
        ran_groups,
        grep,
        x = out$Parameter[ran_pars_rows[corrs]],
        fixed = TRUE
      ))
      if (length(fixed_name)) {
        corrs[fixed_name] <- FALSE
      }
      # collapse parameter name with RE grouping factor
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
    attributes(cols)$coef_name <- colnames(cols)[coef_column]
    # save p-stars in extra column
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

    # add modelname to column names; for single column layout per model, we just
    # need the column name. If the layout contains more than one column per model,
    # add modelname in parenthesis.
    if (ncol(cols) > 1) {
      colnames(cols) <- paste0(colnames(cols), " (", i, ")")
    } else {
      colnames(cols) <- i
    }

    out <- cbind(out, cols)
  }

  # remove group column
  out$Group <- NULL
  x$Group <- NULL

  # sort by effects and component
  if (isFALSE(split_components)) {
    out <- datawizard::data_arrange(out, c("Effects", "Component"))
  }

  # check whether to split table by certain factors/columns (like component, response...)
  split_by <- split_column <- .prepare_splitby_for_print(x)

  if (length(split_by) > 0L && isTRUE(split_components)) {
    # set up split-factor
    if (length(split_column) > 1L) {
      split_by <- lapply(split_column, function(i) x[[i]])
    } else {
      split_by <- list(x[[split_column]])
    }
    names(split_by) <- split_column

    # make sure we have correct sorting here...
    formatted_table <- split(out, f = split_by)
    formatted_table <- lapply(names(formatted_table), function(tab) {
      i <- formatted_table[[tab]]
      # check if data frame is empty - this may happen if not all combinations
      # of split_by factors are present in the data (e.g., zero-inflated mixed
      # models, that have random effects for the count, but not for the zero-
      # inflation component)
      if (nrow(i) == 0L) {
        return(NULL)
      }
      # remove unique columns
      if (insight::has_single_value(i$Component, remove_na = TRUE)) i$Component <- NULL
      if (insight::has_single_value(i$Effects, remove_na = TRUE)) i$Effects <- NULL
      # format table captions for sub tables
      table_caption <- .format_model_component_header(
        x,
        type = tab, split_column = tab, is_zero_inflated = FALSE,
        is_ordinal_model = FALSE, is_multivariate = FALSE, ran_pars = FALSE,
        formatted_table = i
      )
      # add as attribute, so table captions are printed
      if (identical(format, "html")) {
        i$Component <- table_caption$name
      } else if (identical(format, "md") || identical(format, "markdown")) {
        attr(i, "table_caption") <- table_caption$name
      } else {
        attr(i, "table_caption") <- c(paste("#", table_caption$name), "blue")
      }
      i
    })

    # remove empty tables
    formatted_table <- insight::compact_list(formatted_table)

    # for HTML, bind data frames
    if (identical(format, "html")) {
      # fix non-equal length of columns and bind data frames
      formatted_table <- do.call(rbind, .fix_nonmatching_columns(formatted_table))
    }
  } else {
    formatted_table <- out
    # remove unique columns
    if (insight::has_single_value(formatted_table$Component, remove_na = TRUE)) formatted_table$Component <- NULL
    if (insight::has_single_value(formatted_table$Effects, remove_na = TRUE)) formatted_table$Effects <- NULL
    # add line with info about observations
    formatted_table <- .add_obs_row(formatted_table, parameters_attributes, style = select)
  }

  # information about indention / row groups
  attr(formatted_table, "indent_rows") <- groups

  formatted_table
}


# sem-models ---------------------------------

#' @export
format.parameters_sem <- function(x,
                                  digits = 2,
                                  ci_digits = digits,
                                  p_digits = 3,
                                  format = NULL,
                                  ci_width = NULL,
                                  ci_brackets = TRUE,
                                  pretty_names = TRUE,
                                  ...) {
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

.format_footer <- function(x,
                           digits = 3,
                           verbose = TRUE,
                           show_sigma = FALSE,
                           show_formula = FALSE,
                           show_r2 = FALSE,
                           show_rmse = FALSE,
                           format = "text") {
  # prepare footer
  footer <- NULL
  type <- tolower(format)

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

  # footer: model formula
  if (isTRUE(show_formula)) {
    footer <- .add_footer_formula(footer, model_formula, n_obs, type)
  }

  # footer: residual standard deviation
  if (isTRUE(show_sigma)) {
    footer <- .add_footer_sigma(footer, digits, sigma_value, residual_df, type)
  }

  # footer: r-squared
  if (isTRUE(show_rmse)) {
    footer <- .add_footer_values(footer, digits, value = rmse, text = "RMSE ", type)
  }

  # footer: r-squared
  if (isTRUE(show_r2)) {
    footer <- .add_footer_r2(footer, digits, r2, type)
  }

  # footer: p-adjustment
  if ("p" %in% colnames(x) && isTRUE(verbose) && !is.null(p_adjust) && p_adjust != "none") {
    footer <- .add_footer_text(footer, text = paste("p-value adjustment method:", format_p_adjust(p_adjust)))
  }

  # footer: anova test
  if (!is.null(anova_test)) {
    footer <- .add_footer_text(footer, text = sprintf("%s test statistic", anova_test))
  }

  # footer: anova type
  if (!is.null(anova_type)) {
    footer <- .add_footer_text(footer, text = sprintf("Anova Table (Type %s tests)", anova_type))
  }


  # footer: marginaleffects::comparisons()
  if (!is.null(prediction_type)) {
    footer <- .add_footer_text(footer, text = sprintf("Prediction type: %s", prediction_type))
  }

  # footer: htest alternative
  if (!is.null(text_alternative)) {
    footer <- .add_footer_text(footer, text = text_alternative)
  }

  # footer: generic text
  if (!is.null(footer_text)) {
    footer <- .add_footer_text(footer, footer_text, type)
  }

  # if we have two trailing newlines, remove one
  if (identical(type, "text") && !is.null(footer) && endsWith(footer[1], "\n\n")) {
    footer[1] <- substr(footer[1], 0, nchar(x) - 1)
  }

  footer
}


# footer: generic text
.add_footer_text <- function(footer = NULL, text = NULL, type = "text") {
  if (!is.null(text) && length(text)) {
    if (type == "text" || type == "markdown") {
      if (is.null(footer)) {
        fill <- "\n"
      } else {
        fill <- ""
      }
      footer <- paste0(footer, sprintf("%s%s\n", fill, text))
    } else if (type == "html") {
      footer <- c(footer, gsub("\n", "", text, fixed = TRUE))
    }
  }
  footer
}


# footer: generic values
.add_footer_values <- function(footer = NULL,
                               digits = 3,
                               value = NULL,
                               text = NULL,
                               type = "text") {
  if (!is.null(value) && !is.null(text)) {
    string <- sprintf("%s: %s", text, insight::format_value(value, digits = digits))
    if (type == "text" || type == "markdown") {
      if (is.null(footer)) {
        fill <- "\n"
      } else {
        fill <- ""
      }
      footer <- paste0(footer, fill, string, "\n")
    } else if (type == "html") {
      footer <- c(footer, string)
    }
  }
  footer
}


# footer: residual standard deviation
.add_footer_sigma <- function(footer = NULL, digits = 3, sigma = NULL, residual_df = NULL, type = "text") {
  if (!is.null(sigma)) {
    # format residual df
    if (is.null(residual_df)) {
      res_df <- ""
    } else {
      res_df <- paste0(" (df = ", residual_df, ")")
    }

    if (type == "text" || type == "markdown") {
      if (is.null(footer)) {
        fill <- "\n"
      } else {
        fill <- ""
      }
      footer <- paste0(footer, sprintf("%sSigma: %.*f%s\n", fill, digits, sigma, res_df))
    } else if (type == "html") {
      footer <- c(footer, insight::trim_ws(sprintf("Sigma: %.*f%s", digits, sigma, res_df)))
    }
  }
  footer
}


# footer: r-squared
.add_footer_r2 <- function(footer = NULL, digits = 3, r2 = NULL, type = "text") {
  if (!is.null(r2)) {
    rsq <- .safe(paste(unlist(lapply(r2, function(i) {
      paste0(attributes(i)$names, ": ", insight::format_value(i, digits = digits))
    })), collapse = "; "))

    if (!is.null(rsq)) {
      if (type == "text" || type == "markdown") {
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


# footer: model formula
.add_footer_formula <- function(footer = NULL, model_formula = NULL, n_obs = NULL, type = "text") {
  if (!is.null(model_formula)) {
    # format n of observations
    if (is.null(n_obs)) {
      n <- ""
    } else {
      n <- paste0(" (", n_obs, " Observations)")
    }

    if (type == "text" || type == "markdown") {
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
  if (isTRUE(getOption("parameters_cimethod", TRUE))) {
    # get attributes
    ci_method <- .additional_arguments(x, "ci_method", NULL)
    test_statistic <- .additional_arguments(x, "test_statistic", NULL)
    bootstrap <- .additional_arguments(x, "bootstrap", FALSE)
    is_bayesian <- .additional_arguments(x, "is_bayesian", FALSE)
    simulated <- .additional_arguments(x, "simulated", FALSE)
    residual_df <- .additional_arguments(x, "residual_df", NULL)
    random_variances <- .additional_arguments(x, "ran_pars", FALSE)
    model_class <- .additional_arguments(x, "model_class", NULL)

    # prepare strings
    if (!is.null(ci_method)) {
      # only random effects? no message for fixed effects ci-approximation
      if (!is.null(x$Effects) && all(x$Effects == "random")) {
        msg <- "\n"
        string_method <- ""

        # here we have fixed effects only, or fixed and random effects
      } else {
        # since `.format_ci_method_name()` changes the CI method names to have a
        # mix of cases, standardize them by converting to lower case
        ci_method <- tolower(ci_method)

        # in case of glm's that have df.residual(), and where residual df where requested
        is_test_statistic_t <- ci_method == "residual" &&
          test_statistic == "z-statistic" &&
          !is.null(residual_df) &&
          !is.infinite(residual_df) && !is.na(residual_df)

        if (is_test_statistic_t) {
          test_statistic <- "t-statistic"
        }

        string_tailed <- switch(ci_method,
          hdi = "highest-density",
          uniroot = ,
          profile = "profile-likelihood",
          "equal-tailed"
        )

        # sampling method
        if (isTRUE(bootstrap)) {
          sampling_method <- ifelse(isTRUE(.unicode_symbols()), "na\u0131ve bootstrap", "naive bootstrap")
        } else if (isTRUE(simulated)) {
          sampling_method <- "simulated multivariate normal"
        } else {
          sampling_method <- "MCMC"
        }

        string_method <- switch(ci_method,
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

        if (toupper(ci_method) %in% c("KENWARD", "KR", "KENWARD-ROGER", "KENWARD-ROGERS", "SATTERTHWAITE")) {
          string_approx <- paste0("with ", format_df_adjust(ci_method, approx_string = "", dof_string = ""), " ")
        } else {
          string_approx <- ""
        }

        if (!is.null(test_statistic) && ci_method != "normal" && !isTRUE(bootstrap)) {
          string_statistic <- switch(tolower(test_statistic),
            `t-statistic` = "t",
            `chi-squared statistic` = ,
            `z-statistic` = "z",
            ""
          )
          string_method <- paste0(string_method, " ", string_statistic, "-")
        } else {
          string_method <- paste0(string_method, " ")
        }

        # bootstrapped intervals
        if (isTRUE(bootstrap)) {
          msg <- paste0("\nUncertainty intervals (", string_tailed, ") are ", string_method, "intervals.")
        } else if (isTRUE(is_bayesian)) {
          msg <- paste0("\nUncertainty intervals (", string_tailed, ") computed using a ", string_method, "distribution ", string_approx, "approximation.") # nolint
        } else {
          msg <- paste0("\nUncertainty intervals (", string_tailed, ") and p-values (two-tailed) computed using a ", string_method, "distribution ", string_approx, "approximation.") # nolint
        }
      }

      # do we have random effect variances from lme4/glmmTMB?
      # must be glmmTMB
      show_re_msg <- (identical(model_class, "glmmTMB") &&
        # and not Wald-/normalCIs
        (!string_method %in% c("Wald z-", "Wald normal") || !ci_method %in% c("wald", "normal"))) ||
        # OR must be merMod
        ((identical(model_class, "lmerMod") || identical(model_class, "glmerMod")) &&
          # and not Wald CIs
          !ci_method %in% c("wald", "normal", "profile", "boot"))

      if (show_re_msg && isTRUE(random_variances) && !is.null(x$Effects) && "random" %in% x$Effects) {
        msg <- paste(msg, "Uncertainty intervals for random effect variances computed using a Wald z-distribution approximation.") # nolint
      }

      insight::format_alert(insight::color_text(msg, "yellow"))
    }
  }
}


.print_footer_exp <- function(x) {
  # we need this to check whether we have extremely large cofficients
  if (isTRUE(getOption("parameters_exponentiate", TRUE))) {
    msg <- NULL
    # try to find out the name of the coefficient column
    coef_column <- intersect(colnames(x), .all_coefficient_names)
    if (length(coef_column) && "Parameter" %in% colnames(x)) {
      spurious_coefficients <- abs(x[[coef_column[1]]][!.in_intercepts(x$Parameter)])
    } else {
      spurious_coefficients <- NULL
    }
    exponentiate <- .additional_arguments(x, "exponentiate", FALSE)
    if (!.is_valid_exponentiate_argument(exponentiate)) {
      if (isTRUE(.additional_arguments(x, "log_link", FALSE))) {
        msg <- "The model has a log- or logit-link. Consider using `exponentiate = TRUE` to interpret coefficients as ratios." # nolint
        # we only check for exp(coef), so exp() here since coefficients are on logit-scale
        if (!is.null(spurious_coefficients)) {
          spurious_coefficients <- exp(spurious_coefficients)
        }
      } else if (isTRUE(.additional_arguments(x, "log_response", FALSE))) {
        msg <- "The model has a log-transformed response variable. Consider using `exponentiate = TRUE` to interpret coefficients as ratios." # nolint
        # don't show warning about complete separation
        spurious_coefficients <- NULL
      }
    } else if (.is_valid_exponentiate_argument(exponentiate) && isTRUE(.additional_arguments(x, "log_response", FALSE))) { # nolint
      # don't show warning about complete separation
      spurious_coefficients <- NULL
    }

    # following check only for models with logit-link
    logit_model <- isTRUE(.additional_arguments(x, "logit_link", FALSE)) ||
      isTRUE(attributes(x)$coefficient_name %in% c("Log-Odds", "Odds Ratio"))

    # remove NA and infinite values from spurios coefficients
    if (!is.null(spurious_coefficients)) {
      spurious_coefficients <- spurious_coefficients[!is.na(spurious_coefficients) & !is.infinite(spurious_coefficients)] # nolint
    }

    # check for complete separation coefficients or possible issues with
    # too few data points
    if (!is.null(spurious_coefficients) && length(spurious_coefficients) && logit_model) {
      if (any(spurious_coefficients > 50)) {
        msg <- c(msg, "Some coefficients are very large, which may indicate issues with complete separation.") # nolint
      } else if (any(spurious_coefficients > 15)) {
        msg <- c(msg, "Some coefficients seem to be rather large, which may indicate issues with (quasi) complete separation. Consider using bias-corrected or penalized regression models.") # nolint
      }
    }

    if (!is.null(msg) && isTRUE(getOption("parameters_warning_exponentiate", TRUE))) {
      insight::format_alert(paste0("\n", msg))
      # set flag, so message only displayed once per session
      options(parameters_warning_exponentiate = FALSE)
    }
  }
}

# usual models ---------------------------------

#' @inheritParams print.parameters_model
#' @rdname display.parameters_model
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
    x <- .format_stan_parameters(x)
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
    if (insight::n_unique(x$Response) == 1) {
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

  # Special print for mcp from WRS2
  if (!is.null(m_class) && any(m_class %in% c("mcp1", "mcp2"))) {
    x$Group1 <- paste(x$Group1, x$Group2, sep = " vs. ")
    x$Group2 <- NULL
    colnames(x)[1] <- "Group"
  }

  # check if we have mixed models with random variance parameters
  # in such cases, we don't need the group-column, but we rather
  # merge it with the parameter column
  if (isTRUE(random_variances) && !is.null(x$Group) && !is.null(x$Effects)) {
    ran_pars <- which(x$Effects == "random")
    stddevs <- startsWith(x$Parameter[ran_pars], "SD (")
    x$Parameter[ran_pars[stddevs]] <- paste0(
      gsub("(.*)\\)", "\\1", x$Parameter[ran_pars[stddevs]]),
      ": ",
      x$Group[ran_pars[stddevs]],
      ")"
    )
    corrs <- startsWith(x$Parameter[ran_pars], "Cor (")
    x$Parameter[ran_pars[corrs]] <- paste0(
      gsub("(.*)\\)", "\\1", x$Parameter[ran_pars[corrs]]),
      ": ",
      x$Group[ran_pars[corrs]],
      ")"
    )
    x$Parameter[x$Parameter == "SD (Observations: Residual)"] <- "SD (Residual)"
    x$Group <- NULL
  }

  # group parameters - this function find those parameters that should be
  # grouped, reorders parameters into groups and indents lines that belong
  # to one group, adding a header for each group
  if (!is.null(groups)) {
    x <- .parameter_groups(x, groups)
  }
  indent_groups <- attributes(x)$indent_groups
  indent_rows <- attributes(x)$indent_rows

  # prepare output, to have in shape for printing. this function removes
  # empty columns, or selects only those columns that should be printed
  x <- .prepare_x_for_print(x, select, coef_name, s_value)

  # check whether to split table by certain factors/columns (like component, response...)
  split_by <- .prepare_splitby_for_print(x)

  # add p-stars, if we need this for style-argument
  if (!is.null(style) && grepl("{stars}", style, fixed = TRUE)) {
    x$p_stars <- insight::format_p(x[["p"]], stars = TRUE, stars_only = TRUE)
  }

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
      ...
    )
  }

  # remove unique columns
  if (insight::n_unique(formatted_table$Component) == 1) formatted_table$Component <- NULL
  if (insight::n_unique(formatted_table$Effects) == 1) formatted_table$Effects <- NULL
  if (insight::n_unique(formatted_table$Group) == 1 && isTRUE(mixed_model)) formatted_table$Group <- NULL

  # no column with CI-level in output
  if (!is.null(formatted_table$CI) && insight::n_unique(formatted_table$CI) == 1) {
    formatted_table$CI <- NULL
  }

  # we also allow style-argument for model parameters. In this case, we need
  # some small preparation, namely, we need the p_stars column, and we need
  # to "split" the formatted table, because the glue-function needs the columns
  # without the parameters-column.
  if (!is.null(style)) {
    if (is.data.frame(formatted_table)) {
      formatted_table <- .style_formatted_table(
        formatted_table,
        style = style,
        format = format
      )
    } else {
      formatted_table[] <- lapply(
        formatted_table,
        .style_formatted_table,
        style = style,
        format = format
      )
    }
  }

  if (!is.null(indent_rows)) {
    attr(formatted_table, "indent_rows") <- indent_rows
    attr(formatted_table, "indent_groups") <- NULL
  } else if (!is.null(indent_groups)) {
    attr(formatted_table, "indent_groups") <- indent_groups
  }

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





# Compare parameters ----------------------


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
  if (!is.null(x$Group)) {
    ran_groups <- unique(insight::compact_character(x$Group))
    ran_group_rows <- which(nchar(x$Group) > 0)
  } else {
    ran_groups <- NULL
    ran_group_rows <- NULL
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
    coef_column <- which(colnames(cols) %in% c(.all_coefficient_types(), "Coefficient"))
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
      fixed_name <- unlist(lapply(ran_groups, function(g) {
        which(grepl(g, out$Parameter[ran_pars_rows[stddevs]], fixed = TRUE))
      }))
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
      fixed_name <- unlist(lapply(ran_groups, function(g) {
        which(grepl(g, out$Parameter[ran_pars_rows[corrs]], fixed = TRUE))
      }))
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
    # save p-stars in extra column
    cols$p_stars <- insight::format_p(cols$p, stars = TRUE, stars_only = TRUE)
    cols <- insight::format_table(
      cols,
      digits = digits,
      ci_width = ci_width,
      ci_brackets = ci_brackets,
      ci_digits = ci_digits,
      p_digits = p_digits,
      zap_small = zap_small,
      ...
    )
    out <- cbind(out, .format_output_style(cols, style = select, format, i))
  }

  # remove group column
  out$Group <- NULL
  x$Group <- NULL

  # sort by effects and component
  if (isFALSE(split_components)) {
    out <- datawizard::data_arrange(out, c("Effects", "Component"))
  }

  # group parameters - this function find those parameters that should be
  # grouped, reorders parameters into groups and indents lines that belong
  # to one group, adding a header for each group
  if (!is.null(groups)) {
    out <- .parameter_groups(out, groups)
  }
  indent_groups <- attributes(x)$indent_groups
  indent_rows <- attributes(x)$indent_rows


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
      # remove unique columns
      if (insight::n_unique(i$Component) == 1L) i$Component <- NULL
      if (insight::n_unique(i$Effects) == 1L) i$Effects <- NULL
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

    # for HTML, bind data frames
    if (identical(format, "html")) {
      # fix non-equal length of columns and bind data frames
      formatted_table <- do.call(rbind, .fix_nonmatching_columns(formatted_table))
    }
  } else {
    formatted_table <- out
    # remove unique columns
    if (insight::n_unique(formatted_table$Component) == 1L) formatted_table$Component <- NULL
    if (insight::n_unique(formatted_table$Effects) == 1L) formatted_table$Effects <- NULL
    # add line with info about observations
    formatted_table <- .add_obs_row(formatted_table, parameters_attributes, style = select)
  }

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


# helper ---------------------

.style_formatted_table <- function(formtab, style, format) {
  additional_columns <- intersect(c("Effects", "Group", "Component"), colnames(formtab))
  if (length(additional_columns)) {
    additional_columns <- formtab[additional_columns]
  }
  # define column names in case the glue-pattern has multiple columns.
  if (grepl("|", style, fixed = TRUE)) {
    cn <- NULL
  } else {
    cn <- .style_pattern_to_name(style)
  }
  formtab <- cbind(
    formtab[1],
    .format_output_style(
      formtab[2:ncol(formtab)],
      style = style,
      format = format,
      modelname = cn
    )
  )
  if (!insight::is_empty_object(additional_columns)) {
    formtab <- cbind(formtab, additional_columns)
  }
  formtab
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
  prediction_type <- attributes(x)$prediction_type
  footer_text <- attributes(x)$footer_text
  text_alternative <- attributes(x)$text_alternative
  n_obs <- attributes(x)$n_obs
  is_ggeffects <- isTRUE(attributes(x)$is_ggeffects)

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

  # footer: marginaleffects::comparisons()
  if (!is.null(prediction_type)) {
    footer <- .add_footer_prediction_type(footer, prediction_type, type)
  }

  # footer: htest alternative
  if (!is.null(text_alternative)) {
    footer <- .add_footer_alternative(footer, text_alternative, type)
  }

  # footer: generic text
  if (!is.null(footer_text)) {
    footer <- .add_footer_text(footer, footer_text, type, is_ggeffects)
  }

  # add color code, if we have a footer
  if (!is.null(footer) && type == "text") {
    footer <- c(footer, "blue")
  }

  # if we have two trailing newlines, remove one
  if (identical(type, "text") && !is.null(footer) && endsWith(footer[1], "\n\n")) {
    footer[1] <- substr(footer[1], 0, nchar(x) - 1)
  }

  # finally, for ggeffects and HTML, remove *
  if (is_ggeffects && type == "html") {
    footer <- gsub("*", "", footer, fixed = TRUE)
    footer <- gsub(":;", ":", footer, fixed = TRUE)
  }

  footer
}


# footer: generic text
.add_footer_text <- function(footer = NULL, text = NULL, type = "text", is_ggeffects = FALSE) {
  if (!is.null(text)) {
    if (type == "text" || type == "markdown") {
      if (is.null(footer)) {
        fill <- "\n"
      } else {
        fill <- ""
      }
      footer <- paste0(footer, sprintf("%s%s\n", fill, text))
    } else if (type == "html") {
      replacement <- ifelse(is_ggeffects, ";", "")
      footer <- c(footer, gsub("\n", replacement, text, fixed = TRUE))
    }
  }
  footer
}


# footer: residual standard deviation
.add_footer_sigma <- function(footer = NULL, digits = 3, sigma = NULL, residual_df = NULL, type = "text") {
  if (!is.null(sigma)) {
    # format residual df
    if (!is.null(residual_df)) {
      res_df <- paste0(" (df = ", residual_df, ")")
    } else {
      res_df <- ""
    }

    if (type == "text" || type == "markdown") {
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
.add_footer_r2 <- function(footer = NULL, digits = 3, r2 = NULL, type = "text") {
  if (!is.null(r2)) {
    rsq <- .safe(paste0(unlist(lapply(r2, function(i) {
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


# footer: anova type
.add_footer_anova_type <- function(footer = NULL, aov_type = NULL, type = "text") {
  if (!is.null(aov_type)) {
    if (type == "text" || type == "markdown") {
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


# footer: marginaleffects::comparisions() prediction_type
.add_footer_prediction_type <- function(footer = NULL, prediction_type = NULL, type = "text") {
  if (!is.null(prediction_type)) {
    if (type == "text" || type == "markdown") {
      if (is.null(footer)) {
        fill <- "\n"
      } else {
        fill <- ""
      }
      footer <- paste0(footer, sprintf("%sPrediction type: %s\n", fill, prediction_type))
    } else if (type == "html") {
      footer <- c(footer, sprintf("Prediction type: %s", prediction_type))
    }
  }
  footer
}


# footer: anova test
.add_footer_anova_test <- function(footer = NULL, test = NULL, type = "text") {
  if (!is.null(test)) {
    if (type == "text" || type == "markdown") {
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
.add_footer_alternative <- function(footer = NULL, text_alternative = NULL, type = "text") {
  if (!is.null(text_alternative)) {
    if (type == "text" || type == "markdown") {
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
.add_footer_padjust <- function(footer = NULL, p_adjust = NULL, type = "text") {
  if (!is.null(p_adjust) && p_adjust != "none") {
    if (type == "text" || type == "markdown") {
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
.add_footer_formula <- function(footer = NULL, model_formula = NULL, n_obs = NULL, type = "text") {
  if (!is.null(model_formula)) {
    # format n of observations
    if (!is.null(n_obs)) {
      n <- paste0(" (", n_obs, " Observations)")
    } else {
      n <- ""
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

        if (!is.null(test_statistic) && !ci_method == "normal" && !isTRUE(bootstrap)) {
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

      insight::format_alert(msg)
    }
  }
}


.print_footer_exp <- function(x) {
  if (isTRUE(getOption("parameters_exponentiate", TRUE))) {
    msg <- NULL
    # we need this to check whether we have extremely large cofficients
    spurious_coefficients <- abs(x$Coefficient[!.in_intercepts(x$Parameter)])
    exponentiate <- .additional_arguments(x, "exponentiate", FALSE)
    if (!.is_valid_exponentiate_argument(exponentiate)) {
      if (isTRUE(.additional_arguments(x, "log_link", FALSE))) {
        msg <- "The model has a log- or logit-link. Consider using `exponentiate = TRUE` to interpret coefficients as ratios." # nolint
        # we only check for exp(coef), so exp() here soince coefficients are on logit-scale
        spurious_coefficients <- exp(spurious_coefficients)
      } else if (isTRUE(.additional_arguments(x, "log_response", FALSE))) {
        msg <- "The model has a log-transformed response variable. Consider using `exponentiate = TRUE` to interpret coefficients as ratios." # nolint
        # don't show warning about complete separation
        spurious_coefficients <- NULL
      }
    } else if (.is_valid_exponentiate_argument(exponentiate) && isTRUE(.additional_arguments(x, "log_response", FALSE))) { # nolint
      msg <- c(
        "This model has a log-transformed response variable, and exponentiated parameters are reported.",
        "A one-unit increase in the predictor is associated with multiplying the outcome by that predictor's coefficient." # nolint
      )
      # don't show warning about complete separation
      spurious_coefficients <- NULL
    }

    # check for complete separation coefficients or possible issues with
    # too few data points
    if (!is.null(spurious_coefficients) && any(spurious_coefficients > 140)) {
      msg <- c(msg, "Note that some coefficients are very large, which may indicate issues with complete separation.") # nolint
    } else if (any(spurious_coefficients > 20)) {
      msg <- c(msg, "Note that some coefficients are very large, which may indicate issues with too few data points for some parameters. Consider using bias-corrected or penalized regression models.") # nolint
    }

    if (!is.null(msg) && isTRUE(getOption("parameters_warning_exponentiate", TRUE))) {
      insight::format_alert(paste0("\n", msg))
      # set flag, so message only displayed once per session
      options(parameters_warning_exponentiate = FALSE)
    }
  }
}

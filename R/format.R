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
                                    ...) {
  # save attributes
  coef_name <- attributes(x)$coefficient_name
  s_value <- attributes(x)$s_value
  m_class <- attributes(x)$model_class
  htest_type <- attributes(x)$htest_type
  mixed_model <- attributes(x)$mixed_model
  random_variances <- isTRUE(attributes(x)$ran_pars)
  mean_group_values <- attributes(x)$mean_group_values

  if (identical(format, "html")) {
    coef_name <- NULL
    attr(x, "coefficient_name") <- NULL
    attr(x, "zi_coefficient_name") <- NULL
  }

  # remove method for htest
  if (!is.null(m_class) && any(m_class %in% c("BFBayesFactor", "htest", "rma", "t1way", "yuen", "PMCMR", "osrt", "trendPMCMR", "anova"))) {
    x$Method <- NULL
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

  # prepare output, to have in shape for printing
  x <- .prepare_x_for_print(x, select, coef_name, s_value)

  # check whether to split table by certain factors/columns (like component, response...)
  split_by <- .prepare_splitby_for_print(x)

  # print everything now...
  if (split_components && !is.null(split_by) && length(split_by)) {
    formatted_table <- .print_model_parms_components(x, pretty_names, split_column = split_by, digits = digits, ci_digits = ci_digits, p_digits = p_digits, coef_column = coef_name, format = format, ci_width = ci_width, ci_brackets = ci_brackets, zap_small = zap_small, ...)
  } else {
    formatted_table <- .format_columns_single_component(x, pretty_names = pretty_names, digits = digits, ci_width = ci_width, ci_brackets = ci_brackets, ci_digits = ci_digits, p_digits = p_digits, format = format, coef_name = coef_name, zap_small = zap_small, ...)
  }

  # remove unique columns
  if (.n_unique(formatted_table$Component) == 1) formatted_table$Component <- NULL
  if (.n_unique(formatted_table$Effects) == 1) formatted_table$Effects <- NULL
  if (.n_unique(formatted_table$Group) == 1 && isTRUE(mixed_model)) formatted_table$Group <- NULL

  # no column with CI-level in output
  if (!is.null(formatted_table$CI) && .n_unique(formatted_table$CI) == 1) {
    formatted_table$CI <- NULL
  }

  formatted_table
}


.format_columns_single_component <- function(x, pretty_names, digits = 2, ci_digits = 2, p_digits = 3, ci_width = "auto", ci_brackets = TRUE, format = NULL, coef_name = NULL, zap_small = FALSE, ...) {
  # default brackets are parenthesis for HTML / MD
  if ((is.null(ci_brackets) || isTRUE(ci_brackets)) && (identical(format, "html") || identical(format, "markdown"))) {
    brackets <- c("(", ")")
  } else if (is.null(ci_brackets) || isTRUE(ci_brackets)) {
    brackets <- c("[", "]")
  } else {
    brackets <- ci_brackets
  }

  # fix coefficient column name for random effects
  if (all(x$EFfects == "random") && any(colnames(x) %in% .all_coefficient_types())) {
    colnames(x)[colnames(x) %in% .all_coefficient_types()] <- "Coefficient"
  }

  # random pars with level? combine into parameter column
  if (all(c("Parameter", "Level") %in% colnames(x))) {
    x$Parameter <- paste0(x$Parameter, " ", brackets[1], x$Level, brackets[2])
    x$Level <- NULL
  }

  insight::format_table(x, pretty_names = pretty_names, digits = digits, ci_width = ci_width, ci_brackets = ci_brackets, ci_digits = ci_digits, p_digits = p_digits, zap_small = zap_small, ...)
}


#' @export
format.parameters_simulate <- format.parameters_model

#' @export
format.parameters_brms_meta <- format.parameters_model

#' @importFrom insight format_p format_table
#' @inheritParams print.parameters_model
#' @export
format.compare_parameters <- function(x, style = NULL, split_components = TRUE, digits = 2, ci_digits = 2, p_digits = 3, ci_width = NULL, ci_brackets = NULL, zap_small = FALSE, format = NULL, ...) {
  x$Method <- NULL

  out <- data.frame(
    Parameter = x$Parameter,
    Component = x$Component,
    stringsAsFactors = FALSE
  )

  # save model names
  models <- attributes(x)$model_names

  # save model parameters attributes
  parameters_attributes <- attributes(x)$all_attributes

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
      if (.n_unique(i$Component) == 1) i$Component <- NULL
      if (.n_unique(i$Effects) == 1) i$Effects <- NULL
      i
    })
  } else {
    formatted_table <- out
    # remove unique columns
    if (.n_unique(formatted_table$Component) == 1) formatted_table$Component <- NULL
    if (.n_unique(formatted_table$Effects) == 1) formatted_table$Effects <- NULL
    # add line with info about observations
    formatted_table <- .add_obs_row(formatted_table, parameters_attributes, style)
  }

  formatted_table
}



# output-format helper  -------------------------

# this function does the main composition of columns for the output

.format_output_style <- function(x, style, format, modelname) {
  linesep <- " "
  if (style %in% c("se", "ci")) {
    x$p_stars <- ""
  }

  if (style == "minimal") {
    ci_col <- colnames(x)[grepl(" CI$", colnames(x)) | colnames(x) == "CI"]
    param_col <- colnames(x)[1]
    x[[param_col]] <- trimws(paste0(x[[param_col]], linesep, x[[ci_col]]))
    x <- x[c(param_col, "p")]
    colnames(x) <- paste0(colnames(x), " (", modelname, ")")
  } else if (style %in% c("ci_p", "ci")) {
    ci_col <- colnames(x)[grepl(" CI$", colnames(x)) | colnames(x) == "CI"]
    param_col <- colnames(x)[1]
    x[[param_col]] <- trimws(paste0(x[[param_col]], x$p_stars, linesep, x[[ci_col]]))
    x <- x[param_col]
    colnames(x) <- modelname
  } else if (style %in% c("se_p", "se")) {
    param_col <- colnames(x)[1]
    x[[param_col]] <- trimws(paste0(x[[param_col]], x$p_stars, linesep, "(", x$SE, ")"))
    x <- x[param_col]
    colnames(x) <- modelname
  } else if (style %in% c("ci_p2")) {
    ci_col <- colnames(x)[grepl(" CI$", colnames(x)) | colnames(x) == "CI"]
    param_col <- colnames(x)[1]
    x[[param_col]] <- trimws(paste0(x[[param_col]], linesep, x[[ci_col]]))
    x <- x[c(param_col, "p")]
    colnames(x) <- paste0(colnames(x), " (", modelname, ")")
  } else if (style %in% c("se_p2")) {
    param_col <- colnames(x)[1]
    x[[param_col]] <- trimws(paste0(x[[param_col]], linesep, "(", x$SE, ")"))
    x <- x[c(param_col, "p")]
    colnames(x) <- paste0(colnames(x), " (", modelname, ")")
  }
  x[[1]][x[[1]] == "()"] <- ""
  x
}



.add_obs_row <- function(x, att, style) {
  observations <- unlist(lapply(att, function(i) {
    if (is.null(i$n_obs)) {
      NA
    } else {
      i$n_obs
    }
  }))
  weighted_observations <- unlist(lapply(att, function(i) {
    if (is.null(i$weighted_nobs)) {
      NA
    } else {
      i$weighted_nobs
    }
  }))

  # check if model had weights, and if due to missing values n of weighted
  # observations differs from "raw" observations
  if (!all(is.na(weighted_observations)) && !all(is.na(observations))) {
    if (!isTRUE(all.equal(as.vector(weighted_observations), as.vector(observations)))) {
      message("Number of weighted observations differs from number of unweighted observations.")
    }
    observations <- weighted_observations
  }

  if (!all(is.na(observations))) {
    # add empty row, as separator
    empty_row <- do.call(data.frame, as.list(rep(NA, ncol(x))))
    colnames(empty_row) <- colnames(x)
    x <- rbind(x, empty_row)
    # add observations
    steps <- (ncol(x) - 1) / length(observations)
    empty_row[[1]] <- "Observations"
    insert_at <- seq(2, ncol(x), by = steps)
    for (i in 1:length(insert_at)) {
      empty_row[[insert_at[i]]] <- observations[i]
    }
    x <- rbind(x, empty_row)
  }
  x
}





# stan models ----------------------------

#' @importFrom utils modifyList
#' @importFrom insight print_parameters format_table
#' @export
format.parameters_stan <- function(x, split_components = TRUE, select = NULL, ci_width = NULL, ci_brackets = NULL, zap_small = FALSE, format = NULL, ...) {
  cp <- attributes(x)$parameter_info
  att <- attributes(x)
  final_table <- list()

  if (!split_components || is.null(cp)) {
    NextMethod()
  } else {
    if (!is.null(select)) {
      if (is.numeric(select)) select <- colnames(x)[select]
      select <- union(select, c("Parameter", "Component", "Effects", "Response", "Subgroup"))
      to_remove <- setdiff(colnames(x), select)
      x[to_remove] <- NULL
    }

    out <- insight::print_parameters(cp, x, keep_parameter_column = FALSE)

    final_table <- lapply(out, function(i) {
      if (identical(format, "markdown")) {
        attr(i, "table_caption") <- attributes(i)$main_title
      }
      attributes(i) <- utils::modifyList(att, attributes(i))
      param_table <- insight::format_table(i, ci_width = ci_width, ci_brackets = ci_brackets, zap_small = zap_small, preserve_attributes = TRUE)
      param_table$Group <- NULL
      param_table$Response <- NULL
      param_table$Function <- NULL
      param_table
    })
  }

  .compact_list(final_table)
}




# sem-models ---------------------------------

#' @export
format.parameters_sem <- function(x, digits = 2, ci_digits = 2, p_digits = 3, format = NULL, ci_width = NULL, ci_brackets = TRUE, ...) {
  if (missing(digits)) digits <- .additional_arguments(x, "digits", 2)
  if (missing(ci_digits)) ci_digits <- .additional_arguments(x, "ci_digits", 2)
  if (missing(p_digits)) p_digits <- .additional_arguments(x, "p_digits", 3)
  .print_model_parms_components(x, pretty_names = TRUE, split_column = "Component", digits = digits, ci_digits = ci_digits, p_digits = p_digits, format = format, ci_width = ci_width, ci_brackets = ci_brackets, ...)
}




# distribution ---------------------------------

#' @export
format.parameters_distribution <- function(x, digits = 2, format = NULL, ci_width = "auto", ci_brackets = TRUE, ...) {
  if (all(c("Min", "Max") %in% names(x))) {
    x$Min <- insight::format_ci(x$Min, x$Max, ci = NULL, digits = digits, width = ci_width, brackets = ci_brackets)
    x$Max <- NULL
    colnames(x)[which(colnames(x) == "Min")] <- "Range"
  }

  if (all(c("CI_low", "CI_high") %in% names(x))) {
    x$CI_low <- insight::format_ci(x$CI_low, x$CI_high, ci = NULL, digits = digits, width = ci_width, brackets = ci_brackets)
    x$CI_high <- NULL
    ci_lvl <- attributes(x)$ci
    centrality_ci <- attributes(x)$first_centrality

    if (!is.null(centrality_ci)) {
      ci_suffix <- paste0(" (", centrality_ci, ")")
    } else {
      ci_suffix <- ""
    }

    if (!is.null(ci_lvl)) {
      colnames(x)[which(colnames(x) == "CI_low")] <- sprintf("%i%% CI%s", round(100 * ci_lvl), ci_suffix)
    } else {
      colnames(x)[which(colnames(x) == "CI_low")] <- sprintf("CI%s", ci_suffix)
    }
  }

  if ("Trimmed_Mean" %in% colnames(x)) {
    threshold <- attributes(x)$threshold
    if (is.null(threshold)) {
      trim_name <- "Trimmed"
    } else {
      trim_name <- sprintf("Trimmed (%g%%)", round(100 * threshold))
    }
    colnames(x)[which(colnames(x) == "Trimmed_Mean")] <- trim_name
  }

  if (".group" %in% colnames(x)) {
    final_table <- list()
    grps <- split(x, x[[".group"]])
    for (i in names(grps)) {
      grps[[i]][[".group"]] <- NULL
      table_caption <- NULL
      if (is.null(format) || format == "text") {
        table_caption <- c(sprintf("# %s", i), "blue")
      } else if (format == "markdown") {
        table_caption <- sprintf("%s", i)
      }
      attr(grps[[i]], "table_caption") <- table_caption
      final_table <- c(final_table, list(grps[[i]]))
    }
  } else {
    final_table <- x
  }

  final_table
}





# footer functions ------------------

.format_footer <- function(x, digits = 2, verbose = TRUE, show_sigma = FALSE, show_formula = FALSE, type = "text") {
  # prepare footer
  footer <- NULL
  type <- tolower(type)

  sigma <- attributes(x)$sigma
  p_adjust <- attributes(x)$p_adjust
  model_formula <- attributes(x)$model_formula
  anova_test <- attributes(x)$anova_test
  footer_text <- attributes(x)$footer_text

  # footer: residual standard deviation
  if (isTRUE(show_sigma)) {
    footer <- .add_footer_sigma(footer, digits, sigma, type)
  }

  # footer: p-adjustment
  if ("p" %in% colnames(x) && isTRUE(verbose)) {
    footer <- .add_footer_padjust(footer, p_adjust, type)
  }

  # footer: model formula
  if (isTRUE(show_formula)) {
    footer <- .add_footer_formula(footer, model_formula, type)
  }

  # footer: anova test
  if (!is.null(anova_test)) {
    footer <- .add_footer_anova_test(footer, anova_test, type)
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
      footer <- c(footer, text)
    }
  }
  footer
}


# footer: residual standard deviation
.add_footer_sigma <- function(footer = NULL, digits, sigma, type = "text") {
  if (!is.null(sigma)) {
    if (type == "text") {
      if (is.null(footer)) {
        fill <- "\n"
      } else {
        fill <- ""
      }
      footer <- paste0(footer, sprintf("%sResidual standard deviation: %.*f\n", fill, digits, sigma))
    } else if (type == "html") {
      footer <- c(footer, sprintf("Residual standard deviation: %.*f", digits, sigma))
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
.add_footer_formula <- function(footer = NULL, model_formula, type = "text") {
  if (!is.null(model_formula)) {
    if (type == "text") {
      if (is.null(footer)) {
        fill <- "\n"
      } else {
        fill <- ""
      }
      footer <- paste0(footer, fill, "Model: ", model_formula, "\n")
    } else if (type == "html") {
      footer <- c(footer, paste0("Model: ", model_formula))
    }
  }
  footer
}


# footer: type of uncertainty interval
.print_footer_cimethod <- function(ci_method = NULL) {
  if (!is.null(ci_method)) {
    ci_method <- switch(toupper(ci_method),
      "HDI" = "highest density intervals",
      "ETI" = "equal-tailed intervals",
      "SI" = "support intervals",
      "uncertainty intervals"
    )
    message(paste0("\nUsing ", ci_method, " as credible intervals."))
  }
}

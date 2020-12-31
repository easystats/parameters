# usual models ---------------------------------

#' @inheritParams print.parameters_model
#' @rdname display.parameters_model
#' @export
format.parameters_model <- function(x, pretty_names = TRUE, split_components = TRUE, select = NULL, digits = 2, ci_digits = 2, p_digits = 3, ci_width = NULL, ci_brackets = NULL, format = NULL, ...) {
  # save attributes
  res <- attributes(x)$details
  coef_name <- attributes(x)$coefficient_name
  s_value <- attributes(x)$s_value
  m_class <- attributes(x)$model_class

  if (identical(format, "html")) {
    coef_name <- NULL
    attr(x, "coefficient_name") <- NULL
    attr(x, "zi_coefficient_name") <- NULL
  }

  # remove method for htest
  if (!is.null(m_class) && any(m_class %in% c("htest", "t1way", "yuen", "PMCMR", "osrt", "trendPMCMR"))) {
    x$Method <- NULL
  }

  # Special print for mcp from WRS2
  if (!is.null(m_class) && any(m_class %in% c("mcp1", "mcp2"))) {
    x$Group1 <-  paste(x$Group1, x$Group2, sep = " vs. ")
    x$Group2 <- NULL
    colnames(x)[1] <- "Group"
  }

  # prepare output, to have in shape for printing
  x <- .prepare_x_for_print(x, select, coef_name, s_value)

  # check whether to split table by certain factors/columns (like component, response...)
  split_by <- .prepare_splitby_for_print(x)

  # print everything now...
  if (split_components && !is.null(split_by) && length(split_by)) {
    formatted_table <- .print_model_parms_components(x, pretty_names, split_column = split_by, digits = digits, ci_digits = ci_digits, p_digits = p_digits, coef_column = coef_name, format = format, ci_width = ci_width, ci_brackets = ci_brackets, ...)
  } else {
    formatted_table <- insight::format_table(x, pretty_names = pretty_names, digits = digits, ci_width = ci_width, ci_brackets = ci_brackets, ci_digits = ci_digits, p_digits = p_digits, ...)
  }

  # remove unique columns
  if (.n_unique(formatted_table$Component) == 1) formatted_table$Component <- NULL
  if (.n_unique(formatted_table$Effects) == 1) formatted_table$Effects <- NULL

  formatted_table
}

#' @export
format.parameters_simulate <- format.parameters_model

#' @export
format.parameters_brms_meta <- format.parameters_model



# stan models ----------------------------

#' @importFrom utils modifyList
#' @importFrom insight print_parameters format_table
#' @export
format.parameters_stan <- function(x, split_components = TRUE, select = NULL, ci_width = NULL, ci_brackets = NULL, format = NULL, ...) {
  cp <- attributes(x)$parameter_info
  att <- attributes(x)
  final_table <- list()

  # round ESS
  if (!is.null(x$ESS)) {
    x$ESS <- round(x$ESS)
  }

  if (!split_components || is.null(cp)) {
    NextMethod()
  } else {
    if (!is.null(select)) {
      if (is.numeric(select)) select <- colnames(x)[select]
      select <- union(select, c("Parameter", "Component", "Effects", "Response", "Subgroup", "Function"))
      to_remove <- setdiff(colnames(x), select)
      x[to_remove] <- NULL
    }

    out <- insight::print_parameters(cp, x, keep_parameter_column = FALSE)

    final_table <- lapply(out, function(i) {
      if (identical(format, "markdown")) {
        attr(i, "table_caption") <- attributes(i)$main_title
      }
      attributes(i) <- utils::modifyList(att, attributes(i))
      param_table <- insight::format_table(i, ci_width = ci_width, ci_brackets = ci_brackets, preserve_attributes = TRUE)
      param_table$Group <- NULL
      param_table$Response <- NULL
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

  .print_model_parms_components(x, pretty_names = TRUE, split_column = "Type", digits = digits, ci_digits = ci_digits, p_digits = p_digits, format = format, ci_width = ci_width, ci_brackets = ci_brackets, ...)
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

  p_adjust <- attributes(x)$p_adjust
  model_formula <- attributes(x)$model_formula
  anova_test <- attributes(x)$anova_test

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

  # add color code, if we have a footer
  if (!is.null(footer) && type == "text") {
    footer <- c(footer, "blue")
  }

  footer
}


# footer: residual standard deviation
.add_footer_sigma <- function(footer = NULL, digits, sigma, type = "text") {
  if (!is.null(sigma)) {
    if (type == "text") {
      footer <- paste0(footer, sprintf("\nResidual standard deviation: %.*f", digits, sigma))
    } else if (type == "html") {
      footer <- c(footer, sprintf("Residual standard deviation: %.*f", digits, sigma))
    }
  }
  footer
}


# footer: residual standard deviation
.add_footer_anova_test <- function(footer = NULL, test, type = "text") {
  if (!is.null(test)) {
    if (type == "text") {
      footer <- paste0(footer, sprintf("\n%s test statistic", test))
    } else if (type == "html") {
      footer <- c(footer, sprintf("\n%s test statistic", test))
    }
  }
  footer
}


# footer: p-adjustment
.add_footer_padjust <- function(footer = NULL, p_adjust, type = "text") {
  if (!is.null(p_adjust) && p_adjust != "none") {
    if (type == "text") {
      footer <- paste0(footer, "\np-value adjustment method: ", format_p_adjust(p_adjust))
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
      footer <- paste0(footer, "\nModel: ", model_formula)
    } else if (type == "html") {
      footer <- c(footer, paste0("Model: ", model_formula))
    }
  }
  footer
}


# footer: type of uncertainty interval
.print_footer_cimethod <- function(ci_method = NULL) {
  if (!is.null(ci_method)) {
    ci_method <- switch(
      toupper(ci_method),
      "HDI" = "highest density intervals",
      "ETI" = "equal-tailed intervals",
      "SI" = "support intervals",
      "uncertainty intervals"
    )
    message(paste0("\nUsing ", ci_method, " as credible intervals."))
  }
}

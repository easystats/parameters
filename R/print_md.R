# normal print ----------------------------

#' @rdname print.parameters_model
#' @export
print_md.parameters_model <- function(x,
                                      pretty_names = TRUE,
                                      split_components = TRUE,
                                      select = NULL,
                                      caption = NULL,
                                      subtitle = NULL,
                                      footer = NULL,
                                      align = NULL,
                                      digits = 2,
                                      ci_digits = digits,
                                      p_digits = 3,
                                      footer_digits = 3,
                                      ci_brackets = c("(", ")"),
                                      show_sigma = FALSE,
                                      show_formula = FALSE,
                                      zap_small = FALSE,
                                      groups = NULL,
                                      include_reference = FALSE,
                                      verbose = TRUE,
                                      ...) {
  # check if user supplied digits attributes
  if (missing(digits)) {
    digits <- .additional_arguments(x, "digits", digits)
  }
  if (missing(ci_digits)) {
    ci_digits <- .additional_arguments(x, "ci_digits", digits)
  }
  if (missing(p_digits)) {
    p_digits <- .additional_arguments(x, "p_digits", p_digits)
  }
  if (missing(footer_digits)) {
    footer_digits <- .additional_arguments(x, "footer_digits", footer_digits)
  }

  # check options ---------------

  # check if pretty names should be replaced by value labels
  # (if we have labelled data)
  if (isTRUE(getOption("parameters_labels", FALSE)) || identical(pretty_names, "labels")) {
    attr(x, "pretty_names") <- attr(x, "pretty_labels", exact = TRUE)
    pretty_names <- TRUE
  }

  # select which columns to print
  if (is.null(select)) {
    select <- getOption("parameters_select")
  }

  # table caption
  table_caption <- .print_caption(x, caption, format = "markdown")

  # main table
  formatted_table <- format(
    x = x,
    pretty_names = pretty_names,
    split_components = split_components,
    select = select,
    digits = digits,
    ci_digits = ci_digits,
    p_digits = p_digits,
    zap_small = zap_small,
    ci_width = NULL,
    ci_brackets = ci_brackets,
    format = "markdown",
    groups = groups,
    include_reference = include_reference,
    ...
  )

  # replace brackets by parenthesis
  if (!is.null(ci_brackets) && "Parameter" %in% colnames(formatted_table)) {
    formatted_table$Parameter <- gsub("[", ci_brackets[1], formatted_table$Parameter, fixed = TRUE)
    formatted_table$Parameter <- gsub("]", ci_brackets[2], formatted_table$Parameter, fixed = TRUE)
  }

  # footer
  footer_stats <- .print_footer(
    x,
    digits = footer_digits,
    show_sigma = show_sigma,
    show_formula = show_formula,
    format = "markdown"
  )

  # check if footer should be printed at all. can be FALSE, or "" to suppress footer
  if (isFALSE(footer)) {
    footer <- ""
  }
  if (!identical(footer, "")) {
    if (is.null(footer)) {
      footer <- footer_stats
    } else {
      footer <- paste0("\n", footer, "\n", footer_stats)
    }
  }

  insight::export_table(
    formatted_table,
    format = "markdown",
    caption = table_caption,
    subtitle = subtitle,
    footer = footer,
    align = "firstleft",
    ...
  )
}

#' @export
print_md.parameters_brms_meta <- print_md.parameters_model

#' @export
print_md.parameters_simulate <- print_md.parameters_model


# compare parameters -------------------------


#' @rdname print.compare_parameters
#' @export
print_md.compare_parameters <- function(x,
                                        digits = 2,
                                        ci_digits = digits,
                                        p_digits = 3,
                                        caption = NULL,
                                        subtitle = NULL,
                                        footer = NULL,
                                        select = NULL,
                                        split_components = TRUE,
                                        ci_brackets = c("(", ")"),
                                        zap_small = FALSE,
                                        groups = NULL,
                                        ...) {
  # check if user supplied digits attributes
  if (missing(digits)) {
    digits <- .additional_arguments(x, "digits", digits)
  }
  if (missing(ci_digits)) {
    ci_digits <- .additional_arguments(x, "ci_digits", digits)
  }
  if (missing(p_digits)) {
    p_digits <- .additional_arguments(x, "p_digits", p_digits)
  }

  # get attributes
  if (missing(select) || is.null(select)) {
    select <- attributes(x)$output_style
  }
  if (missing(groups)) {
    groups <- attributes(x)$parameter_groups
  }

  formatted_table <- format(
    x,
    select = select,
    split_components = split_components,
    digits = digits,
    ci_digits = ci_digits,
    p_digits = p_digits,
    ci_width = NULL,
    ci_brackets = ci_brackets,
    format = "markdown",
    zap_small = zap_small,
    groups = groups
  )

  # replace brackets by parenthesis
  if (!is.null(ci_brackets) && "Parameter" %in% colnames(formatted_table)) {
    formatted_table$Parameter <- gsub("[", ci_brackets[1], formatted_table$Parameter, fixed = TRUE)
    formatted_table$Parameter <- gsub("]", ci_brackets[2], formatted_table$Parameter, fixed = TRUE)
  }

  insight::export_table(
    formatted_table,
    format = "markdown",
    caption = caption,
    subtitle = subtitle,
    footer = footer
  )
}


# SEM print ----------------------------

#' @export
print_md.parameters_sem <- function(x,
                                    digits = 2,
                                    ci_digits = digits,
                                    p_digits = 3,
                                    ci_brackets = c("(", ")"),
                                    ...) {
  # check if user supplied digits attributes
  # check if user supplied digits attributes
  if (missing(digits)) {
    digits <- .additional_arguments(x, "digits", digits)
  }
  if (missing(ci_digits)) {
    ci_digits <- .additional_arguments(x, "ci_digits", digits)
  }
  if (missing(p_digits)) {
    p_digits <- .additional_arguments(x, "p_digits", p_digits)
  }

  formatted_table <- format(
    x = x,
    digits = digits,
    ci_digits,
    p_digits = p_digits,
    format = "markdown",
    ci_width = NULL,
    ci_brackets = ci_brackets,
    ...
  )

  insight::export_table(formatted_table, format = "markdown", align = "firstleft", ...)
}


# PCA / EFA / CFA ----------------------------

#' @export
print_md.parameters_efa_summary <- function(x, digits = 3, ...) {
  table_caption <- "(Explained) Variance of Components"

  if ("Parameter" %in% names(x)) {
    x$Parameter <- c("Eigenvalues", "Variance Explained", "Variance Explained (Cumulative)", "Variance Explained (Proportion)") # nolint
  } else if ("Component" %in% names(x)) {
    names(x) <- c("Component", "Eigenvalues", "Variance Explained", "Variance Explained (Cumulative)", "Variance Explained (Proportion)") # nolint
  }

  # we may have factor correlations
  fc <- attributes(x)$factor_correlations

  # if we have factor correlations, we need to add them to the table
  if (!is.null(fc)) {
    x <- list(x, fc)
    table_caption <- list(
      table_caption,
      "Factor Correlations"
    )
  }

  insight::export_table(x, digits = digits, format = "markdown", caption = table_caption, align = "firstleft")
}

#' @export
print_md.parameters_pca_summary <- print_md.parameters_efa_summary

#' @export
print_md.parameters_omega_summary <- function(x, ...) {
  out <- .print_omega_summary(x, format = "markdown")
  insight::export_table(out$tables, caption = out$captions, format = "markdown", ...)
}


#' @export
print_md.parameters_efa <- function(x,
                                    digits = 2,
                                    sort = FALSE,
                                    threshold = NULL,
                                    labels = NULL,
                                    ...) {
  # extract attributes
  if (is.null(threshold)) {
    threshold <- attributes(x)$threshold
  }
  .print_parameters_cfa_efa(
    x,
    threshold = threshold,
    sort = sort,
    format = "markdown",
    digits = digits,
    labels = labels,
    ...
  )
}

#' @export
print_md.parameters_pca <- print_md.parameters_efa

#' @export
print_md.parameters_omega <- print_md.parameters_efa


# Equivalence test ----------------------------

#' @export
print_md.equivalence_test_lm <- function(
  x,
  digits = 2,
  ci_brackets = c("(", ")"),
  zap_small = FALSE,
  ...
) {
  .print_equivalence_test_lm(
    x,
    digits = digits,
    ci_brackets = ci_brackets,
    zap_small = zap_small,
    format = "markdown",
    ...
  )
}


# p_function ----------------------------

#' @export
print_md.parameters_p_function <- function(x,
                                           digits = 2,
                                           ci_width = "auto",
                                           ci_brackets = c("(", ")"),
                                           pretty_names = TRUE,
                                           ...) {
  .print_p_function(x, digits, ci_width, ci_brackets, pretty_names, format = "markdown", ...)
}

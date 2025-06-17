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
  formatted_table <- .print_core(
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
                                        engine = "tt",
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

  # markdown engine?
  engine <- match.arg(engine, c("tt", "default"))

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
    groups = groups,
    engine = engine
  )

  # replace brackets by parenthesis
  if (!is.null(ci_brackets) && "Parameter" %in% colnames(formatted_table)) {
    formatted_table$Parameter <- gsub("[", ci_brackets[1], formatted_table$Parameter, fixed = TRUE)
    formatted_table$Parameter <- gsub("]", ci_brackets[2], formatted_table$Parameter, fixed = TRUE)
  }

  if (identical(engine, "tt")) {
    # retrieve output format - print_md() may be called from print_html()
    dots <- list(...)
    if (identical(dots$outformat, "html")) {
      outformat <- "html"
    } else {
      outformat <- "markdown"
    }
    .export_table_tt(
      x,
      formatted_table,
      groups,
      caption = caption,
      footer = footer,
      outformat = outformat
    )
  } else {
    insight::export_table(
      formatted_table,
      format = "markdown",
      caption = caption,
      subtitle = subtitle,
      footer = footer
    )
  }
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
  insight::export_table(x, digits = digits, format = "markdown", caption = table_caption, align = "firstleft")
}

#' @export
print_md.parameters_pca_summary <- print_md.parameters_efa_summary

#' @export
print_md.parameters_efa <- function(x, digits = 2, sort = FALSE, threshold = NULL, labels = NULL, ...) {
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
print_md.psych_efa <- function(x, digits = 2, sort = FALSE, threshold = NULL, labels = NULL, ...) {
  if (is.null(threshold)) {
    threshold <- attributes(x)$threshold
  }
  if (is.null(sort)) {
    sort <- attributes(x)$sort
  }
  out <- model_parameters(x, sort = sort, threshold = threshold, labels = labels, ...)
  print_md(
    out,
    digits = digits,
    sort = sort,
    threshold = threshold,
    labels = labels,
    ...
  )
}


# Equivalence test ----------------------------

#' @export
print_md.equivalence_test_lm <- function(x,
                                         digits = 2,
                                         ci_brackets = c("(", ")"),
                                         zap_small = FALSE,
                                         ...) {
  rule <- attributes(x)$rule
  rope <- attributes(x)$rope

  if (is.null(rule)) {
    table_caption <- "Test for Practical Equivalence"
  } else if (rule == "cet") {
    table_caption <- "Conditional Equivalence Testing"
  } else if (rule == "classic") {
    table_caption <- "TOST-test for Practical Equivalence"
  } else {
    table_caption <- "Test for Practical Equivalence"
  }

  if ("Component" %in% colnames(x)) {
    x <- x[x$Component %in% c("conditional", "count"), ]
  }

  formatted_table <- insight::format_table(
    x,
    pretty_names = TRUE,
    digits = digits,
    ci_width = NULL,
    ci_brackets = ci_brackets,
    zap_small = zap_small,
    ...
  )

  colnames(formatted_table)[which(colnames(formatted_table) == "Equivalence (ROPE)")] <- "H0"
  formatted_table$ROPE <- NULL

  # col_order <- c("Parameter", "H0", "% in ROPE", colnames(formatted_table)[grepl(" CI$", colnames(formatted_table))])
  # col_order <- c(col_order, setdiff(colnames(formatted_table), col_order))
  # formatted_table <- formatted_table[col_order]

  # replace brackets by parenthesis
  if (!is.null(ci_brackets) && "Parameter" %in% colnames(formatted_table)) {
    formatted_table$Parameter <- gsub("[", ci_brackets[1], formatted_table$Parameter, fixed = TRUE)
    formatted_table$Parameter <- gsub("]", ci_brackets[2], formatted_table$Parameter, fixed = TRUE)
  }

  if (!is.null(rope)) {
    names(formatted_table)[names(formatted_table) == "% in ROPE"] <- sprintf("%% in ROPE (%.*f, %.*f)", digits, rope[1], digits, rope[2]) # nolint
  }

  insight::export_table(formatted_table, format = "markdown", caption = table_caption, align = "firstleft")
}


# helper -----------------------

.export_table_tt <- function(x, formatted_table, groups, caption = NULL, footer = NULL, outformat = "markdown") {
  insight::check_if_installed("tinytable", minimum_version = "0.1.0")
  row_groups <- NULL
  # check if we have a list of tables
  if (!is.data.frame(formatted_table) && is.list(formatted_table) && length(formatted_table) > 1) {
    # sanity check - cannot combine multiple tables when we have groups
    if (!is.null(groups)) {
      insight::format_error("Cannot combine multiple tables when groups are present.")
    }
    # add table caption as group variable, and bind tables
    # we then extract row headers based on values in the group indices
    formatted_table <- lapply(formatted_table, function(i) {
      i$group <- attr(i, "table_caption")
      i
    })
    # bind tables
    formatted_table <- do.call(rbind, formatted_table)
    # find positions for sub headers
    row_groups <- as.list(which(!duplicated(formatted_table$group)))
    names(row_groups) <- formatted_table$group[unlist(row_groups)]
    # remove no longer needed group variable
    formatted_table$group <- NULL
  }
  # we need to find out which columns refer to which model, in order to
  # add a column heading for each model
  models <- attributes(x)$model_names
  col_names <- gsub("(.*) \\((.*)\\)$", "\\2", colnames(formatted_table))
  col_groups <- sapply(models, function(i) which(i == col_names), simplify = FALSE)
  # clean column names. These still contain the model name
  colnames(formatted_table) <- gsub("(.*) \\((.*)\\)$", "\\1", colnames(formatted_table))
  # check if we have column spans at all?
  if (all(lengths(col_groups) == 1)) {
    col_groups <- NULL
  }
  # group rows?
  if (!is.null(groups)) {
    # make sure we have numeric indices for groups
    groups <- lapply(groups, function(g) {
      if (is.character(g)) {
        # if groups were provided as parameter names, we find the row position
        # by matching the parameter name
        match(g, formatted_table$Parameter)
      } else {
        # else, we assume that the group is a row position
        g
      }
    })
    # sanity check - do all rows match a parameter?
    group_indices <- unlist(groups, use.names = FALSE)
    if (anyNA(group_indices) || any(group_indices < 1) || any(group_indices > nrow(formatted_table))) {
      insight::format_error("Some group indices do not match any parameter.")
    }
    # if row indices are not sorted, we need to resort the parameters data frame
    if (is.unsorted(unlist(groups))) {
      new_rows <- c(unlist(groups), setdiff(seq_len(nrow(formatted_table)), unlist(groups)))
      formatted_table <- formatted_table[new_rows, ]
      # we need to update indices in groups as well. Therefore, we need to convert
      # list of row indices into a vector with row indices, then subtract the
      # differences of old and new row positions, and then split that vector into
      # a list again
      groups <- stats::setNames(unlist(groups), rep(names(groups), lengths(groups)))
      groups <- groups - (unlist(groups) - sort(unlist(groups)))
      groups <- split(unname(groups), factor(names(groups), levels = unique(names(groups))))
    }
    # find matching rows for groups
    row_groups <- lapply(seq_along(groups), function(i) {
      g <- groups[[i]]
      if (is.character(g)) {
        # if groups were provided as parameter names, we find the row position
        # by matching the parameter name
        g <- match(g, formatted_table$Parameter)[1]
      } else {
        # else, we assume that the group is a row position
        g <- g[1]
      }
      g
    })
    # set element names
    names(row_groups) <- names(groups)
    if (identical(outformat, "markdown")) {
      # for markdown, format italic
      names(row_groups) <- paste0("*", names(row_groups), "*")
    }
  }
  # replace NA in formatted_table by ""
  formatted_table[is.na(formatted_table)] <- ""
  # create base table
  out <- tinytable::tt(formatted_table, notes = footer, caption = caption)
  # insert sub header rows and column spans, if we have them
  if (!(is.null(row_groups) && is.null(col_groups))) {
    out <- tinytable::group_tt(out, i = row_groups, j = col_groups)
  }
  out@output <- outformat
  out
}

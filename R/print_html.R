# normal print ----------------------------

#' @rdname print.parameters_model
#' @export
print_html.parameters_model <- function(x,
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
                                        font_size = "100%",
                                        line_padding = 4,
                                        column_labels = NULL,
                                        include_reference = FALSE,
                                        engine = "gt",
                                        verbose = TRUE,
                                        ...) {
  # which engine?
  engine <- insight::validate_argument(
    getOption("easystats_html_engine", engine),
    c("gt", "default", "tt")
  )

  # line separator - for tinytable, we have no specific line separator,
  # because the output format is context-dependent
  line_sep <- ifelse(identical(engine, "tt"), " ", "<br>")

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

  # get attributes
  if (missing(select) || is.null(select)) {
    select <- attributes(x)$output_style
  }
  if (missing(groups)) {
    groups <- attributes(x)$parameter_groups
  }

  # we need glue-like syntax right now...
  if (!is.null(select)) {
    select <- .convert_to_glue_syntax(style = select, line_sep)
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
  table_caption <- .print_caption(x, caption, format = "html")

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
    format = "html",
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
    format = "html"
  )
  if (!is.null(footer)) {
    footer <- paste0(footer, line_sep, paste(footer_stats, collapse = line_sep))
  } else if (!is.null(footer_stats)) {
    footer <- paste(footer_stats, collapse = line_sep)
  }

  out <- insight::export_table(
    formatted_table,
    format = ifelse(identical(engine, "tt"), "tt", "html"),
    caption = table_caption,
    subtitle = subtitle,
    footer = footer,
    align = align,
    ...
  )

  if (identical(engine, "tt")) {
    out
  } else {
    .add_gt_options(
      out,
      style = select,
      font_size = font_size,
      line_padding = line_padding,
      user_labels = column_labels
    )
  }
}

#' @export
print_html.parameters_brms_meta <- print_html.parameters_model

#' @export
print_html.parameters_simulate <- print_html.parameters_model

#' @export
print_html.parameters_sem <- print_html.parameters_model


#' @rdname print.compare_parameters
#' @export
print_html.compare_parameters <- function(x,
                                          caption = NULL,
                                          subtitle = NULL,
                                          footer = NULL,
                                          digits = 2,
                                          ci_digits = digits,
                                          p_digits = 3,
                                          zap_small = FALSE,
                                          groups = NULL,
                                          select = NULL,
                                          ci_brackets = c("(", ")"),
                                          font_size = "100%",
                                          line_padding = 4,
                                          column_labels = NULL,
                                          engine = "gt",
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

  # which engine?
  engine <- insight::validate_argument(
    getOption("easystats_html_engine", engine),
    c("gt", "default", "tt")
  )

  # line separator - for tinytable, we have no specific line separator,
  # because the output format is context-dependent
  line_sep <- ifelse(identical(engine, "tt"), " ", "<br>")

  # we need glue-like syntax right now...
  select <- .convert_to_glue_syntax(style = select, line_sep)

  formatted_table <- format(
    x,
    select = select,
    split_components = TRUE,
    digits = digits,
    ci_digits = ci_digits,
    p_digits = p_digits,
    ci_width = NULL,
    ci_brackets = ci_brackets,
    format = "html",
    zap_small = zap_small,
    groups = groups
  )

  # replace brackets by parenthesis
  if (!is.null(ci_brackets) && "Parameter" %in% colnames(formatted_table)) {
    formatted_table$Parameter <- gsub("[", ci_brackets[1], formatted_table$Parameter, fixed = TRUE)
    formatted_table$Parameter <- gsub("]", ci_brackets[2], formatted_table$Parameter, fixed = TRUE)
  }

  # setup grouping for tt-backend --------------------------------------------
  # --------------------------------------------------------------------------

  model_groups <- NULL
  by <- NULL

  # find columns that contain model names, which we want to group
  models <- setdiff(
    unique(gsub("(.*) \\((.*)\\)$", "\\2", colnames(formatted_table))[-1]),
    c("Component", "Effects", "Response", "Group")
  )
  # grouping only applies when we have custom column layout (with "select")
  # else, we don't need grouping
  if (any(grepl(paste0("(", models[1], ")"), colnames(formatted_table), fixed = TRUE))) {
    model_groups <- lapply(models, function(model) {
      which(endsWith(colnames(formatted_table), paste0("(", model, ")")))
    })
    names(model_groups) <- models
    if (identical(engine, "tt")) {
      # for the tt backend, we need to add the model name to the column names
      colnames(formatted_table)[-1] <- gsub(
        "(.*) \\((.*)\\)$",
        "\\1",
        colnames(formatted_table)[-1]
      )
    }
  }
  if ("Component" %in% colnames(formatted_table)) {
    by <- c(by, "Component")
  }
  if ("Effects" %in% colnames(formatted_table)) {
    by <- c(by, "Effects")
  }

  # export table ------------------------------------------------------------

  out <- insight::export_table(
    formatted_table,
    format = ifelse(identical(engine, "tt"), "tt", "html"),
    caption = caption, # TODO: get rid of NOTE
    subtitle = subtitle,
    footer = footer,
    column_groups = model_groups,
    by = by,
    ...
  )

  # setup gt-backend ---------------------------------------------------------
  # --------------------------------------------------------------------------

  if (identical(engine, "tt")) {
    out
  } else {
    .add_gt_options(
      out,
      style = select,
      font_size = font_size,
      line_padding = line_padding,
      column_names = colnames(formatted_table),
      user_labels = column_labels
    )
  }
}


# PCA / EFA / CFA ----------------------------


#' @export
print_html.parameters_efa <- function(x,
                                      digits = 2,
                                      sort = FALSE,
                                      threshold = NULL,
                                      labels = NULL,
                                      engine = "gt",
                                      ...) {
  # which engine?
  engine <- insight::validate_argument(
    getOption("easystats_html_engine", engine),
    c("gt", "default", "tt")
  )

  # extract attributes
  if (is.null(threshold)) {
    threshold <- attributes(x)$threshold
  }
  .print_parameters_cfa_efa(
    x,
    threshold = threshold,
    sort = sort,
    format = ifelse(identical(engine, "tt"), "tt", "html"),
    digits = digits,
    labels = labels,
    ...
  )
}

#' @export
print_html.parameters_pca <- print_html.parameters_efa


#' @export
print_html.parameters_efa_summary <- function(x, digits = 3, engine = "gt", ...) {
  # html engine?
  engine <- insight::validate_argument(
    getOption("easystats_html_engine", engine),
    c("gt", "default", "tt")
  )

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
    fc$Component <- "Factor Correlations"
    x$Component <- "Explained Variance"
    colnames(fc)[1] <- colnames(x)[1]
    x <- .safe(rbind(x, fc), x)
  }

  insight::export_table(
    x,
    digits = digits,
    format = ifelse(identical(engine, "tt"), "tt", "html"),
    caption = table_caption,
    align = "firstleft"
  )
}

#' @export
print_html.parameters_pca_summary <- print_html.parameters_efa_summary


# p_function ----------------------------

#' @export
print_html.parameters_p_function <- function(x,
                                             digits = 2,
                                             ci_width = "auto",
                                             ci_brackets = c("(", ")"),
                                             pretty_names = TRUE,
                                             engine = "gt",
                                             ...) {
  .print_p_function(
    x,
    digits,
    ci_width,
    ci_brackets,
    pretty_names,
    format = "html",
    engine = engine,
    ...
  )
}


# helper ------------------

.add_gt_options <- function(out,
                            style,
                            font_size = "100%",
                            line_padding = 4,
                            column_names = NULL,
                            user_labels = NULL) {
  insight::check_if_installed("gt")
  out <- gt::tab_options(out,
    table.font.size = font_size,
    data_row.padding = gt::px(line_padding)
  )
  # insert newlines
  if (!is.null(style) && grepl("<br>", style, fixed = TRUE)) {
    insight::check_if_installed("tidyselect")
    out <- gt::fmt_markdown(out, columns = tidyselect::everything())
  }
  # user defined column labels
  new_labels <- NULL
  if (!is.null(user_labels)) {
    new_labels <- c(
      colnames(out[["_data"]])[1],
      rep_len(user_labels, ncol(out[["_data"]]) - 1)
    )
    new_labels <- as.list(new_labels)
  }

  # relabel columns. The single columns still have their old labels
  # (like "Estimate (model1)", "p (model1)"), and we extracted the "model names"
  # and used them for the column spanner. Now we no longer need this suffix,
  # and remove it. In case user-defined column labels are provided, "new_labels"
  # is not NULL, so we use user labels, else we extract labels from columns.
  if (!is.null(column_names)) {
    if (is.null(new_labels)) {
      new_labels <- as.list(gsub("(.*) \\((.*)\\)$", "\\1", column_names))
    }
    names(new_labels) <- column_names
    out <- gt::cols_label(out, .list = new_labels)
    # default column label, if we have user labels
  } else if (!is.null(new_labels)) {
    names(new_labels) <- colnames(out[["_data"]])
    out <- gt::cols_label(out, .list = new_labels)
  }
  # find name of parameter column
  pcol_name <- colnames(out[["_data"]])[1]
  # check where last parameter row ends. For "compare_models()", the
  # first Parameter value after data rows is "". If this is not found,
  # simply use number of rows as last row
  last_row <- which(!nzchar(as.character(out[["_data"]][[pcol_name]]), keepNA = TRUE))[1]
  if (is.na(last_row)) {
    last_row <- nrow(out[["_data"]])
  } else {
    last_row <- last_row - 1
  }
  # add a border to the first column.
  out <- gt::tab_style(
    out,
    style = gt::cell_borders(
      sides = "right",
      style = "solid",
      color = "#d3d3d3"
    ),
    locations = gt::cells_body(
      columns = pcol_name,
      rows = 1:last_row
    )
  )
  out
}

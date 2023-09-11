# normal print ----------------------------

#' @rdname display.parameters_model
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

  # get attributes
  if (missing(select) || is.null(select)) {
    select <- attributes(x)$output_style
  }

  # we need glue-like syntax right now...
  if (!is.null(select)) {
    select <- .convert_to_glue_syntax(style = select, "<br>")
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
    format = "html",
    groups = groups,
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
    footer <- paste0(footer, "<br>", paste(footer_stats, collapse = "<br>"))
  } else if (!is.null(footer_stats)) {
    footer <- paste(footer_stats, collapse = "<br>")
  }

  out <- insight::export_table(
    formatted_table,
    format = "html",
    caption = table_caption,
    subtitle = subtitle,
    footer = footer,
    align = align,
    ...
  )

  .add_gt_options(
    out,
    style = select,
    font_size = font_size,
    line_padding = line_padding,
    user_labels = column_labels
  )
}

#' @export
print_html.parameters_brms_meta <- print_html.parameters_model

#' @export
print_html.parameters_simulate <- print_html.parameters_model

#' @export
print_html.parameters_sem <- print_html.parameters_model

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

  # we need glue-like syntax right now...
  select <- .convert_to_glue_syntax(style = select, "<br>")

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

  out <- insight::export_table(
    formatted_table,
    format = "html",
    caption = caption, # TODO: get rid of NOTE
    subtitle = subtitle,
    footer = footer,
    ...
  )

  .add_gt_options(
    out,
    style = select,
    font_size = font_size,
    line_padding = line_padding,
    # we assume that model names are at the end of each column name, in parenthesis
    original_colnames = gsub("(.*) \\((.*)\\)$", "\\2", colnames(formatted_table))[-1],
    column_names = colnames(formatted_table),
    user_labels = column_labels
  )
}



# helper ------------------

.add_gt_options <- function(out,
                            style,
                            font_size = "100%",
                            line_padding = 4,
                            original_colnames = NULL,
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
      rep(user_labels, length.out = ncol(out[["_data"]]) - 1)
    )
    new_labels <- as.list(new_labels)
  }
  # add a column span? here we have multiple columns (like estimate, CI, p, ...)
  # for each model. In this case, we want to add a column spanner, i.e. a
  # separate heading for all columns of each model.
  if (!is.null(original_colnames) && anyDuplicated(original_colnames) > 0) {
    duplicates <- original_colnames[duplicated(original_colnames)]
    for (d in duplicates) {
      # we need +1 here, because first column is parameter column
      span <- which(original_colnames == d) + 1
      # add column spanner
      out <- gt::tab_spanner(out, label = d, columns = span)
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
    }
    # default column label, if we have user labels
  } else if (!is.null(new_labels)) {
    names(new_labels) <- colnames(out[["_data"]])
    out <- gt::cols_label(out, .list = new_labels)
  }
  # check where last parameter row ends. For "compare_models()", the
  # first Parameter value after data rows is "". If this is not found,
  # simply use number of rows as last row
  last_row <- which(out[["_data"]]$Parameter == "")[1]
  if (is.na(last_row)) {
    last_row <- nrow(out[["_data"]])
  } else {
    last_row <- last_row - 1
  }
  # add a border to the first column (Parameters)
  out <- gt::tab_style(
    out,
    style = gt::cell_borders(
      sides = "right",
      style = "solid",
      color = "#d3d3d3"
    ),
    locations = gt::cells_body(
      columns = "Parameter",
      rows = 1:last_row
    )
  )
  out
}

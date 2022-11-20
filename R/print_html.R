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
                                        ci_digits = 2,
                                        p_digits = 3,
                                        footer_digits = 3,
                                        ci_brackets = c("(", ")"),
                                        show_sigma = FALSE,
                                        show_formula = FALSE,
                                        zap_small = FALSE,
                                        groups = NULL,
                                        font_size = "100%",
                                        line_padding = 4,
                                        verbose = TRUE,
                                        ...) {
  # check if user supplied digits attributes
  if (missing(digits)) {
    digits <- .additional_arguments(x, "digits", digits)
  }

  if (missing(ci_digits)) {
    ci_digits <- .additional_arguments(x, "ci_digits", ci_digits)
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
    footer <- paste0(footer, "<br/>", paste(footer_stats, collapse = "<br/>"))
  } else {
    footer <- paste(footer_stats, collapse = "<br/>")
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

  .add_gt_options(out, font_size, line_padding)
}

#' @export
print_html.parameters_brms_meta <- print_html.parameters_model

#' @export
print_html.parameters_simulate <- print_html.parameters_model

#' @export
print_html.parameters_sem <- print_html.parameters_model

#' @export
print_html.compare_parameters <- function(x,
                                          digits = 2,
                                          ci_digits = 2,
                                          p_digits = 3,
                                          caption = NULL,
                                          subtitle = NULL,
                                          footer = NULL,
                                          style = NULL,
                                          groups = NULL,
                                          zap_small = FALSE,
                                          font_size = "100%",
                                          line_padding = 4,
                                          ...) {
  # check if user supplied digits attributes
  if (missing(digits)) {
    digits <- .additional_arguments(x, "digits", digits)
  }

  if (missing(ci_digits)) {
    ci_digits <- .additional_arguments(x, "ci_digits", ci_digits)
  }

  if (missing(p_digits)) {
    p_digits <- .additional_arguments(x, "p_digits", p_digits)
  }

  # get attributes
  if (missing(style) || is.null(style)) {
    style <- attributes(x)$output_style
  }

  # we need glue-like syntax right now...
  style <- .convert_to_glue_syntax(style, "<br>")

  formatted_table <- format(
    x,
    style,
    split_components = TRUE,
    digits = digits,
    ci_digits = ci_digits,
    p_digits = p_digits,
    ci_width = NULL,
    ci_brackets = c("(", ")"),
    format = "html",
    zap_small = zap_small,
    groups = groups
  )

  # we assume that model names are at the end of each column name, in parenthesis
  model_columns <- gsub("(.*) \\((.*)\\)$", "\\2", colnames(formatted_table))[-1]

  out <- insight::export_table(
    formatted_table,
    format = "html",
    caption = caption, # TODO: get rid of NOTE
    subtitle = subtitle,
    footer = footer,
    ...
  )

  .add_gt_options(out, font_size, line_padding, model_columns, style)
}



# helper ------------------

.add_gt_options <- function(out, font_size, line_padding, model_columns, style) {
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
  # add a column span?
  if (any(duplicated(model_columns))) {
    duplicates <- model_columns[duplicated(model_columns)]
    for (d in duplicates) {
      # we need +1 here, because first column is parameter column
      span <- which(model_columns == d) + 1
      # add column spanner
      out <- gt::tab_spanner(out, label = d, columns = span)
    }
  }
  out
}

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
  footer <- .print_footer(
    x,
    digits = footer_digits,
    show_sigma = show_sigma,
    show_formula = show_formula,
    format = "html"
  )

  insight::export_table(
    formatted_table,
    format = "html",
    caption = table_caption,
    subtitle = subtitle,
    footer = footer,
    align = align,
    ...
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
                                          digits = 2,
                                          ci_digits = 2,
                                          p_digits = 3,
                                          caption = NULL,
                                          subtitle = NULL,
                                          footer = NULL,
                                          style = NULL,
                                          ...) {
  # save original input
  orig_x <- x

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

  formatted_table <- format(
    x,
    style,
    split_components = TRUE,
    digits = digits,
    ci_digits = ci_digits,
    p_digits = p_digits,
    ci_width = NULL,
    ci_brackets = c("(", ")"),
    format = "html"
  )

  insight::export_table(
    formatted_table,
    format = "html",
    caption = table_caption,
    subtitle = subtitle,
    footer = footer,
    ...
  )
}






# Reexports models ------------------------

#' @importFrom insight print_html
#' @export
insight::print_html

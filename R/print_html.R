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
                                        ci_brackets = c("(", ")"),
                                        show_sigma = FALSE,
                                        show_formula = FALSE,
                                        verbose = TRUE,
                                        ...) {
  # table caption
  if (!is.null(attributes(x)$title) && is.null(caption)) {
    table_caption <- attributes(x)$title
  } else if (!is.null(caption)) {
    table_caption <- caption
  } else {
    table_caption <- "Regression Model"
  }

  # check if user supplied digits attributes
  if (missing(digits)) digits <- .additional_arguments(x, "digits", 2)
  if (missing(ci_digits)) ci_digits <- .additional_arguments(x, "ci_digits", 2)
  if (missing(p_digits)) p_digits <- .additional_arguments(x, "p_digits", 3)

  formatted_table <- format(x, format = "html", pretty_names = pretty_names, split_components = split_components, select = select, digits = digits, ci_digits = ci_digits, p_digits = p_digits, ci_width = NULL, ci_brackets = ci_brackets)

  # replace brackets by parenthesis
  if (!is.null(ci_brackets) && "Parameter" %in% colnames(formatted_table)) {
    formatted_table$Parameter <- gsub("[", ci_brackets[1], formatted_table$Parameter, fixed = TRUE)
    formatted_table$Parameter <- gsub("]", ci_brackets[2], formatted_table$Parameter, fixed = TRUE)
  }

  if (is.null(footer)) {
    footer <- .format_footer(x, digits = digits, verbose = verbose, show_sigma = show_sigma, show_formula = show_formula, type = "html")
  }
  insight::export_table(formatted_table, format = "html", caption = table_caption, subtitle = subtitle, footer = footer, align = align, ...)
}

#' @export
print_html.parameters_brms_meta <- print_html.parameters_model

#' @export
print_html.parameters_simulate <- print_html.parameters_model

#' @export
print_html.compare_parameters <- function(x,
                                          digits = 2,
                                          ci_digits = 2,
                                          p_digits = 3,
                                          style = NULL,
                                          ...) {
  # save original input
  orig_x <- x

  # get attributes
  if (missing(style)) {
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

  insight::export_table(formatted_table, format = "html", footer = NULL)
}






# Reexports models ------------------------

#' @importFrom insight print_html
#' @export
insight::print_html

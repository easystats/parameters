#' @title Print tables in different output formats
#' @name display.parameters_model
#'
#' @description Prints tables (i.e. data frame) in different output formats.
#' `print_md()` is an alias for `display(format = "markdown")`, `print_html()`
#' is an alias for `display(format = "html")`. `print_table()` is for specific
#' use cases only, and currently only works for `compare_parameters()` objects.
#'
#' @param x An object returned by [`model_parameters()`].
#' @param object An object returned by [`model_parameters()`],[`simulate_parameters()`],
#' [`equivalence_test()`] or [`principal_components()`].
#' @param format String, indicating the output format. Can be `"markdown"`
#' or `"html"`.
#' @param align Only applies to HTML tables. May be one of `"left"`,
#' `"right"` or `"center"`.
#' @param digits,ci_digits,p_digits Number of digits for rounding or
#' significant figures. May also be `"signif"` to return significant
#' figures or `"scientific"` to return scientific notation. Control the
#' number of digits by adding the value as suffix, e.g. `digits = "scientific4"`
#' to have scientific notation with 4 decimal places, or `digits = "signif5"`
#' for 5 significant figures (see also [signif()]).
#' @param subtitle Table title (same as caption) and subtitle, as strings. If `NULL`,
#' no title or subtitle is printed, unless it is stored as attributes (`table_title`,
#' or its alias `table_caption`, and `table_subtitle`). If `x` is a list of
#' data frames, `caption` may be a list of table captions, one for each table.
#' @param font_size For HTML tables, the font size.
#' @param line_padding For HTML tables, the distance (in pixel) between lines.
#' @param column_labels Labels of columns for HTML tables. If `NULL`, automatic
#' column names are generated. See 'Examples'.
#' @param theme String, indicating the table theme. Can be one of `"default"`,
#' `"grid"`, `"striped"`, `"bootstrap"` or `"darklines"`.
#' @inheritParams print.parameters_model
#' @inheritParams insight::format_table
#' @inheritParams insight::export_table
#' @inheritParams compare_parameters
#'
#' @return If `format = "markdown"`, the return value will be a character
#' vector in markdown-table format. If `format = "html"`, an object of
#' class `gt_tbl`. For `print_table()`, an object of class `tinytable` is
#' returned.
#'
#' @details `display()` is useful when the table-output from functions,
#' which is usually printed as formatted text-table to console, should
#' be formatted for pretty table-rendering in markdown documents, or if
#' knitted from rmarkdown to PDF or Word files. See
#' [vignette](https://easystats.github.io/parameters/articles/model_parameters_formatting.html)
#' for examples.
#'
#' `print_table()` is a special function for `compare_parameters()` objects,
#' which prints the output as a formatted HTML table. It is still somewhat
#' experimental, thus, only a fixed layout-style is available at the moment
#' (columns for estimates, confidence intervals and p-values). However, it
#' is possible to include other model components, like zero-inflation, or random
#' effects in the table. See 'Examples'. An alternative is to set `engine = "tt"`
#' in `print_html()` to use the _tinytable_ package for creating HTML tables.
#'
#' @seealso [print.parameters_model()] and [print.compare_parameters()]
#'
#' @examplesIf require("gt", quietly = TRUE)
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' mp <- model_parameters(model)
#' display(mp)
#'
#' \donttest{
#' data(iris)
#' lm1 <- lm(Sepal.Length ~ Species, data = iris)
#' lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
#' lm3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
#' out <- compare_parameters(lm1, lm2, lm3)
#'
#' print_html(
#'   out,
#'   select = "{coef}{stars}|({ci})",
#'   column_labels = c("Estimate", "95% CI")
#' )
#'
#' # line break, unicode minus-sign
#' print_html(
#'   out,
#'   select = "{estimate}{stars}<br>({ci_low} \u2212 {ci_high})",
#'   column_labels = c("Est. (95% CI)")
#' )
#' }
#' @export
display.parameters_model <- function(object,
                                     format = "markdown",
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
                                     font_size = "100%",
                                     line_padding = 4,
                                     column_labels = NULL,
                                     include_reference = FALSE,
                                     verbose = TRUE,
                                     ...) {
  format <- insight::validate_argument(format, c("markdown", "html", "md", "tt"))

  fun_args <- list(
    x = object, pretty_names = pretty_names, split_components = split_components,
    select = select, digits = digits, caption = caption, subtitle = subtitle,
    footer = footer, ci_digits = ci_digits, p_digits = p_digits,
    footer_digits = footer_digits, ci_brackets = ci_brackets,
    show_sigma = show_sigma, show_formula = show_formula, zap_small = zap_small,
    include_reference = include_reference, verbose = verbose
  )

  if (format %in% c("html", "tt")) {
    fun_args <- c(
      fun_args,
      list(
        column_labels = column_labels,
        align = align,
        font_size = font_size,
        line_padding = line_padding
      )
    )
    do.call(print_html, c(fun_args, list(...)))
  } else {
    do.call(print_md, c(fun_args, list(...)))
  }
}

#' @export
display.parameters_simulate <- display.parameters_model

#' @export
display.parameters_brms_meta <- display.parameters_model


# Compare Parameters ------------------------


#' @export
display.compare_parameters <- function(object,
                                       format = "markdown",
                                       digits = 2,
                                       ci_digits = digits,
                                       p_digits = 3,
                                       select = NULL,
                                       column_labels = NULL,
                                       ci_brackets = c("(", ")"),
                                       font_size = "100%",
                                       line_padding = 4,
                                       zap_small = FALSE,
                                       ...) {
  format <- insight::validate_argument(format, c("markdown", "html", "md", "tt"))

  fun_args <- list(
    x = object,
    digits = digits,
    ci_digits = ci_digits,
    p_digits = p_digits,
    ci_brackets = ci_brackets,
    select = select,
    zap_small = zap_small
  )

  if (format %in% c("html", "tt")) {
    fun_args <- c(
      fun_args,
      list(
        column_labels = column_labels,
        align = align,
        font_size = font_size,
        line_padding = line_padding
      )
    )
    do.call(print_html, c(fun_args, list(...)))
  } else {
    do.call(print_md, c(fun_args, list(...)))
  }
}


# SEM models ------------------------


#' @rdname display.parameters_model
#' @export
display.parameters_sem <- function(object,
                                   format = "markdown",
                                   digits = 2,
                                   ci_digits = digits,
                                   p_digits = 3,
                                   ci_brackets = c("(", ")"),
                                   ...) {
  format <- insight::validate_argument(format, c("markdown", "html", "md", "tt"))

  fun_args <- list(
    x = object,
    digits = digits,
    ci_digits = ci_digits,
    p_digits = p_digits,
    ci_brackets = ci_brackets
  )

  if (format %in% c("html", "tt")) {
    do.call(print_html, c(fun_args, list(...)))
  } else {
    do.call(print_md, c(fun_args, list(...)))
  }
}


# PCA /EFA  models ------------------------


#' @rdname display.parameters_model
#' @export
display.parameters_efa_summary <- function(object, format = "markdown", digits = 3, ...) {
  format <- insight::validate_argument(format, c("markdown", "html", "md", "tt"))
  fun_args <- list(x = object, digits = digits)

  if (format %in% c("html", "tt")) {
    do.call(print_html, c(fun_args, list(...)))
  } else {
    do.call(print_md, c(fun_args, list(...)))
  }
}

#' @export
display.parameters_pca_summary <- display.parameters_efa_summary


#' @inheritParams model_parameters.principal
#' @rdname display.parameters_model
#' @export
display.parameters_efa <- function(object, format = "markdown", digits = 2, sort = FALSE, threshold = NULL, labels = NULL, ...) {
  format <- insight::validate_argument(format, c("markdown", "html", "md", "tt"))

  fun_args <- list(
    x = object,
    digits = digits,
    sort = sort,
    threshold = threshold,
    labels = labels
  )

  if (format %in% c("html", "tt")) {
    do.call(print_html, c(fun_args, list(...)))
  } else {
    do.call(print_md, c(fun_args, list(...)))
  }
}

#' @export
display.parameters_pca <- display.parameters_efa

#' @export
display.psych_efa <- display.parameters_efa


# Equivalence tests ------------------------


#' @rdname display.parameters_model
#' @export
display.equivalence_test_lm <- function(object, format = "markdown", digits = 2, ...) {
  print_md(x = object, digits = digits, ...)
}

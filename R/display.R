#' @title Print tables in different output formats
#' @name display.parameters_model
#'
#' @description Prints tables (i.e. data frame) in different output formats.
#'   `print_md()` is a alias for `display(format = "markdown")`.
#'
#' @param object An object returned by [`model_parameters()`][model_parameters],
#'   [`simulate_parameters()`][simulate_parameters],
#'   [`equivalence_test()`][equivalence_test.lm] or
#'   [`principal_components()`][principal_components].
#' @param format String, indicating the output format. Can be `"markdown"`
#'   or `"html"`.
#' @param align Only applies to HTML tables. May be one of `"left"`,
#'   `"right"` or `"center"`.
#' @param digits,ci_digits,p_digits Number of digits for rounding or
#'   significant figures. May also be `"signif"` to return significant
#'   figures or `"scientific"` to return scientific notation. Control the
#'   number of digits by adding the value as suffix, e.g. `digits = "scientific4"`
#'   to have scientific notation with 4 decimal places, or `digits = "signif5"`
#'   for 5 significant figures (see also [signif()]).
#' @inheritParams print.parameters_model
#' @inheritParams insight::format_table
#' @inheritParams insight::export_table
#'
#' @return If `format = "markdown"`, the return value will be a character
#'   vector in markdown-table format. If `format = "html"`, an object of
#'   class `gt_tbl`.
#'
#' @details `display()` is useful when the table-output from functions,
#'   which is usually printed as formatted text-table to console, should
#'   be formatted for pretty table-rendering in markdown documents, or if
#'   knitted from rmarkdown to PDF or Word files. See
#'   [vignette](https://easystats.github.io/parameters/articles/model_parameters_formatting.html)
#'   for examples.
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' mp <- model_parameters(model)
#' display(mp)
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
                                     ci_digits = 2,
                                     p_digits = 3,
                                     footer_digits = 3,
                                     ci_brackets = c("(", ")"),
                                     show_sigma = FALSE,
                                     show_formula = FALSE,
                                     zap_small = FALSE,
                                     verbose = TRUE,
                                     ...) {
  if (identical(format, "html")) {
    print_html(x = object, pretty_names = pretty_names, split_components = split_components, select = select, digits = digits, caption = caption, subtitle = subtitle, footer = footer, ci_digits = ci_digits, p_digits = p_digits, footer_digits = footer_digits, align = align, ci_brackets = ci_brackets, show_sigma = show_sigma, show_formula = show_formula, zap_small = zap_small, verbose = verbose, ...)
  } else {
    print_md(x = object, pretty_names = pretty_names, split_components = split_components, select = select, digits = digits, caption = caption, subtitle = subtitle, footer = footer, ci_digits = ci_digits, p_digits = p_digits, footer_digits = footer_digits, ci_brackets = ci_brackets, show_sigma = show_sigma, show_formula = show_formula, zap_small = zap_small, verbose = verbose, ...)
  }
}

#' @export
display.parameters_stan <- display.parameters_model

#' @export
display.parameters_simulate <- display.parameters_model

#' @export
display.parameters_brms_meta <- display.parameters_model





# Compare Parameters ------------------------


#' @export
display.compare_parameters <- function(object,
                                       format = "markdown",
                                       digits = 2,
                                       ci_digits = 2,
                                       p_digits = 3,
                                       style = NULL,
                                       ...) {
  if (identical(format, "html")) {
    print_html(x = object, digits = digits, ci_digits = ci_digits, p_digits = p_digits, style = style, ...)
  } else {
    print_md(x = object, digits = digits, ci_digits = ci_digits, p_digits = p_digits, style = style, ...)
  }
}





# SEM models ------------------------


#' @rdname display.parameters_model
#' @export
display.parameters_sem <- function(object,
                                   format = "markdown",
                                   digits = 2,
                                   ci_digits = 2,
                                   p_digits = 3,
                                   ci_brackets = c("(", ")"),
                                   ...) {
  print_md(x = object, digits = digits, ci_digits = ci_digits, p_digits = p_digits, ci_brackets = ci_brackets, ...)
}





# PCA /EFA  models ------------------------


#' @rdname display.parameters_model
#' @export
display.parameters_efa_summary <- function(object, format = "markdown", digits = 3, ...) {
  print_md(x = object, digits = digits, ...)
}

#' @export
display.parameters_pca_summary <- display.parameters_efa_summary


#' @inheritParams model_parameters.principal
#' @rdname display.parameters_model
#' @export
display.parameters_efa <- function(object, format = "markdown", digits = 2, sort = FALSE, threshold = NULL, labels = NULL, ...) {
  print_md(x = object, digits = digits, sort = sort, threshold = threshold, labels = labels, ...)
}

#' @export
display.parameters_pca <- display.parameters_efa





# Equivalence tests ------------------------


#' @rdname display.parameters_model
#' @export
display.equivalence_test_lm <- function(object, format = "markdown", digits = 2, ...) {
  print_md(x = object, digits = digits, ...)
}




# Other functions ------------------------


#' @export
display.parameters_distribution <- function(object, format = "markdown", digits = 2, ...) {
  print_md(x = object, digits = digits, ...)
}





# Reexports models ------------------------

#' @importFrom insight display
#' @export
insight::display
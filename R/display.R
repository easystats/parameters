#' @title Print tables in different output formats
#' @name display.parameters_model
#'
#' @description Prints tables (i.e. data frame) in different output formats.
#'   \code{print_md()} is a alias for \code{display(format = "markdown")}.
#'
#' @param object An object returned by \code{\link[=model_parameters]{model_parameters()}},
#'   \code{\link[=simulate_parameters]{simulate_parameters()}},
#'   \code{\link[=equivalence_test.lm]{equivalence_test()}} or
#'   \code{\link[=principal_components]{principal_components()}}.
#' @param format String, indicating the output format. Can be \code{"markdown"}
#'   or \code{"html"}.
#' @param align Only applies to HTML tables. May be one of \code{"left"},
#'   \code{"right"} or \code{"center"}.
#' @inheritParams print.parameters_model
#' @inheritParams insight::format_table
#' @inheritParams insight::export_table
#'
#' @return If \code{format = "markdown"}, the return value will be a character
#'   vector in markdown-table format. If \code{format = "html"}, an object of
#'   class \code{gt_tbl}.
#'
#' @details \code{display()} is useful when the table-output from functions,
#'   which is usually printed as formatted text-table to console, should
#'   be formatted for pretty table-rendering in markdown documents, or if
#'   knitted from rmarkdown to PDF or Word files. See
#'   \href{https://easystats.github.io/parameters/articles/model_parameters_formatting.html}{vignette}
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
                                     ci_brackets = c("(", ")"),
                                     ...) {
  if (identical(format, "html")) {
    print_html(x = object, pretty_names = pretty_names, split_components = split_components, select = select, digits = digits, caption = caption, subtitle = subtitle, footer = footer, ci_digits = ci_digits, p_digits = p_digits, align = align, ci_brackets = ci_brackets, ...)
  } else {
    print_md(x = object, pretty_names = pretty_names, split_components = split_components, select = select, digits = digits, caption = caption, subtitle = subtitle, footer = footer, ci_digits = ci_digits, p_digits = p_digits, ci_brackets = ci_brackets, ...)
  }
}

#' @export
display.parameters_simulate <- display.parameters_model

#' @export
display.parameters_brms_meta <- display.parameters_model

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




# Stan models ------------------------


#' @rdname display.parameters_model
#' @export
display.parameters_stan <- function(object, split_components = TRUE, select = NULL, format = "markdown", ...) {
  print_md(x = object, split_components = split_components, select = select, ...)
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
#' @importFrom insight export_table
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

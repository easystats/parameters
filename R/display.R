#' @title Print tables in different output formats
#' @name display.parameters_model
#'
#' @description Prints tables (i.e. data frame) in different output formats.
#'   \code{table_to_markdown()} is a alias for \code{display(format = "markdown")}.
#'
#' @param x An object returned by \code{\link[=model_parameters]{model_parameters()}},
#'   \code{\link[=simulate_parameters]{simulate_parameters()}},
#'   \code{\link[=equivalence_test.lm]{equivalence_test()}} or
#'   \code{\link[=principal_components]{principal_components()}}.
#' @param format String, indicating the output format. Currently, only
#'   \code{"markdown"} is supported.
#' @inheritParams print.parameters_model
#' @inheritParams insight::parameters_table
#'
#' @return A character vector. If \code{format = "markdown"}, the return value
#'   will be a character vector in markdown-table format.
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
display.parameters_model <- function(x, format = "markdown", pretty_names = TRUE, split_components = TRUE, select = NULL, digits = 2, ci_digits = 2, p_digits = 3, ...) {
  print_md(x = x, pretty_names = pretty_names, split_components = split_components, select = select, digits = digits, ci_digits = ci_digits, p_digits = p_digits, ...)
}

#' @export
display.parameters_simulate <- display.parameters_model

#' @export
display.parameters_brms_meta <- display.parameters_model




# Stan models ------------------------


#' @rdname display.parameters_model
#' @export
display.parameters_stan <- function(x, split_components = TRUE, select = NULL, format = "markdown", ...) {
  print_md(x = x, split_components = split_components, select = select, ...)
}




# SEM models ------------------------


#' @rdname display.parameters_model
#' @export
display.parameters_sem <- function(x, format = "markdown", digits = 2, ci_digits = 2, p_digits = 3, ...) {
  print_md(x = x, digits = digits, ci_digits = ci_digits, p_digits = p_digits, ...)
}





# PCA /EFA  models ------------------------


#' @rdname display.parameters_model
#' @importFrom insight export_table
#' @export
display.parameters_efa_summary <- function(x, format = "markdown", digits = 3, ...) {
  print_md(x, digits = digits, ...)
}

#' @export
display.parameters_pca_summary <- display.parameters_efa_summary

#' @rdname display.parameters_model
#' @export
display.parameters_efa <- function(x, format = "markdown", digits = 2, sort = FALSE, threshold = NULL, labels = NULL, ...) {
  print_md(x = x, digits = digits, sort = sort, threshold = threshold, labels = labels, ...)
}

#' @export
display.parameters_pca <- display.parameters_efa





# Equivalence tests ------------------------


#' @rdname display.parameters_model
#' @export
display.equivalence_test_lm <- function(x, format = "markdown", digits = 2, ...) {
  print_md(x = x, digits = digits, ...)
}





# Reexports models ------------------------

#' @importFrom insight display
#' @export
insight::display

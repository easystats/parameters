#' @rdname to_table.parameters_model
#' @export
table_to_markdown.parameters_model <- function(x, pretty_names = TRUE, split_components = TRUE, select = NULL, digits = 2, ci_digits = 2, p_digits = 3, ...) {
  to_table(x = x, pretty_names = pretty_names, split_components = split_components, select = select, digits = digits, ci_digits = ci_digits, p_digits = p_digits, format = "markdown", ...)
}

#' @export
table_to_markdown.parameters_brms_meta <- table_to_markdown.parameters_model

#' @export
table_to_markdown.parameters_simulate <- table_to_markdown.parameters_model

#' @export
table_to_markdown.parameters_sem <- function(x, digits = 2, ci_digits = 2, p_digits = 3, ...) {
  to_table(x = x, digits = digits, ci_digits = ci_digits, format = "markdown", ...)
}

#' @export
table_to_markdown.parameters_stan <- function(x, split_components = TRUE, select = NULL, ...) {
  to_table(x = x, split_components = split_components, select = select, format = "markdown", ...)
}

#' @export
table_to_markdown.parameters_efa_summary <- function(x, digits = 3, ...) {
  to_table(x = x, digits = digits, format = "markdown", ...)
}

#' @export
table_to_markdown.parameters_pca_summary <- table_to_markdown.parameters_efa_summary

#' @export
table_to_markdown.parameters_efa <- function(x, digits = 2, sort = FALSE, threshold = NULL, labels = NULL, ...) {
  to_table(x = x, digits = digits, threshold = threshold, labels = labels, format = "markdown", ...)
}

#' @export
table_to_markdown.parameters_pca <- table_to_markdown.parameters_efa

#' @export
table_to_markdown.equivalence_test_lm <- function(x, digits = 2, ...) {
  to_table(x = x, digits = digits, format = "markdown", ...)
}





# Reexports models ------------------------

#' @importFrom insight table_to_markdown
#' @export
insight::table_to_markdown

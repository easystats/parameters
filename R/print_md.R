#' @rdname display.parameters_model
#' @export
print_md.parameters_model <- function(x, pretty_names = TRUE, split_components = TRUE, select = NULL, digits = 2, ci_digits = 2, p_digits = 3, ...) {
  # table caption
  res <- attributes(x)$details
  if (!is.null(attributes(x)$title)) {
    table_caption <- attributes(x)$title
  } else if (!is.null(res)) {
    table_caption <- "Fixed Effects"
  } else {
    table_caption <- NULL
  }

  formatted_table <- format(x, format = "markdown", pretty_names = pretty_names, split_components = split_components, select = select, digits = digits, ci_digits = ci_digits, p_digits = p_digits, ci_width = NULL, ci_brackets = c("(", ")"))

  # replace brackets by parenthesis
  formatted_table$Parameter <- gsub("[", "(", formatted_table$Parameter, fixed = TRUE)
  formatted_table$Parameter <- gsub("]", ")", formatted_table$Parameter, fixed = TRUE)

  insight::export_table(formatted_table, format = "markdown", caption = table_caption, align = "firstleft", ...)
}

#' @export
print_md.parameters_brms_meta <- print_md.parameters_model

#' @export
print_md.parameters_simulate <- print_md.parameters_model

#' @export
print_md.parameters_sem <- function(x, digits = 2, ci_digits = 2, p_digits = 3, ...) {
  formatted_table <- format(x = x, digits = digits, ci_digits, p_digits = p_digits, format = "markdown",  ci_width = NULL, ci_brackets = c("(", ")"), ...)
  insight::export_table(formatted_table, format = "markdown", align = "firstleft", ...)
}

#' @export
print_md.parameters_stan <- function(x, split_components = TRUE, select = NULL, ...) {
  formatted_table <- format(split_components = split_components, select = select, format = "markdown",  ci_width = NULL, ci_brackets = c("(", ")"), ...)
  insight::export_table(formatted_table, format = "markdown")
}

#' @export
print_md.parameters_efa_summary <- function(x, digits = 3, ...) {
  table_caption <- "(Explained) Variance of Components"

  if ("Parameter" %in% names(x)) {
    x$Parameter <- c("Eigenvalues", "Variance Explained", "Variance Explained (Cumulative)", "Variance Explained (Proportion)")
  } else if ("Component" %in% names(x)) {
    names(x) <- c("Component", "Eigenvalues", "Variance Explained", "Variance Explained (Cumulative)", "Variance Explained (Proportion)")
  }
  insight::export_table(x, digits = digits, format = "markdown", caption = table_caption, align = "firstleft")
}

#' @export
print_md.parameters_pca_summary <- print_md.parameters_efa_summary

#' @export
print_md.parameters_efa <- function(x, digits = 2, sort = FALSE, threshold = NULL, labels = NULL, ...) {
  .print_parameters_cfa_efa(x, threshold = threshold, sort = sort, format = "markdown", digits = digits, labels = labels, ...)
}

#' @export
print_md.parameters_pca <- print_md.parameters_efa

#' @export
print_md.equivalence_test_lm <- function(x, digits = 2, ...) {
  display(x = x, digits = digits, format = "markdown", ...)
}





# Reexports models ------------------------

#' @importFrom insight print_md
#' @export
insight::print_md

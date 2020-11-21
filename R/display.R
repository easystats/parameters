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
  rule <- attributes(x)$rule
  rope <- attributes(x)$rope

  if (!is.null(rule)) {
    if (rule == "cet") {
      table_caption <- "Conditional Equivalence Testing"
    } else if (rule == "classic") {
      table_caption <- "TOST-test for Practical Equivalence"
    } else {
      table_caption <- "Test for Practical Equivalence"
    }
  } else {
    table_caption <- "Test for Practical Equivalence"
  }

  if ("Component" %in% colnames(x)) {
    x <- x[x$Component %in% c("conditional", "count"), ]
  }

  formatted_table <- insight::parameters_table(x, pretty_names = TRUE, digits = digits, ci_width = NULL, ci_brackets = c("(", ")"), ...)

  colnames(formatted_table)[which(colnames(formatted_table) == "ROPE_Equivalence")] <- "H0"
  formatted_table$ROPE_low <- NULL
  formatted_table$ROPE_high <- NULL

  col_order <- c("Parameter", "H0", "% in ROPE", colnames(formatted_table)[grepl(" CI$", colnames(formatted_table))])
  col_order <- c(col_order, setdiff(colnames(formatted_table), col_order))
  formatted_table <- formatted_table[col_order]

  # replace brackets by parenthesis
  formatted_table$Parameter <- gsub("[", "(", formatted_table$Parameter, fixed = TRUE)
  formatted_table$Parameter <- gsub("]", ")", formatted_table$Parameter, fixed = TRUE)

  if (!is.null(rope)) {
    names(formatted_table)[names(formatted_table) == "% in ROPE"] <- sprintf("%% in ROPE (%.*f, %.*f)", digits, rope[1], digits, rope[2])
  }

  insight::export_table(formatted_table, format = format, caption = table_caption, align = "firstleft")
}





# Reexports models ------------------------

#' @importFrom insight display
#' @export
insight::display

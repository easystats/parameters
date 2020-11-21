#' @title Export tables into different output formats
#' @name to_table.parameters_model
#'
#' @description Export tables (i.e. data frame) into different output formats.
#'   \code{table_to_markdown()} is a alias for \code{to_table(format = "markdown")}.
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
#' @details \code{to_table()} is useful when the table-output from functions,
#'   which is usually printed as formatted text-table to console, should
#'   be formatted for pretty table-rendering in markdown documents, or if
#'   knitted from rmarkdown to PDF or Word files. See
#'   \href{https://easystats.github.io/parameters/articles/model_parameters_formatting.html}{vignette}
#'   for examples.
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' mp <- model_parameters(model)
#' to_table(mp)
#' @export
to_table.parameters_model <- function(x, format = "markdown", pretty_names = TRUE, split_components = TRUE, select = NULL, digits = 2, ci_digits = 2, p_digits = 3, ...) {
  # save original input
  orig_x <- x

  # save attributes
  res <- attributes(x)$details
  coef_name <- attributes(x)$coefficient_name
  sigma <- attributes(x)$sigma
  s_value <- attributes(x)$s_value
  p_adjust <- attributes(x)$p_adjust
  model_formula <- attributes(x)$model_formula
  ci_method <- .additional_arguments(x, "bayes_ci_method", NULL)

  # check if user supplied digits attributes
  if (missing(digits)) digits <- .additional_arguments(x, "digits", 2)
  if (missing(ci_digits)) ci_digits <- .additional_arguments(x, "ci_digits", 2)
  if (missing(p_digits)) p_digits <- .additional_arguments(x, "p_digits", 3)

  # table caption
  if (!is.null(attributes(x)$title)) {
    table_caption <- attributes(x)$title
  } else if (!is.null(res)) {
    table_caption <- "Fixed Effects"
  } else {
    table_caption <- NULL
  }

  # prepare output, to have in shape for printing
  x <- .prepare_x_for_print(x, select, coef_name, s_value)

  # check whether to split table by certain factors/columns (like component, response...)
  split_by <- .prepare_splitby_for_print(x)


  # generate final markdown table
  if (split_components && !is.null(split_by) && length(split_by)) {
    formatted_table <- .print_model_parms_components(x, pretty_names, split_column = split_by, digits = digits, ci_digits = ci_digits, p_digits = p_digits, coef_column = coef_name, format = "markdown", ci_width = NULL, ci_brackets = c("(", ")"), ...)
    attr(formatted_table, "format") <- "pipe"
    class(formatted_table) <- c("knitr_kable", "character")
    formatted_table
  } else {
    formatted_table <- insight::parameters_table(x, pretty_names = pretty_names, digits = digits, ci_width = NULL, ci_brackets = c("(", ")"), ci_digits = ci_digits, p_digits = p_digits, ...)
    # replace brackets by parenthesis
    formatted_table$Parameter <- gsub("[", "(", formatted_table$Parameter, fixed = TRUE)
    formatted_table$Parameter <- gsub("]", ")", formatted_table$Parameter, fixed = TRUE)
    insight::export_table(formatted_table, format = format, caption = table_caption, align = "firstleft")
  }
}

#' @export
to_table.parameters_simulate <- to_table.parameters_model

#' @export
to_table.parameters_brms_meta <- to_table.parameters_model





# SEM models ------------------------


#' @rdname to_table.parameters_model
#' @export
to_table.parameters_sem <- function(x, format = "markdown", digits = 2, ci_digits = 2, p_digits = 3, ...) {
  # check if user supplied digits attributes
  if (missing(digits)) digits <- .additional_arguments(x, "digits", 2)
  if (missing(ci_digits)) ci_digits <- .additional_arguments(x, "ci_digits", 2)
  if (missing(p_digits)) p_digits <- .additional_arguments(x, "p_digits", 3)

  formatted_table <- .print_model_parms_components(x, pretty_names = TRUE, split_column = "Type", digits = digits, ci_digits = ci_digits, p_digits = p_digits, format = "markdown", ci_width = NULL, ci_brackets = c("(", ")"), ...)
  attr(formatted_table, "format") <- "pipe"
  class(formatted_table) <- c("knitr_kable", "character")
  formatted_table
}





# Stan models ------------------------


#' @rdname to_table.parameters_model
#' @importFrom insight print_parameters
#' @export
to_table.parameters_stan <- function(x, split_components = TRUE, select = NULL, ...) {
  orig_x <- x
  cp <- attributes(x)$parameter_info

  # round ESS
  if (!is.null(x$ESS)) {
    x$ESS <- round(x$ESS)
  }

  # check if user supplied digits attributes
  ci <- .additional_arguments(x, "ci", .95)
  ci_method <- .additional_arguments(x, "bayes_ci_method", NULL)
  digits <- .additional_arguments(x, "digits", 2)
  ci_digits <- .additional_arguments(x, "ci_digits", 2)
  p_digits <- .additional_arguments(x, "p_digits", 3)

  if (!split_components || is.null(cp)) {
    NextMethod()
  } else {
    if (!is.null(select)) {
      if (is.numeric(select)) select <- colnames(x)[select]
      select <- union(select, c("Parameter", "Component", "Effects", "Response", "Subgroup", "Function"))
      to_remove <- setdiff(colnames(x), select)
      x[to_remove] <- NULL
    }

    final_table <- NULL
    out <- insight::print_parameters(cp, x)

    for (i in out) {
      table_caption <- paste0(attr(i, "main_title"), gsub("  ", " ", attr(i, "sub_title"), fixed = TRUE))
      rem <- which(colnames(i) %in% c("Parameter", "Component", "Effects", "Group", "Response", "Subgroup", "Function"))
      i <- i[, -rem]

      colnames(i)[1] <- "Parameter"
      attr(i, "ci") <- ci
      attr(i, "digits") <- digits
      attr(i, "ci_digits") <- ci_digits
      attr(i, "p_digits") <- p_digits

      formatted_table <- insight::parameters_table(i, pretty_names = FALSE, ci_width = NULL, ci_brackets = c("(", ")"), ...)
      # replace brackets by parenthesis
      formatted_table$Parameter <- gsub("[", "(", formatted_table$Parameter, fixed = TRUE)
      formatted_table$Parameter <- gsub("]", ")", formatted_table$Parameter, fixed = TRUE)

      final_table <- c(
        final_table,
        insight::export_table(formatted_table, format = format, caption = table_caption, align = "firstleft")
      )
    }
    attr(final_table, "format") <- "pipe"
    class(final_table) <- c("knitr_kable", "character")
    final_table
  }
}





# PCA /EFA  models ------------------------


#' @rdname to_table.parameters_model
#' @importFrom insight export_table
#' @export
to_table.parameters_efa_summary <- function(x, format = "markdown", digits = 3, ...) {
  table_caption <- "(Explained) Variance of Components"

  if ("Parameter" %in% names(x)) {
    x$Parameter <- c("Eigenvalues", "Variance Explained", "Variance Explained (Cumulative)", "Variance Explained (Proportion)")
  } else if ("Component" %in% names(x)) {
    names(x) <- c("Component", "Eigenvalues", "Variance Explained", "Variance Explained (Cumulative)", "Variance Explained (Proportion)")
  }

  insight::export_table(x, digits = digits, format = format, caption = table_caption, align = "firstleft")
}

#' @export
to_table.parameters_pca_summary <- to_table.parameters_efa_summary

#' @rdname to_table.parameters_model
#' @export
to_table.parameters_efa <- function(x, format = "markdown", digits = 2, sort = FALSE, threshold = NULL, labels = NULL, ...) {
  if (inherits(x, "parameters_pca")) {
    method <- "Principal Component Analysis"
  } else {
    method <- "Factor Analysis"
  }

  # Labels
  if (!is.null(labels)) {
    x$Label <- labels
    x <- x[c("Variable", "Label", names(x)[!names(x) %in% c("Variable", "Label")])]
  }

  # Sorting
  if (isTRUE(sort)) {
    x <- .sort_loadings(x)
  }

  # Replace by NA all cells below threshold
  if (!is.null(threshold)) {
    x <- .filter_loadings(x, threshold = threshold)
  }


  rotation_name <- attr(x, "rotation", exact = TRUE)

  if (is.null(rotation_name) || rotation_name == "none") {
    table_caption <- sprintf("Loadings from %s (no rotation)", method)
  } else {
    table_caption <- sprintf("Rotated loadings from %s (%s-rotation)", method, rotation_name)
  }

  if (!is.null(attributes(x)$type)) {
    footer <- .text_components_variance(x)
  } else {
    footer <- NULL
  }

  insight::export_table(x, digits = digits, format = format, caption = table_caption, align = "firstleft", footer = footer, ...)
}

#' @export
to_table.parameters_pca <- to_table.parameters_efa





# Equivalence tests ------------------------


#' @rdname to_table.parameters_model
#' @export
to_table.equivalence_test_lm <- function(x, format = "markdown", digits = 2, ...) {
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

#' @importFrom insight to_table
#' @export
insight::to_table

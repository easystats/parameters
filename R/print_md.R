# normal print ----------------------------

#' @rdname display.parameters_model
#' @export
print_md.parameters_model <- function(x,
                                      pretty_names = TRUE,
                                      split_components = TRUE,
                                      select = NULL,
                                      digits = 2,
                                      ci_digits = 2,
                                      p_digits = 3,
                                      ...) {
  # table caption
  res <- attributes(x)$details
  if (!is.null(attributes(x)$title)) {
    table_caption <- attributes(x)$title
  } else if (!is.null(res)) {
    table_caption <- "Fixed Effects"
  } else {
    table_caption <- NULL
  }

  # check if user supplied digits attributes
  if (missing(digits)) digits <- .additional_arguments(x, "digits", 2)
  if (missing(ci_digits)) ci_digits <- .additional_arguments(x, "ci_digits", 2)
  if (missing(p_digits)) p_digits <- .additional_arguments(x, "p_digits", 3)


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





# Stan / SEM print ----------------------------

#' @export
print_md.parameters_sem <- function(x, digits = 2, ci_digits = 2, p_digits = 3, ...) {
  # check if user supplied digits attributes
  if (missing(digits)) digits <- .additional_arguments(x, "digits", 2)
  if (missing(ci_digits)) ci_digits <- .additional_arguments(x, "ci_digits", 2)
  if (missing(p_digits)) p_digits <- .additional_arguments(x, "p_digits", 3)

  formatted_table <- format(x = x, digits = digits, ci_digits, p_digits = p_digits, format = "markdown", ci_width = NULL, ci_brackets = c("(", ")"), ...)
  insight::export_table(formatted_table, format = "markdown", align = "firstleft", ...)
}

#' @export
print_md.parameters_stan <- function(x, split_components = TRUE, select = NULL, digits = 2, ci_digits = 2, p_digits = 3, ...) {
  # check if user supplied digits attributes
  if (missing(digits)) digits <- .additional_arguments(x, "digits", 2)
  if (missing(ci_digits)) ci_digits <- .additional_arguments(x, "ci_digits", 2)
  if (missing(p_digits)) p_digits <- .additional_arguments(x, "p_digits", 3)

  formatted_table <- format(x = x, split_components = split_components, select = select, format = "markdown", digits = digits, ci_digits, p_digits = p_digits, ci_width = NULL, ci_brackets = c("(", ")"), ...)
  insight::export_table(formatted_table, format = "markdown")
}





# PCA / EFA / CFA ----------------------------

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





# Equivalence test ----------------------------

#' @export
print_md.equivalence_test_lm <- function(x, digits = 2, ...) {
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

  insight::export_table(formatted_table, format = "markdown", caption = table_caption, align = "firstleft")
}





# distribution print ----------------------------

#' @export
print_md.parameters_distribution <- function(x, digits = 2, ...) {
  formatted_table <- format(x = x, digits = digits, format = "markdown",  ci_width = NULL, ci_brackets = c("(", ")"), ...)
  insight::export_table(formatted_table, format = "markdown", align = "firstleft", ...)
}





# Reexports models ------------------------

#' @importFrom insight print_md
#' @export
insight::print_md

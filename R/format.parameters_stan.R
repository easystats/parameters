#' @importFrom insight print_parameters parameters_table
#' @export
format.parameters_stan <- function(x, split_components = TRUE, select = NULL, format = NULL, ...) {
  cp <- attributes(x)$parameter_info
  final_table <- list()

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

    out <- insight::print_parameters(cp, x)

    for (i in out) {
      rem <- which(colnames(i) %in% c("Parameter", "Component", "Effects", "Group", "Response", "Subgroup", "Function"))
      i <- i[, -rem]

      colnames(i)[1] <- "Parameter"
      attr(i, "ci") <- ci
      attr(i, "digits") <- digits
      attr(i, "ci_digits") <- ci_digits
      attr(i, "p_digits") <- p_digits

      formatted_table <- insight::parameters_table(i, pretty_names = FALSE, ...)

      if (is.null(format) || format == "text") {
        table_caption <- c(paste0("# ", attr(i, "main_title")), "blue")
        subtitle <- c(trimws(gsub("  ", " ", attr(i, "sub_title"), fixed = TRUE)), "red")
      } else {
        table_caption <- c(paste0("# ", attr(i, "main_title")), "blue")
        subtitle <- c(trimws(gsub("  ", " ", attr(i, "sub_title"), fixed = TRUE)), "red")
        # replace brackets by parenthesis
        formatted_table$Parameter <- gsub("[", "(", formatted_table$Parameter, fixed = TRUE)
        formatted_table$Parameter <- gsub("]", ")", formatted_table$Parameter, fixed = TRUE)
      }

      attr(formatted_table, "table_caption") <- table_caption
      attr(formatted_table, "table_subtitle") <- subtitle
      final_table <- c(final_table, list(formatted_table))
    }
  }

  .compact_list(final_table)
}

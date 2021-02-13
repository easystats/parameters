#' @export
print.compare_parameters <- function(x,
                                     digits = 2,
                                     ci_digits = 2,
                                     p_digits = 3,
                                     ...) {
  # save original input
  orig_x <- x

  # get attributes
  style <- attributes(x)$output_style

  formatted_table <- format(
    x,
    style,
    split_components = TRUE,
    digits = digits,
    ci_digits = ci_digits,
    p_digits = p_digits,
    ci_width = "auto",
    ci_brackets = c("(", ")"),
    format = "text"
  )

  cat(insight::export_table(formatted_table, format = "text", footer = NULL))

  invisible(orig_x)
}





#' @export
print_html.compare_parameters <- function(x,
                                          digits = 2,
                                          ci_digits = 2,
                                          p_digits = 3,
                                          ...) {
  # save original input
  orig_x <- x

  # get attributes
  style <- attributes(x)$output_style

  formatted_table <- format(
    x,
    style,
    split_components = TRUE,
    digits = digits,
    ci_digits = ci_digits,
    p_digits = p_digits,
    ci_width = "auto",
    ci_brackets = c("(", ")"),
    format = "html"
  )

  insight::export_table(formatted_table, format = "html", footer = NULL)
}





#' @export
print_md.compare_parameters <- function(x,
                                        digits = 2,
                                        ci_digits = 2,
                                        p_digits = 3,
                                        ...) {
  # save original input
  orig_x <- x

  # get attributes
  style <- attributes(x)$output_style

  formatted_table <- format(
    x,
    style,
    split_components = TRUE,
    digits = digits,
    ci_digits = ci_digits,
    p_digits = p_digits,
    ci_width = "auto",
    ci_brackets = c("(", ")"),
    format = "md"
  )

  insight::export_table(formatted_table, format = "md", footer = NULL)
}




#' @importFrom insight format_p format_table
#' @inheritParams print.parameters_model
#' @export
format.compare_parameters <- function(x, style = NULL, split_components = TRUE, digits = 2, ci_digits = 2, p_digits = 3, ci_width = NULL, ci_brackets = NULL, format = NULL, ...) {
  x$Method <- NULL

  out <- data.frame(
    Parameter = x$Parameter,
    Component = x$Component,
    stringsAsFactors = FALSE
  )

  # save model names
  models <- attributes(x)$model_names

  for (i in models) {
    # each column is suffixed with ".model_name", so we extract
    # columns for each model separately here
    pattern <- paste0("\\.", i, "$")
    cols <- x[grepl(pattern, colnames(x))]
    # since we now have the columns for a single model, we clean the
    # column names (i.e. remove suffix), so we can use "format_table" function
    colnames(cols) <- gsub(pattern, "", colnames(cols))
    # save p-stars in extra column
    cols$p_stars <- insight::format_p(cols$p, stars = TRUE, stars_only = TRUE)
    cols <- insight::format_table(cols, digits = digits, ci_width = ci_width, ci_brackets = ci_brackets, ci_digits = ci_digits, p_digits = p_digits)
    out <- cbind(out, .format_output_style(cols, style, format, i))
  }

  # check whether to split table by certain factors/columns (like component, response...)
  split_by <- split_column <- .prepare_splitby_for_print(x)

  if (length(split_by) > 0) {
    # set up split-factor
    if (length(split_column) > 1) {
      split_by <- lapply(split_column, function(i) x[[i]])
    } else {
      split_by <- list(x[[split_column]])
    }
    names(split_by) <- split_column

    # make sure we have correct sorting here...
    formatted_table <- split(out, f = split_by)
    formatted_table <- lapply(formatted_table, function(i) {
      # remove unique columns
      if (.n_unique(i$Component) == 1) i$Component <- NULL
      if (.n_unique(i$Effects) == 1) i$Effects <- NULL
      i
    })
  } else {
    formatted_table <- out
    # remove unique columns
    if (.n_unique(formatted_table$Component) == 1) formatted_table$Component <- NULL
    if (.n_unique(formatted_table$Effects) == 1) formatted_table$Effects <- NULL
  }

  formatted_table
}



.format_output_style <- function(x, style, format, modelname) {
  if (style == "minimal") {
    ci_col <- colnames(x)[grepl(" CI$", colnames(x))]
    param_col <- colnames(x)[1]
    x[[param_col]] <- trimws(paste0(x[[param_col]], " ", x[[ci_col]], ""))
    x <- x[c(param_col, "p")]
    if (identical(format, "html")) {
      colnames(x) <- paste0(colnames(x), " (", modelname, ")")
    }
  } else if (style == "one_col_1") {
    ci_col <- colnames(x)[grepl(" CI$", colnames(x))]
    param_col <- colnames(x)[1]
    x[[param_col]] <- trimws(paste0(x[[param_col]], x$p_stars, " ", x[[ci_col]], ""))
    x <- x[param_col]
    colnames(x) <- modelname
    x[[1]][x[[1]] == "()"] <- ""
  } else if (style == "one_col_2") {
    param_col <- colnames(x)[1]
    x[[param_col]] <- trimws(paste0(x[[param_col]], x$p_stars, " (", x$SE, ")"))
    x <- x[param_col]
    colnames(x) <- modelname
    x[[1]][x[[1]] == "()"] <- ""
  }
  x
}

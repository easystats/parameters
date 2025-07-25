#' @title Print model parameters
#' @name print.parameters_model
#'
#' @description A `print()`-method for objects from [`model_parameters()`][model_parameters].
#'
#' @param x,object An object returned by [`model_parameters()`][model_parameters].
#' @param split_components Logical, if `TRUE` (default), For models with
#' multiple components (zero-inflation, smooth terms, ...), each component is
#' printed in a separate table. If `FALSE`, model parameters are printed
#' in a single table and a `Component` column is added to the output.
#' @param select Determines which columns and and which layout columns are
#' printed. There are three options for this argument:
#'
#' * **Selecting columns by name or index**
#'
#'   `select` can be a character vector (or numeric index) of column names that
#'   should be printed, where columns are extracted from the data frame returned
#'   by `model_parameters()` and related functions.
#'
#'   There are two pre-defined options for selecting columns:
#'   `select = "minimal"` prints coefficients, confidence intervals and
#'   p-values, while `select = "short"` prints coefficients, standard errors and
#'   p-values.
#'
#' * **A string expression with layout pattern**
#'
#'   `select` is a string with "tokens" enclosed in braces. These tokens will be
#'   replaced by their associated columns, where the selected columns will be
#'   collapsed into one column. Following tokens are replaced by the related
#'   coefficients or statistics: `{estimate}`, `{se}`, `{ci}` (or `{ci_low}` and
#'   `{ci_high}`), `{p}` and `{stars}`. The token `{ci}` will be replaced by
#'   `{ci_low}, {ci_high}`. Example: `select = "{estimate}{stars} ({ci})"`
#'
#'   It is possible to create multiple columns as well. A `|` separates values
#'   into new cells/columns. Example: `select = "{estimate} ({ci})|{p}"`.
#'
#'   If `format = "html"`, a `<br>` inserts a line break inside a cell. See
#'   'Examples'.
#'
#' * **A string indicating a pre-defined layout**
#'
#'   `select` can be one of the following string values, to create one of the
#'   following pre-defined column layouts:
#'
#'   - `"ci"`: Estimates and confidence intervals, no asterisks for p-values.
#'     This is equivalent to `select = "{estimate} ({ci})"`.
#'   - `"se"`: Estimates and standard errors, no asterisks for p-values. This is
#'     equivalent to `select = "{estimate} ({se})"`.
#'   - `"ci_p"`: Estimates, confidence intervals and asterisks for p-values. This
#'     is equivalent to `select = "{estimate}{stars} ({ci})"`.
#'   - `"se_p"`: Estimates, standard errors and asterisks for p-values. This is
#'     equivalent to `select = "{estimate}{stars} ({se})"`..
#'   - `"ci_p2"`: Estimates, confidence intervals and numeric p-values, in two
#'     columns. This is equivalent to `select = "{estimate} ({ci})|{p}"`.
#'   - `"se_p2"`: Estimate, standard errors and numeric p-values, in two columns.
#'     This is equivalent to `select = "{estimate} ({se})|{p}"`.
#'
#' For `model_parameters()`, glue-like syntax is still experimental in the
#' case of more complex models (like mixed models) and may not return expected
#' results.
#' @param show_sigma Logical, if `TRUE`, adds information about the residual
#' standard deviation.
#' @param show_formula Logical, if `TRUE`, adds the model formula to the output.
#' @param caption Table caption as string. If `NULL`, depending on the model,
#' either a default caption or no table caption is printed. Use `caption = ""`
#' to suppress the table caption.
#' @param footer Can either be `FALSE` or an empty string (i.e. `""`) to
#' suppress the footer, `NULL` to print the default footer, or a string. The
#' latter will combine the string value with the default footer.
#' @param footer_digits Number of decimal places for values in the footer summary.
#' @param groups Named list, can be used to group parameters in the printed output.
#' List elements may either be character vectors that match the name of those
#' parameters that belong to one group, or list elements can be row numbers
#' of those parameter rows that should belong to one group. The names of the
#' list elements will be used as group names, which will be inserted as "header
#' row". A possible use case might be to emphasize focal predictors and control
#' variables, see 'Examples'. Parameters will be re-ordered according to the
#' order used in `groups`, while all non-matching parameters will be added
#' to the end.
#' @param column_width Width of table columns. Can be either `NULL`, a named
#' numeric vector, or `"fixed"`. If `NULL`, the width for each table column is
#' adjusted to the minimum required width. If a named numeric vector, value
#' names are matched against column names, and for each match, the specified
#' width is used. If `"fixed"`, and table is split into multiple components,
#' columns across all table components are adjusted to have the same width.
#' @param digits,ci_digits,p_digits Number of digits for rounding or
#' significant figures. May also be `"signif"` to return significant
#' figures or `"scientific"` to return scientific notation. Control the
#' number of digits by adding the value as suffix, e.g. `digits = "scientific4"`
#' to have scientific notation with 4 decimal places, or `digits = "signif5"`
#' for 5 significant figures (see also [signif()]).
#' @param pretty_names Can be `TRUE`, which will return "pretty" (i.e. more human
#' readable) parameter names. Or `"labels"`, in which case value and variable
#' labels will be used as parameters names. The latter only works for "labelled"
#' data, i.e. if the data used to fit the model had `"label"` and `"labels"`
#' attributes. See also section _Global Options to Customize Messages when Printing_.
#' @param include_reference Logical, if `TRUE`, the reference level of factors will
#' be added to the parameters table. This is only relevant for models with
#' categorical predictors. The coefficient for the reference level is always
#' `0` (except when `exponentiate = TRUE`, then the coefficient will be `1`),
#' so this is just for completeness.
#' @param align Only applies to HTML tables. May be one of `"left"`,
#' `"right"` or `"center"`.
#' @param subtitle Table title (same as caption) and subtitle, as strings. If `NULL`,
#' no title or subtitle is printed, unless it is stored as attributes (`table_title`,
#' or its alias `table_caption`, and `table_subtitle`). If `x` is a list of
#' data frames, `caption` may be a list of table captions, one for each table.
#' @param font_size For HTML tables, the font size.
#' @param line_padding For HTML tables, the distance (in pixel) between lines.
#' @param column_labels Labels of columns for HTML tables. If `NULL`, automatic
#' column names are generated. See 'Examples'.
#' @param ... Arguments passed down to [`format.parameters_model()`],
#' [`insight::format_table()`] and [`insight::export_table()`]
#'
#' @inheritParams insight::format_table
#' @inheritParams compare_parameters
#' @inheritParams insight::export_table
#' @inheritParams display.parameters_model
#'
#' @inheritSection format_parameters Interpretation of Interaction Terms
#' @inheritSection model_parameters Labeling the Degrees of Freedom
#'
#' @section Global Options to Customize Messages and Tables when Printing:
#' The `verbose` argument can be used to display or silence messages and
#' warnings for the different functions in the **parameters** package. However,
#' some messages providing additional information can be displayed or suppressed
#' using `options()`:
#'
#' - `parameters_info`: `options(parameters_info = TRUE)` will override the
#' `include_info` argument in `model_parameters()` and always show the model
#' summary for non-mixed models.
#'
#' - `parameters_mixed_info`: `options(parameters_mixed_info = TRUE)` will
#' override the `include_info` argument in `model_parameters()` for mixed
#' models, and will then always show the model summary.
#'
#' - `parameters_cimethod`: `options(parameters_cimethod = TRUE)` will show the
#' additional information about the approximation method used to calculate
#' confidence intervals and p-values. Set to `FALSE` to hide this message when
#' printing `model_parameters()` objects.
#'
#' - `parameters_exponentiate`: `options(parameters_exponentiate = TRUE)` will
#' show the additional information on how to interpret coefficients of models
#' with log-transformed response variables or with log-/logit-links when the
#' `exponentiate` argument in `model_parameters()` is not `TRUE`. Set this option
#' to `FALSE` to hide this message when printing `model_parameters()` objects.
#'
#' There are further options that can be used to modify the default behaviour
#' for printed outputs:
#'
#' - `parameters_labels`: `options(parameters_labels = TRUE)` will use variable
#' and value labels for pretty names, if data is labelled. If no labels
#' available, default pretty names are used.
#'
#' - `parameters_interaction`: `options(parameters_interaction = <character>)`
#' will replace the interaction mark (by default, `*`) with the related character.
#'
#' - `parameters_select`: `options(parameters_select = <value>)` will set the
#' default for the `select` argument. See argument's documentation for available
#' options.
#'
#' - `easystats_table_width`: `options(easystats_table_width = <value>)` will
#' set the default width for tables in text-format, i.e. for most of the outputs
#' printed to console. If not specified, tables will be adjusted to the current
#' available width, e.g. of the of the console (or any other source for textual
#' output, like markdown files). The argument `table_width` can also be used in
#' most `print()` methods to specify the table width as desired.
#'
#' - `insight_use_symbols`: `options(insight_use_symbols = TRUE)` will try to
#' print unicode-chars for symbols as column names, wherever possible (e.g.,
#' \ifelse{html}{\out{&omega;}}{\eqn{\omega}} instead of `Omega`).
#'
#' @details `summary()` is a convenient shortcut for
#' `print(object, select = "minimal", show_sigma = TRUE, show_formula = TRUE)`.
#'
#' @return Invisibly returns the original input object.
#'
#' @seealso See also [`display()`][display.parameters_model].
#'
#' @examplesIf require("gt", quietly = TRUE) && require("glmmTMB", quietly = TRUE)
#' \donttest{
#' library(parameters)
#' model <- glmmTMB::glmmTMB(
#'   count ~ spp + mined + (1 | site),
#'   ziformula = ~mined,
#'   family = poisson(),
#'   data = Salamanders
#' )
#' mp <- model_parameters(model)
#'
#' print(mp, pretty_names = FALSE)
#'
#' print(mp, split_components = FALSE)
#'
#' print(mp, select = c("Parameter", "Coefficient", "SE"))
#'
#' print(mp, select = "minimal")
#'
#'
#' # group parameters ------
#'
#' data(iris)
#' model <- lm(
#'   Sepal.Width ~ Sepal.Length + Species + Petal.Length,
#'   data = iris
#' )
#' # don't select "Intercept" parameter
#' mp <- model_parameters(model, parameters = "^(?!\\(Intercept)")
#' groups <- list(`Focal Predictors` = c(1, 4), Controls = c(2, 3))
#' print(mp, groups = groups)
#'
#' # only show coefficients, CI and p,
#' # put non-matched parameters to the end
#'
#' data(mtcars)
#' mtcars$cyl <- as.factor(mtcars$cyl)
#' mtcars$gear <- as.factor(mtcars$gear)
#' model <- lm(mpg ~ hp + gear * vs + cyl + drat, data = mtcars)
#'
#' # don't select "Intercept" parameter
#' mp <- model_parameters(model, parameters = "^(?!\\(Intercept)")
#' print(mp, groups = list(
#'   Engine = c(5, 6, 4, 1),
#'   Interactions = c(8, 9)
#' ))
#' }
#'
#'
#' # custom column layouts ------
#'
#' data(iris)
#' lm1 <- lm(Sepal.Length ~ Species, data = iris)
#' lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
#'
#' # custom style
#' result <- compare_parameters(lm1, lm2, select = "{estimate}{stars} ({se})")
#' print(result)
#'
#' \donttest{
#' # custom style, in HTML
#' result <- compare_parameters(lm1, lm2, select = "{estimate}<br>({se})|{p}")
#' print_html(result)
#' }
#' @export
print.parameters_model <- function(x,
                                   pretty_names = TRUE,
                                   split_components = TRUE,
                                   select = NULL,
                                   caption = NULL,
                                   footer = NULL,
                                   digits = 2,
                                   ci_digits = digits,
                                   p_digits = 3,
                                   footer_digits = 3,
                                   show_sigma = FALSE,
                                   show_formula = FALSE,
                                   zap_small = FALSE,
                                   groups = NULL,
                                   column_width = NULL,
                                   ci_brackets = c("[", "]"),
                                   include_reference = FALSE,
                                   ...) {
  # save original input
  orig_x <- x

  # check options ---------------

  # check if pretty names should be replaced by value labels
  # (if we have labelled data)
  if (isTRUE(getOption("parameters_labels", FALSE)) || identical(pretty_names, "labels")) {
    attr(x, "pretty_names") <- attr(x, "pretty_labels", exact = TRUE)
    pretty_names <- TRUE
  }

  # select which columns to print
  if (is.null(select)) {
    select <- getOption("parameters_select")
  }

  # check if user supplied digits attributes
  if (missing(digits)) {
    digits <- .additional_arguments(x, "digits", digits)
  }
  if (missing(ci_digits)) {
    ci_digits <- .additional_arguments(x, "ci_digits", digits)
  }
  if (missing(p_digits)) {
    p_digits <- .additional_arguments(x, "p_digits", p_digits)
  }
  if (missing(footer_digits)) {
    footer_digits <- .additional_arguments(x, "footer_digits", footer_digits)
  }

  # table caption
  table_caption <- .print_caption(x, caption, format = "text")

  # main table
  formatted_table <- format(
    x = x,
    pretty_names = pretty_names,
    split_components = split_components,
    select = select,
    digits = digits,
    ci_digits = ci_digits,
    p_digits = p_digits,
    zap_small = zap_small,
    ci_width = "auto",
    ci_brackets = ci_brackets,
    format = "text",
    groups = groups,
    include_reference = include_reference,
    ...
  )

  # if we have multiple components, we can align colum width across components here
  if (!is.null(column_width) && all(column_width == "fixed") && is.list(formatted_table)) {
    column_width <- .find_min_colwidth(formatted_table)
  }

  # footer
  footer_stats <- .print_footer(
    x,
    digits = footer_digits,
    show_sigma = show_sigma,
    show_formula = show_formula
  )

  # check if footer should be printed at all. can be FALSE, or "" to suppress footer
  if (isFALSE(footer)) {
    footer <- ""
  }
  if (!identical(footer, "")) {
    if (is.null(footer)) {
      footer <- footer_stats
    } else {
      footer <- paste0("\n", footer, "\n", footer_stats)
    }
  }

  # get attributes
  verbose <- .additional_arguments(x, "verbose", TRUE)

  # print main table
  cat(insight::export_table(
    formatted_table,
    format = "text",
    caption = table_caption,
    footer = footer,
    width = column_width,
    ...
  ))

  # inform about CI and df approx.
  if (isTRUE(verbose)) {
    .print_footer_cimethod(x)
    .print_footer_exp(x)
  }

  invisible(orig_x)
}

#' @rdname print.parameters_model
#' @export
summary.parameters_model <- function(object, ...) {
  print(
    x = object,
    select = "minimal",
    show_sigma = TRUE,
    show_formula = TRUE,
    ...
  )
}

#' @export
print.parameters_simulate <- print.parameters_model

#' @export
print.parameters_brms_meta <- print.parameters_model


# Random effects ------------------

#' @export
print.parameters_coef <- function(x, ...) {
  cat(insight::export_table(format(x, ...), ...))
}

#' @export
print.parameters_random <- function(x, digits = 2, ...) {
  .print_random_parameters(x, digits = digits)
  invisible(x)
}


# helper --------------------

.print_footer <- function(x,
                          digits = 3,
                          show_sigma = FALSE,
                          show_formula = FALSE,
                          format = "text") {
  # get attributes
  model_sigma <- attributes(x)$sigma
  show_summary <- isTRUE(attributes(x)$show_summary)
  verbose <- .additional_arguments(x, "verbose", TRUE)

  # override defaults. if argument "summary" is called in "model_parameters()",
  # this overrides the defaults...
  show_sigma <- ifelse(show_summary, TRUE, show_sigma)
  show_formula <- ifelse(show_summary, TRUE, show_formula)
  show_r2 <- .additional_arguments(x, "show_summary", FALSE)
  show_rmse <- .additional_arguments(x, "show_summary", FALSE)

  # set defaults, if necessary
  if (is.null(model_sigma)) {
    show_sigma <- FALSE
  }

  .format_footer(
    x,
    digits = digits,
    verbose = verbose,
    show_sigma = show_sigma,
    show_formula = show_formula,
    show_r2 = show_r2,
    show_rmse = show_rmse,
    format = format
  )
}


.print_caption <- function(x, caption = NULL, format = "text") {
  no_caption <- attributes(x)$no_caption
  # no table-title for certain model tables, indicated by the no_caption attribute
  if (isTRUE(no_caption)) {
    return(NULL)
  }

  title_attribute <- attributes(x)$title[1]

  # check effects and component parts
  if (!is.null(x$Effects) && all(x$Effects == "random")) {
    eff_name <- "Random"
  } else {
    eff_name <- "Fixed"
  }
  if (!is.null(x$Component) && all(x$Component == "zero_inflated")) {
    zero_inflated <- " (Zero-Inflation Component)"
  } else {
    zero_inflated <- ""
  }

  # caption = NULL, set default for HTML tables
  if (identical(format, "html") && is.null(caption)) {
    table_caption <- "Model Summary"
  } else if (isTRUE(attributes(x)$ordinal_model)) {
    table_caption <- ""

    # caption is NULL, set default title, using title-attribute
  } else if (!is.null(title_attribute) && is.null(caption)) {
    if (length(title_attribute) == 1 && title_attribute == "") {
      table_caption <- NULL
    } else {
      table_caption <- title_attribute
    }

    # if caption is not empty, use it as title
  } else if (!is.null(caption) && caption != "") {
    table_caption <- caption

    # no table-title if caption is empty string
  } else if (!is.null(caption) && caption == "") {
    table_caption <- NULL

    # default title for sub-components of models
  } else if (identical(format, "text")) {
    table_caption <- c(paste0("# ", eff_name, " Effects", zero_inflated), "blue")
  } else {
    table_caption <- paste0(eff_name, " Effects", zero_inflated)
  }

  table_caption
}


#' @keywords internal
.print_random_parameters <- function(random_params, digits = 2) {
  insight::print_color("# Random Effects\n\n", "blue")

  # create SD
  random_params$SD <- NA
  var_components <- random_params$Description %in% c("Within-Group Variance", "Between-Group Variance")
  random_params$SD[var_components] <- sqrt(random_params$Value[var_components])

  # format values
  random_params$Value <- format(sprintf("%g", round(random_params$Value, digits = digits)), justify = "right")
  random_params$SD[var_components] <- format(
    sprintf("(%g)", round(random_params$SD[var_components], digits = digits)),
    justify = "right"
  )

  # create summary-information for each component
  random_params$Line <- ""
  random_params$Term[is.na(random_params$Term)] <- ""
  random_params$SD[is.na(random_params$SD)] <- ""

  non_empty <- random_params$Term != "" & random_params$Type != "" # nolint
  random_params$Line[non_empty] <- sprintf("%s (%s)", random_params$Type[non_empty], random_params$Term[non_empty])

  non_empty <- random_params$Term != "" & random_params$Type == "" # nolint
  random_params$Line[non_empty] <- sprintf("%s", random_params$Term[non_empty])

  # final fix, indentions
  random_params$Line <- sprintf("  %s", format(random_params$Line))
  max_len <- max(nchar(random_params$Line)) + 2

  out <- split(random_params, factor(random_params$Description, levels = unique(random_params$Description)))

  for (i in out) {
    if ("Within-Group Variance" %in% i$Description) {
      insight::print_color(format("Within-Group Variance", width = max_len), color = "blue")
      cat(sprintf("%s %s\n", i$Value, i$SD))
    } else if ("Between-Group Variance" %in% i$Description) {
      insight::print_color("Between-Group Variance\n", "blue")
      for (j in seq_len(nrow(i))) {
        cat(sprintf("%s  %s %s\n", i$Line[j], i$Value[j], i$SD[j]))
      }
    } else if ("Correlations" %in% i$Description) {
      insight::print_color("Correlations\n", "blue")
      for (j in seq_len(nrow(i))) {
        cat(sprintf("%s  %s\n", i$Line[j], i$Value[j]))
      }
    } else if ("N" %in% i$Description) {
      insight::print_color("N (groups per factor)\n", "blue")
      for (j in seq_len(nrow(i))) {
        cat(sprintf("  %s%s\n", format(i$Term[j], width = max_len - 2), i$Value[j]))
      }
    } else if ("Observations" %in% i$Description) {
      insight::print_color(format("Observations", width = max_len), color = "blue")
      cat(sprintf("%s\n", i$Value))
    }
  }
}


.find_min_colwidth <- function(formatted_table) {
  shared_cols <- unique(unlist(lapply(formatted_table, colnames)))
  col_width <- rep(NA, length(shared_cols))
  for (i in seq_along(shared_cols)) {
    col_width[i] <- max(unlist(lapply(formatted_table, function(j) {
      column <- j[[shared_cols[i]]]
      if (is.null(column)) {
        NA
      } else {
        max(nchar(column))
      }
    })))
  }
  stats::na.omit(stats::setNames(col_width, shared_cols))
}

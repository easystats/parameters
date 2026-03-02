#' @title Global options from the parameters package
#' @name parameters-options
#'
#' @section Global options to set defaults for function arguments:
#'
#' The `verbose` argument can be used to display or silence messages and
#' warnings for the different functions in the **parameters** package. However,
#' some messages providing additional information can be displayed or suppressed
#' using `options()`:
#'
#' - `options(parameters_info = TRUE)` will override the `include_info` argument
#'   in `model_parameters()` and always show the model summary for non-mixed
#'   models.
#'
#' - `options(parameters_mixed_info = TRUE)` will override the `include_info`
#'   argument in `model_parameters()` for mixed models, and will then always
#'   show the model summary.
#'
#' - `options(parameters_cimethod = TRUE)` will show the additional information
#'   about the approximation method used to calculate confidence intervals and
#'   p-values. Set to `FALSE` to hide this message when printing
#'   `model_parameters()` objects.
#'
#' - `options(parameters_exponentiate = TRUE)` will show the additional
#'   information on how to interpret coefficients of models with log-transformed
#'   response variables or with log-/logit-links when the `exponentiate`
#'   argument in `model_parameters()` is not `TRUE`. Set this option to `FALSE`
#'   to hide this message when printing `model_parameters()` objects.
#'
#' There are further options that can be used to modify the default behaviour
#' for printed outputs:
#'
#' - `options(parameters_labels = TRUE)` will use variable and value labels for
#'   pretty names, if data is labelled. If no labels available, default pretty
#'   names are used.
#'
#' - `options(parameters_interaction = <character>)` will replace the
#'   interaction mark (by default, `*`) with the related character.
#'
#' - `options(parameters_select = <value>)` will set the default for the
#'   `select` argument. See argument's documentation for available options.
#'
#' - `options(easystats_table_width = <value>)` will set the default width for
#'   tables in text-format, i.e. for most of the outputs printed to console. If
#'   not specified, tables will be adjusted to the current available width, e.g.
#'   of the of the console (or any other source for textual output, like
#'   markdown files). The argument `table_width` can also be used in most
#'   `print()` methods to specify the table width as desired.
#'
#' - `options(insight_use_symbols = TRUE)` will try to print unicode-chars for
#'   symbols as column names, wherever possible (e.g.,
#'   \ifelse{html}{\out{&omega;}}{\eqn{\omega}} instead of `Omega`).
#'
#' - `options(easystats_display_format = <value>)` will set the default format for
#'   the `display()` methods. Can be one of `"markdown"`, `"html"`, or `"tt"`.
#'   See [`display.parameters_model()`] for details.
#'
NULL

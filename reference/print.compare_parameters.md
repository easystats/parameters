# Print comparisons of model parameters

A [`print()`](https://rdrr.io/r/base/print.html)-method for objects from
[`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md).

## Usage

``` r
# S3 method for class 'compare_parameters'
format(
  x,
  split_components = TRUE,
  select = NULL,
  digits = 2,
  ci_digits = digits,
  p_digits = 3,
  ci_width = NULL,
  ci_brackets = NULL,
  zap_small = FALSE,
  format = NULL,
  groups = NULL,
  ...
)

# S3 method for class 'compare_parameters'
print(
  x,
  split_components = TRUE,
  caption = NULL,
  subtitle = NULL,
  footer = NULL,
  digits = 2,
  ci_digits = digits,
  p_digits = 3,
  zap_small = FALSE,
  groups = NULL,
  column_width = NULL,
  ci_brackets = c("[", "]"),
  select = NULL,
  ...
)

# S3 method for class 'compare_parameters'
print_html(
  x,
  caption = NULL,
  subtitle = NULL,
  footer = NULL,
  digits = 2,
  ci_digits = digits,
  p_digits = 3,
  zap_small = FALSE,
  groups = NULL,
  select = NULL,
  ci_brackets = c("(", ")"),
  font_size = "100%",
  line_padding = 4,
  column_labels = NULL,
  ...
)

# S3 method for class 'compare_parameters'
print_md(
  x,
  digits = 2,
  ci_digits = digits,
  p_digits = 3,
  caption = NULL,
  subtitle = NULL,
  footer = NULL,
  select = NULL,
  split_components = TRUE,
  ci_brackets = c("(", ")"),
  zap_small = FALSE,
  groups = NULL,
  ...
)
```

## Arguments

- x:

  An object returned by
  [`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md).

- split_components:

  Logical, if `TRUE` (default), For models with multiple components
  (zero-inflation, smooth terms, ...), each component is printed in a
  separate table. If `FALSE`, model parameters are printed in a single
  table and a `Component` column is added to the output.

- select:

  Determines which columns and and which layout columns are printed.
  There are three options for this argument:

  - **Selecting columns by name or index**

    `select` can be a character vector (or numeric index) of column
    names that should be printed, where columns are extracted from the
    data frame returned by
    [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
    and related functions.

    There are two pre-defined options for selecting columns:
    `select = "minimal"` prints coefficients, confidence intervals and
    p-values, while `select = "short"` prints coefficients, standard
    errors and p-values.

  - **A string expression with layout pattern**

    `select` is a string with "tokens" enclosed in braces. These tokens
    will be replaced by their associated columns, where the selected
    columns will be collapsed into one column. Following tokens are
    replaced by the related coefficients or statistics: `{estimate}`,
    `{se}`, `{ci}` (or `{ci_low}` and `{ci_high}`), `{p}` and `{stars}`.
    The token `{ci}` will be replaced by `{ci_low}, {ci_high}`. Example:
    `select = "{estimate}{stars} ({ci})"`

    It is possible to create multiple columns as well. A `|` separates
    values into new cells/columns. Example:
    `select = "{estimate} ({ci})|{p}"`.

    If `format = "html"`, a `<br>` inserts a line break inside a cell.
    See 'Examples'.

  - **A string indicating a pre-defined layout**

    `select` can be one of the following string values, to create one of
    the following pre-defined column layouts:

    - `"ci"`: Estimates and confidence intervals, no asterisks for
      p-values. This is equivalent to `select = "{estimate} ({ci})"`.

    - `"se"`: Estimates and standard errors, no asterisks for p-values.
      This is equivalent to `select = "{estimate} ({se})"`.

    - `"ci_p"`: Estimates, confidence intervals and asterisks for
      p-values. This is equivalent to
      `select = "{estimate}{stars} ({ci})"`.

    - `"se_p"`: Estimates, standard errors and asterisks for p-values.
      This is equivalent to `select = "{estimate}{stars} ({se})"`..

    - `"ci_p2"`: Estimates, confidence intervals and numeric p-values,
      in two columns. This is equivalent to
      `select = "{estimate} ({ci})|{p}"`.

    - `"se_p2"`: Estimate, standard errors and numeric p-values, in two
      columns. This is equivalent to `select = "{estimate} ({se})|{p}"`.

  For
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md),
  glue-like syntax is still experimental in the case of more complex
  models (like mixed models) and may not return expected results.

- digits, ci_digits, p_digits:

  Number of digits for rounding or significant figures. May also be
  `"signif"` to return significant figures or `"scientific"` to return
  scientific notation. Control the number of digits by adding the value
  as suffix, e.g. `digits = "scientific4"` to have scientific notation
  with 4 decimal places, or `digits = "signif5"` for 5 significant
  figures (see also [`signif()`](https://rdrr.io/r/base/Round.html)).

- ci_width:

  Minimum width of the returned string for confidence intervals. If not
  `NULL` and width is larger than the string's length, leading
  whitespaces are added to the string. If `width="auto"`, width will be
  set to the length of the longest string.

- ci_brackets:

  Logical, if `TRUE` (default), CI-values are encompassed in square
  brackets (else in parentheses).

- zap_small:

  Logical, if `TRUE`, small values are rounded after `digits` decimal
  places. If `FALSE`, values with more decimal places than `digits` are
  printed in scientific notation.

- format:

  Name of output-format, as string. If `NULL` (or `"text"`), returned
  output is used for basic printing. Can be one of `NULL` (the default)
  resp. `"text"` for plain text, `"markdown"` (or `"md"`) for markdown
  and `"html"` for HTML output. A special option is `"tt"`, which
  creates a
  [`tinytable::tt()`](https://vincentarelbundock.github.io/tinytable/man/tt.html)
  object, where the output format is dependent on the context where the
  table is used, i.e. it can be markdown format when `export_table()` is
  used in markdown files, or LaTeX format when creating PDFs etc.

- groups:

  Named list, can be used to group parameters in the printed output.
  List elements may either be character vectors that match the name of
  those parameters that belong to one group, or list elements can be row
  numbers of those parameter rows that should belong to one group. The
  names of the list elements will be used as group names, which will be
  inserted as "header row". A possible use case might be to emphasize
  focal predictors and control variables, see 'Examples'. Parameters
  will be re-ordered according to the order used in `groups`, while all
  non-matching parameters will be added to the end.

- ...:

  Arguments passed down to
  [`format.parameters_model()`](https://easystats.github.io/parameters/reference/print.parameters_model.md),
  [`insight::format_table()`](https://easystats.github.io/insight/reference/format_table.html)
  and
  [`insight::export_table()`](https://easystats.github.io/insight/reference/export_table.html)

- caption:

  Table caption as string. If `NULL`, depending on the model, either a
  default caption or no table caption is printed. Use `caption = ""` to
  suppress the table caption.

- subtitle:

  Table title (same as caption) and subtitle, as strings. If `NULL`, no
  title or subtitle is printed, unless it is stored as attributes
  (`table_title`, or its alias `table_caption`, and `table_subtitle`).
  If `x` is a list of data frames, `caption` may be a list of table
  captions, one for each table.

- footer:

  Can either be `FALSE` or an empty string (i.e. `""`) to suppress the
  footer, `NULL` to print the default footer, or a string. The latter
  will combine the string value with the default footer.

- column_width:

  Width of table columns. Can be either `NULL`, a named numeric vector,
  or `"fixed"`. If `NULL`, the width for each table column is adjusted
  to the minimum required width. If a named numeric vector, value names
  are matched against column names, and for each match, the specified
  width is used. If `"fixed"`, and table is split into multiple
  components, columns across all table components are adjusted to have
  the same width.

- font_size:

  For HTML tables, the font size.

- line_padding:

  For HTML tables, the distance (in pixel) between lines.

- column_labels:

  Labels of columns for HTML tables. If `NULL`, automatic column names
  are generated. See 'Examples'.

## Value

Invisibly returns the original input object.

## Global Options to Customize Messages and Tables when Printing

The `verbose` argument can be used to display or silence messages and
warnings for the different functions in the **parameters** package.
However, some messages providing additional information can be displayed
or suppressed using [`options()`](https://rdrr.io/r/base/options.html):

- `parameters_info`: `options(parameters_info = TRUE)` will override the
  `include_info` argument in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  and always show the model summary for non-mixed models.

- `parameters_mixed_info`: `options(parameters_mixed_info = TRUE)` will
  override the `include_info` argument in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for mixed models, and will then always show the model summary.

- `parameters_cimethod`: `options(parameters_cimethod = TRUE)` will show
  the additional information about the approximation method used to
  calculate confidence intervals and p-values. Set to `FALSE` to hide
  this message when printing
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  objects.

- `parameters_exponentiate`: `options(parameters_exponentiate = TRUE)`
  will show the additional information on how to interpret coefficients
  of models with log-transformed response variables or with
  log-/logit-links when the `exponentiate` argument in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  is not `TRUE`. Set this option to `FALSE` to hide this message when
  printing
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  objects.

There are further options that can be used to modify the default
behaviour for printed outputs:

- `parameters_labels`: `options(parameters_labels = TRUE)` will use
  variable and value labels for pretty names, if data is labelled. If no
  labels available, default pretty names are used.

- `parameters_interaction`:
  `options(parameters_interaction = <character>)` will replace the
  interaction mark (by default, `*`) with the related character.

- `parameters_select`: `options(parameters_select = <value>)` will set
  the default for the `select` argument. See argument's documentation
  for available options.

- `easystats_table_width`: `options(easystats_table_width = <value>)`
  will set the default width for tables in text-format, i.e. for most of
  the outputs printed to console. If not specified, tables will be
  adjusted to the current available width, e.g. of the of the console
  (or any other source for textual output, like markdown files). The
  argument `table_width` can also be used in most
  [`print()`](https://rdrr.io/r/base/print.html) methods to specify the
  table width as desired.

- `insight_use_symbols`: `options(insight_use_symbols = TRUE)` will try
  to print unicode-chars for symbols as column names, wherever possible
  (e.g., ω instead of `Omega`).

## Examples

``` r
# \donttest{
data(iris)
lm1 <- lm(Sepal.Length ~ Species, data = iris)
lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)

# custom style
result <- compare_parameters(lm1, lm2, select = "{estimate}{stars} ({se})")
print(result)
#> Parameter            |            lm1 |             lm2
#> -------------------------------------------------------
#> (Intercept)          | 5.01*** (0.07) |  3.68*** (0.11)
#> Species [versicolor] | 0.93*** (0.10) | -1.60*** (0.19)
#> Species [virginica]  | 1.58*** (0.10) | -2.12*** (0.27)
#> Petal Length         |                |  0.90*** (0.06)
#> -------------------------------------------------------
#> Observations         |            150 |             150

# custom style, in HTML
result <- compare_parameters(lm1, lm2, select = "{estimate}<br>({se})|{p}")
print_html(result)


  


Parameter
```

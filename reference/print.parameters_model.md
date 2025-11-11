# Print model parameters

A [`print()`](https://rdrr.io/r/base/print.html)-method for objects from
[`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md).

## Usage

``` r
# S3 method for class 'parameters_model'
format(
  x,
  pretty_names = TRUE,
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
  include_reference = FALSE,
  ...
)

# S3 method for class 'parameters_model'
print(
  x,
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
  ...
)

# S3 method for class 'parameters_model'
summary(object, ...)

# S3 method for class 'parameters_model'
print_html(
  x,
  pretty_names = TRUE,
  split_components = TRUE,
  select = NULL,
  caption = NULL,
  subtitle = NULL,
  footer = NULL,
  align = NULL,
  digits = 2,
  ci_digits = digits,
  p_digits = 3,
  footer_digits = 3,
  ci_brackets = c("(", ")"),
  show_sigma = FALSE,
  show_formula = FALSE,
  zap_small = FALSE,
  groups = NULL,
  font_size = "100%",
  line_padding = 4,
  column_labels = NULL,
  include_reference = FALSE,
  verbose = TRUE,
  ...
)

# S3 method for class 'parameters_model'
print_md(
  x,
  pretty_names = TRUE,
  split_components = TRUE,
  select = NULL,
  caption = NULL,
  subtitle = NULL,
  footer = NULL,
  align = NULL,
  digits = 2,
  ci_digits = digits,
  p_digits = 3,
  footer_digits = 3,
  ci_brackets = c("(", ")"),
  show_sigma = FALSE,
  show_formula = FALSE,
  zap_small = FALSE,
  groups = NULL,
  include_reference = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- x, object:

  An object returned by
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md).

- pretty_names:

  Can be `TRUE`, which will return "pretty" (i.e. more human readable)
  parameter names. Or `"labels"`, in which case value and variable
  labels will be used as parameters names. The latter only works for
  "labelled" data, i.e. if the data used to fit the model had `"label"`
  and `"labels"` attributes. See also section *Global Options to
  Customize Messages when Printing*.

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

- include_reference:

  Logical, if `TRUE`, the reference level of factors will be added to
  the parameters table. This is only relevant for models with
  categorical predictors. The coefficient for the reference level is
  always `0` (except when `exponentiate = TRUE`, then the coefficient
  will be `1`), so this is just for completeness.

- ...:

  Arguments passed down to `format.parameters_model()`,
  [`insight::format_table()`](https://easystats.github.io/insight/reference/format_table.html)
  and
  [`insight::export_table()`](https://easystats.github.io/insight/reference/export_table.html)

- caption:

  Table caption as string. If `NULL`, depending on the model, either a
  default caption or no table caption is printed. Use `caption = ""` to
  suppress the table caption.

- footer:

  Can either be `FALSE` or an empty string (i.e. `""`) to suppress the
  footer, `NULL` to print the default footer, or a string. The latter
  will combine the string value with the default footer.

- footer_digits:

  Number of decimal places for values in the footer summary.

- show_sigma:

  Logical, if `TRUE`, adds information about the residual standard
  deviation.

- show_formula:

  Logical, if `TRUE`, adds the model formula to the output.

- column_width:

  Width of table columns. Can be either `NULL`, a named numeric vector,
  or `"fixed"`. If `NULL`, the width for each table column is adjusted
  to the minimum required width. If a named numeric vector, value names
  are matched against column names, and for each match, the specified
  width is used. If `"fixed"`, and table is split into multiple
  components, columns across all table components are adjusted to have
  the same width.

- subtitle:

  Table title (same as caption) and subtitle, as strings. If `NULL`, no
  title or subtitle is printed, unless it is stored as attributes
  (`table_title`, or its alias `table_caption`, and `table_subtitle`).
  If `x` is a list of data frames, `caption` may be a list of table
  captions, one for each table.

- align:

  Only applies to HTML tables. May be one of `"left"`, `"right"` or
  `"center"`.

- font_size:

  For HTML tables, the font size.

- line_padding:

  For HTML tables, the distance (in pixel) between lines.

- column_labels:

  Labels of columns for HTML tables. If `NULL`, automatic column names
  are generated. See 'Examples'.

- verbose:

  Toggle messages and warnings.

## Value

Invisibly returns the original input object.

## Details

[`summary()`](https://rdrr.io/r/base/summary.html) is a convenient
shortcut for
`print(object, select = "minimal", show_sigma = TRUE, show_formula = TRUE)`.

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

## Interpretation of Interaction Terms

Note that the *interpretation* of interaction terms depends on many
characteristics of the model. The number of parameters, and overall
performance of the model, can differ *or not* between `a * b`, `a : b`,
and `a / b`, suggesting that sometimes interaction terms give different
parameterizations of the same model, but other times it gives completely
different models (depending on `a` or `b` being factors of covariates,
included as main effects or not, etc.). Their interpretation depends of
the full context of the model, which should not be inferred from the
parameters table alone - rather, we recommend to use packages that
calculate estimated marginal means or marginal effects, such as
[modelbased](https://CRAN.R-project.org/package=modelbased),
[emmeans](https://CRAN.R-project.org/package=emmeans),
[ggeffects](https://CRAN.R-project.org/package=ggeffects), or
[marginaleffects](https://CRAN.R-project.org/package=marginaleffects).
To raise awareness for this issue, you may use
`print(...,show_formula=TRUE)` to add the model-specification to the
output of the [`print()`](https://rdrr.io/r/base/print.html) method for
[`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md).

## Labeling the Degrees of Freedom

Throughout the **parameters** package, we decided to label the residual
degrees of freedom *df_error*. The reason for this is that these degrees
of freedom not always refer to the residuals. For certain models, they
refer to the estimate error - in a linear model these are the same, but
in - for instance - any mixed effects model, this isn't strictly true.
Hence, we think that `df_error` is the most generic label for these
degrees of freedom.

## See also

See also
[`display()`](https://easystats.github.io/parameters/reference/display.parameters_model.md).

## Examples

``` r
# \donttest{
library(parameters)
model <- glmmTMB::glmmTMB(
  count ~ spp + mined + (1 | site),
  ziformula = ~mined,
  family = poisson(),
  data = Salamanders
)
mp <- model_parameters(model)

print(mp, pretty_names = FALSE)
#> # Fixed Effects (Count Model) 
#> 
#> Parameter   | Log-Mean |   SE |         95% CI |     z |      p
#> ---------------------------------------------------------------
#> (Intercept) |    -0.36 | 0.28 | [-0.90,  0.18] | -1.30 | 0.194 
#> sppPR       |    -1.27 | 0.24 | [-1.74, -0.80] | -5.27 | < .001
#> sppDM       |     0.27 | 0.14 | [ 0.00,  0.54] |  1.95 | 0.051 
#> sppEC-A     |    -0.57 | 0.21 | [-0.97, -0.16] | -2.75 | 0.006 
#> sppEC-L     |     0.67 | 0.13 | [ 0.41,  0.92] |  5.20 | < .001
#> sppDES-L    |     0.63 | 0.13 | [ 0.38,  0.87] |  4.96 | < .001
#> sppDF       |     0.12 | 0.15 | [-0.17,  0.40] |  0.78 | 0.435 
#> minedno     |     1.27 | 0.27 | [ 0.74,  1.80] |  4.72 | < .001
#> 
#> # Fixed Effects (Zero-Inflation Component) 
#> 
#> Parameter   | Log-Odds |   SE |         95% CI |     z |      p
#> ---------------------------------------------------------------
#> (Intercept) |     0.79 | 0.27 | [ 0.26,  1.32] |  2.90 | 0.004 
#> minedno     |    -1.84 | 0.31 | [-2.46, -1.23] | -5.87 | < .001
#> 
#> # Random Effects Variances 
#> 
#> Parameter            | Coefficient |       95% CI
#> -------------------------------------------------
#> SD (Intercept: site) |        0.33 | [0.18, 0.63]
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a Wald z-distribution approximation.

print(mp, split_components = FALSE)
#> # Fixed Effects
#> 
#> Parameter            | Coefficient |   SE |         95% CI |     z |      p
#> ---------------------------------------------------------------------------
#> (Intercept)          |       -0.36 | 0.28 | [-0.90,  0.18] | -1.30 | 0.194 
#> spp [PR]             |       -1.27 | 0.24 | [-1.74, -0.80] | -5.27 | < .001
#> spp [DM]             |        0.27 | 0.14 | [ 0.00,  0.54] |  1.95 | 0.051 
#> spp [EC-A]           |       -0.57 | 0.21 | [-0.97, -0.16] | -2.75 | 0.006 
#> spp [EC-L]           |        0.67 | 0.13 | [ 0.41,  0.92] |  5.20 | < .001
#> spp [DES-L]          |        0.63 | 0.13 | [ 0.38,  0.87] |  4.96 | < .001
#> spp [DF]             |        0.12 | 0.15 | [-0.17,  0.40] |  0.78 | 0.435 
#> mined [no]           |        1.27 | 0.27 | [ 0.74,  1.80] |  4.72 | < .001
#> (Intercept)          |        0.79 | 0.27 | [ 0.26,  1.32] |  2.90 | 0.004 
#> minedno              |       -1.84 | 0.31 | [-2.46, -1.23] | -5.87 | < .001
#> SD (Intercept: site) |        0.33 |      | [ 0.18,  0.63] |       |       
#> 
#> Parameter            | Effects |     Component
#> ----------------------------------------------
#> (Intercept)          |   fixed |   conditional
#> spp [PR]             |   fixed |   conditional
#> spp [DM]             |   fixed |   conditional
#> spp [EC-A]           |   fixed |   conditional
#> spp [EC-L]           |   fixed |   conditional
#> spp [DES-L]          |   fixed |   conditional
#> spp [DF]             |   fixed |   conditional
#> mined [no]           |   fixed |   conditional
#> (Intercept)          |   fixed | zero_inflated
#> minedno              |   fixed | zero_inflated
#> SD (Intercept: site) |  random |   conditional
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a Wald z-distribution approximation.

print(mp, select = c("Parameter", "Coefficient", "SE"))
#> # Fixed Effects (Count Model) 
#> 
#> Parameter   | Log-Mean |   SE
#> -----------------------------
#> (Intercept) |    -0.36 | 0.28
#> spp [PR]    |    -1.27 | 0.24
#> spp [DM]    |     0.27 | 0.14
#> spp [EC-A]  |    -0.57 | 0.21
#> spp [EC-L]  |     0.67 | 0.13
#> spp [DES-L] |     0.63 | 0.13
#> spp [DF]    |     0.12 | 0.15
#> mined [no]  |     1.27 | 0.27
#> 
#> # Fixed Effects (Zero-Inflation Component) 
#> 
#> Parameter   | Log-Odds |   SE
#> -----------------------------
#> (Intercept) |     0.79 | 0.27
#> mined [no]  |    -1.84 | 0.31
#> 
#> # Random Effects Variances 
#> 
#> Parameter            | Coefficient
#> ----------------------------------
#> SD (Intercept: site) |        0.33
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a Wald z-distribution approximation.

print(mp, select = "minimal")
#> # Fixed Effects (Count Model) 
#> 
#> Parameter   | Log-Mean |         95% CI |      p
#> ------------------------------------------------
#> (Intercept) |    -0.36 | [-0.90,  0.18] | 0.194 
#> spp [PR]    |    -1.27 | [-1.74, -0.80] | < .001
#> spp [DM]    |     0.27 | [ 0.00,  0.54] | 0.051 
#> spp [EC-A]  |    -0.57 | [-0.97, -0.16] | 0.006 
#> spp [EC-L]  |     0.67 | [ 0.41,  0.92] | < .001
#> spp [DES-L] |     0.63 | [ 0.38,  0.87] | < .001
#> spp [DF]    |     0.12 | [-0.17,  0.40] | 0.435 
#> mined [no]  |     1.27 | [ 0.74,  1.80] | < .001
#> 
#> # Fixed Effects (Zero-Inflation Component) 
#> 
#> Parameter   | Log-Odds |         95% CI |      p
#> ------------------------------------------------
#> (Intercept) |     0.79 | [ 0.26,  1.32] | 0.004 
#> mined [no]  |    -1.84 | [-2.46, -1.23] | < .001
#> 
#> # Random Effects Variances 
#> 
#> Parameter            | Coefficient |       95% CI
#> -------------------------------------------------
#> SD (Intercept: site) |        0.33 | [0.18, 0.63]
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a Wald z-distribution approximation.


# group parameters ------

data(iris)
model <- lm(
  Sepal.Width ~ Sepal.Length + Species + Petal.Length,
  data = iris
)
# don't select "Intercept" parameter
mp <- model_parameters(model, parameters = "^(?!\\(Intercept)")
groups <- list(`Focal Predictors` = c(1, 4), Controls = c(2, 3))
print(mp, groups = groups)
#> Parameter              | Coefficient |   SE |         95% CI | t(145) |      p
#> ------------------------------------------------------------------------------
#> Focal Predictors       |             |      |                |        |       
#>   (Intercept)          |        1.60 | 0.28 | [ 1.06,  2.15] |   5.80 | < .001
#>   Species [virginica]  |       -0.88 | 0.28 | [-1.43, -0.33] |  -3.15 | 0.002 
#> Controls               |             |      |                |        |       
#>   Sepal Length         |        0.38 | 0.07 | [ 0.24,  0.52] |   5.31 | < .001
#>   Species [versicolor] |       -0.89 | 0.20 | [-1.29, -0.49] |  -4.43 | < .001
#> Petal Length           |       -0.04 | 0.08 | [-0.21,  0.13] |  -0.50 | 0.618 
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a Wald t-distribution approximation.

# only show coefficients, CI and p,
# put non-matched parameters to the end

data(mtcars)
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$gear <- as.factor(mtcars$gear)
model <- lm(mpg ~ hp + gear * vs + cyl + drat, data = mtcars)

# don't select "Intercept" parameter
mp <- model_parameters(model, parameters = "^(?!\\(Intercept)")
print(mp, groups = list(
  Engine = c(5, 6, 4, 1),
  Interactions = c(8, 9)
))
#> Parameter       | Coefficient |   SE |          95% CI | t(22) |     p
#> ----------------------------------------------------------------------
#> Engine          |             |      |                 |       |      
#>   vs            |        3.18 | 3.79 | [ -4.68, 11.04] |  0.84 | 0.410
#>   cyl [6]       |       -2.47 | 2.21 | [ -7.05,  2.12] | -1.12 | 0.276
#>   gear [5]      |        4.80 | 3.48 | [ -2.42, 12.01] |  1.38 | 0.182
#>   (Intercept)   |       16.63 | 7.77 | [  0.53, 32.74] |  2.14 | 0.044
#> Interactions    |             |      |                 |       |      
#>   drat          |        2.70 | 2.03 | [ -1.52,  6.91] |  1.33 | 0.198
#>   gear [4] × vs |       -2.90 | 4.67 | [-12.57,  6.78] | -0.62 | 0.541
#> hp              |       -0.06 | 0.02 | [ -0.11, -0.02] | -2.91 | 0.008
#> gear [4]        |        3.10 | 4.34 | [ -5.90, 12.10] |  0.71 | 0.482
#> cyl [8]         |        1.97 | 5.11 | [ -8.63, 12.58] |  0.39 | 0.703
#> gear [5] × vs   |        2.59 | 4.54 | [ -6.82, 12.00] |  0.57 | 0.574
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a Wald t-distribution approximation.
# }


# custom column layouts ------

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

# \donttest{
# custom style, in HTML
result <- compare_parameters(lm1, lm2, select = "{estimate}<br>({se})|{p}")
print_html(result)


  


Parameter
```

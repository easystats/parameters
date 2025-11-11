# Compare model parameters of multiple models

Compute and extract model parameters of multiple regression models. See
[`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
for further details.

## Usage

``` r
compare_parameters(
  ...,
  ci = 0.95,
  effects = "fixed",
  component = "conditional",
  standardize = NULL,
  exponentiate = FALSE,
  ci_method = "wald",
  p_adjust = NULL,
  select = NULL,
  column_names = NULL,
  pretty_names = TRUE,
  coefficient_names = NULL,
  keep = NULL,
  drop = NULL,
  include_reference = FALSE,
  groups = NULL,
  verbose = TRUE
)

compare_models(
  ...,
  ci = 0.95,
  effects = "fixed",
  component = "conditional",
  standardize = NULL,
  exponentiate = FALSE,
  ci_method = "wald",
  p_adjust = NULL,
  select = NULL,
  column_names = NULL,
  pretty_names = TRUE,
  coefficient_names = NULL,
  keep = NULL,
  drop = NULL,
  include_reference = FALSE,
  groups = NULL,
  verbose = TRUE
)
```

## Arguments

- ...:

  One or more regression model objects, or objects returned by
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md).
  Regression models may be of different model types. Model objects may
  be passed comma separated, or as a list. If model objects are passed
  with names or the list has named elements, these names will be used as
  column names.

- ci:

  Confidence Interval (CI) level. Default to `0.95` (`95%`).

- effects:

  Should parameters for fixed effects (`"fixed"`), random effects
  (`"random"`), or both fixed and random effects (`"all"`) be returned?
  By default, the variance components for random effects are returned.
  If group-level effects are requested, `"grouplevel"` returns the
  group-level random effects (BLUPs), while `"random_total"` return the
  overall (sum of fixed and random) effects (similar to what
  [`coef()`](https://rdrr.io/r/stats/coef.html) returns). Using
  `"grouplevel"` is equivalent to setting `group_level = TRUE`. The
  `effects` argument only applies to mixed models. If the calculation of
  random effects parameters takes too long, you may use
  `effects = "fixed"`.

- component:

  Model component for which parameters should be shown. See
  documentation for related model class in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md).

- standardize:

  The method used for standardizing the parameters. Can be `NULL`
  (default; no standardization), `"refit"` (for re-fitting the model on
  standardized data) or one of `"basic"`, `"posthoc"`, `"smart"`,
  `"pseudo"`. See 'Details' in
  [`standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.md).
  **Importantly**:

  - The `"refit"` method does *not* standardize categorical predictors
    (i.e. factors), which may be a different behaviour compared to other
    R packages (such as **lm.beta**) or other software packages (like
    SPSS). to mimic such behaviours, either use `standardize="basic"` or
    standardize the data with `datawizard::standardize(force=TRUE)`
    *before* fitting the model.

  - By default, the response (dependent) variable is also standardized,
    *if applicable*. Set `include_response = FALSE` to avoid
    standardization of the response variable. See details in
    [`datawizard::standardize.default()`](https://easystats.github.io/datawizard/reference/standardize.default.html).

  - For mixed models, when using methods other than `"refit"`, only the
    fixed effects will be standardized.

  - Robust estimation (i.e., `vcov` set to a value other than `NULL`) of
    standardized parameters only works when `standardize="refit"`.

- exponentiate:

  Logical, indicating whether or not to exponentiate the coefficients
  (and related confidence intervals). This is typical for logistic
  regression, or more generally speaking, for models with log or logit
  links. It is also recommended to use `exponentiate = TRUE` for models
  with log-transformed response values. For models with a
  log-transformed response variable, when `exponentiate = TRUE`, a
  one-unit increase in the predictor is associated with multiplying the
  outcome by that predictor's coefficient. **Note:** Delta-method
  standard errors are also computed (by multiplying the standard errors
  by the transformed coefficients). This is to mimic behaviour of other
  software packages, such as Stata, but these standard errors poorly
  estimate uncertainty for the transformed coefficient. The transformed
  confidence interval more clearly captures this uncertainty. For
  `compare_parameters()`, `exponentiate = "nongaussian"` will only
  exponentiate coefficients from non-Gaussian families.

- ci_method:

  Method for computing degrees of freedom for p-values and confidence
  intervals (CI). See documentation for related model class in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md).

- p_adjust:

  String value, if not `NULL`, indicates the method to adjust p-values.
  See [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) for
  details. Further possible adjustment methods are `"tukey"`,
  `"scheffe"`, `"sidak"`, `"sup-t"`, and `"none"` to explicitly disable
  adjustment for `emmGrid` objects (from **emmeans**). `"sup-t"`
  computes simultaneous confidence bands, also called sup-t confidence
  band (Montiel Olea & Plagborg-Møller, 2019).

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

- column_names:

  Character vector with strings that should be used as column headers.
  Must be of same length as number of models in `...`.

- pretty_names:

  Can be `TRUE`, which will return "pretty" (i.e. more human readable)
  parameter names. Or `"labels"`, in which case value and variable
  labels will be used as parameters names. The latter only works for
  "labelled" data, i.e. if the data used to fit the model had `"label"`
  and `"labels"` attributes. See also section *Global Options to
  Customize Messages when Printing*.

- coefficient_names:

  Character vector with strings that should be used as column headers
  for the coefficient column. Must be of same length as number of models
  in `...`, or length 1. If length 1, this name will be used for all
  coefficient columns. If `NULL`, the name for the coefficient column
  will detected automatically (as in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)).

- keep:

  Character containing a regular expression pattern that describes the
  parameters that should be included (for `keep`) or excluded (for
  `drop`) in the returned data frame. `keep` may also be a named list of
  regular expressions. All non-matching parameters will be removed from
  the output. If `keep` is a character vector, every parameter name in
  the *"Parameter"* column that matches the regular expression in `keep`
  will be selected from the returned data frame (and vice versa, all
  parameter names matching `drop` will be excluded). Furthermore, if
  `keep` has more than one element, these will be merged with an `OR`
  operator into a regular expression pattern like this:
  `"(one|two|three)"`. If `keep` is a named list of regular expression
  patterns, the names of the list-element should equal the column name
  where selection should be applied. This is useful for model objects
  where
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  returns multiple columns with parameter components, like in
  [`model_parameters.lavaan()`](https://easystats.github.io/parameters/reference/model_parameters.principal.md).
  Note that the regular expression pattern should match the parameter
  names as they are stored in the returned data frame, which can be
  different from how they are printed. Inspect the `$Parameter` column
  of the parameters table to get the exact parameter names.

- drop:

  See `keep`.

- include_reference:

  Logical, if `TRUE`, the reference level of factors will be added to
  the parameters table. This is only relevant for models with
  categorical predictors. The coefficient for the reference level is
  always `0` (except when `exponentiate = TRUE`, then the coefficient
  will be `1`), so this is just for completeness.

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

- verbose:

  Toggle warnings and messages.

## Value

A data frame of indices related to the model's parameters.

## Details

This function is in an early stage and does not yet cope with more
complex models, and probably does not yet properly render all model
components. It should also be noted that when including models with
interaction terms, not only do the values of the parameters change, but
so does their meaning (from main effects, to simple slopes), thereby
making such comparisons hard. Therefore, you should not use this
function to compare models with interaction terms with models without
interaction terms.

## Examples

``` r
data(iris)
lm1 <- lm(Sepal.Length ~ Species, data = iris)
lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
compare_parameters(lm1, lm2)
#> Parameter            |               lm1 |                  lm2
#> ---------------------------------------------------------------
#> (Intercept)          | 5.01 (4.86, 5.15) |  3.68 ( 3.47,  3.89)
#> Species [versicolor] | 0.93 (0.73, 1.13) | -1.60 (-1.98, -1.22)
#> Species [virginica]  | 1.58 (1.38, 1.79) | -2.12 (-2.66, -1.58)
#> Petal Length         |                   |  0.90 ( 0.78,  1.03)
#> ---------------------------------------------------------------
#> Observations         |               150 |                  150

# custom style
compare_parameters(lm1, lm2, select = "{estimate}{stars} ({se})")
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

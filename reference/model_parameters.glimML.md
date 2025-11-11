# Parameters from special models

Parameters from special regression models not listed under one of the
previous categories yet.

## Usage

``` r
# S3 method for class 'glimML'
model_parameters(
  model,
  ci = 0.95,
  bootstrap = FALSE,
  iterations = 1000,
  component = "conditional",
  standardize = NULL,
  exponentiate = FALSE,
  p_adjust = NULL,
  include_info = getOption("parameters_info", FALSE),
  keep = NULL,
  drop = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- model:

  Model object.

- ci:

  Confidence Interval (CI) level. Default to `0.95` (`95%`).

- bootstrap:

  Should estimates be based on bootstrapped model? If `TRUE`, then
  arguments of [Bayesian
  regressions](https://easystats.github.io/parameters/reference/model_parameters.brmsfit.md)
  apply (see also
  [`bootstrap_parameters()`](https://easystats.github.io/parameters/reference/bootstrap_parameters.md)).

- iterations:

  The number of bootstrap replicates. This only apply in the case of
  bootstrapped frequentist models.

- component:

  Model component for which parameters should be shown. May be one of
  `"conditional"`, `"precision"` (e.g. **betareg**), `"scale"` (e.g.
  **ordinal**), `"extra"` (e.g. **glmx**), `"marginal"` (e.g. **mfx**),
  `"conditional"` or `"full"` (for
  [`MuMIn::model.avg()`](https://rdrr.io/pkg/MuMIn/man/model.avg.html))
  or `"all"`. See section *Model components* for an overview of possible
  options for `component`.

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
  [`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md),
  `exponentiate = "nongaussian"` will only exponentiate coefficients
  from non-Gaussian families.

- p_adjust:

  String value, if not `NULL`, indicates the method to adjust p-values.
  See [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) for
  details. Further possible adjustment methods are `"tukey"`,
  `"scheffe"`, `"sidak"`, `"sup-t"`, and `"none"` to explicitly disable
  adjustment for `emmGrid` objects (from **emmeans**). `"sup-t"`
  computes simultaneous confidence bands, also called sup-t confidence
  band (Montiel Olea & Plagborg-Møller, 2019).

- include_info:

  Logical, if `TRUE`, prints summary information about the model (model
  formula, number of observations, residual standard deviation and
  more).

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

- verbose:

  Toggle warnings and messages.

- ...:

  Arguments passed to or from other methods. For instance, when
  `bootstrap = TRUE`, arguments like `type` or `parallel` are passed
  down to
  [`bootstrap_model()`](https://easystats.github.io/parameters/reference/bootstrap_model.md).

  Further non-documented arguments are:

  - `digits`, `p_digits`, `ci_digits` and `footer_digits` to set the
    number of digits for the output. `groups` can be used to group
    coefficients. These arguments will be passed to the print-method, or
    can directly be used in
    [`print()`](https://rdrr.io/r/base/print.html), see documentation in
    [`print.parameters_model()`](https://easystats.github.io/parameters/reference/print.parameters_model.md).

  - If `s_value = TRUE`, the p-value will be replaced by the S-value in
    the output (cf. *Rafi and Greenland 2020*).

  - `pd` adds an additional column with the *probability of direction*
    (see
    [`bayestestR::p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.html)
    for details). Furthermore, see 'Examples' for this function.

  - For developers, whose interest mainly is to get a "tidy" data frame
    of model summaries, it is recommended to set `pretty_names = FALSE`
    to speed up computation of the summary table.

## Value

A data frame of indices related to the model's parameters.

## Model components

Possible values for the `component` argument depend on the model class.
Following are valid options:

- `"all"`: returns all model components, applies to all models, but will
  only have an effect for models with more than just the conditional
  model component.

- `"conditional"`: only returns the conditional component, i.e. "fixed
  effects" terms from the model. Will only have an effect for models
  with more than just the conditional model component.

- `"smooth_terms"`: returns smooth terms, only applies to GAMs (or
  similar models that may contain smooth terms).

- `"zero_inflated"` (or `"zi"`): returns the zero-inflation component.

- `"dispersion"`: returns the dispersion model component. This is common
  for models with zero-inflation or that can model the dispersion
  parameter.

- `"instruments"`: for instrumental-variable or some fixed effects
  regression, returns the instruments.

- `"nonlinear"`: for non-linear models (like models of class `nlmerMod`
  or `nls`), returns staring estimates for the nonlinear parameters.

- `"correlation"`: for models with correlation-component, like `gls`,
  the variables used to describe the correlation structure are returned.

**Special models**

Some model classes also allow rather uncommon options. These are:

- **mhurdle**: `"infrequent_purchase"`, `"ip"`, and `"auxiliary"`

- **BGGM**: `"correlation"` and `"intercept"`

- **BFBayesFactor**, **glmx**: `"extra"`

- **averaging**:`"conditional"` and `"full"`

- **mjoint**: `"survival"`

- **mfx**: `"precision"`, `"marginal"`

- **betareg**, **DirichletRegModel**: `"precision"`

- **mvord**: `"thresholds"` and `"correlation"`

- **clm2**: `"scale"`

- **selection**: `"selection"`, `"outcome"`, and `"auxiliary"`

- **lavaan**: One or more of `"regression"`, `"correlation"`,
  `"loading"`, `"variance"`, `"defined"`, or `"mean"`. Can also be
  `"all"` to include all components.

For models of class `brmsfit` (package **brms**), even more options are
possible for the `component` argument, which are not all documented in
detail here.

## See also

[`insight::standardize_names()`](https://easystats.github.io/insight/reference/standardize_names.html)
to rename columns into a consistent, standardized naming scheme.

## Examples

``` r
library(parameters)
if (require("brglm2", quietly = TRUE)) {
  data("stemcell")
  model <- bracl(
    research ~ as.numeric(religion) + gender,
    weights = frequency,
    data = stemcell,
    type = "ML"
  )
  model_parameters(model)
}
#> # Response level: definitely
#> 
#> Parameter       | Log-Odds |   SE |         95% CI |     z |      p
#> -------------------------------------------------------------------
#> (Intercept)     |    -1.25 | 0.26 | [-1.76, -0.73] | -4.76 | < .001
#> religion        |     0.44 | 0.10 | [ 0.23,  0.64] |  4.20 | < .001
#> gender [female] |    -0.14 | 0.17 | [-0.47,  0.19] | -0.82 | 0.414 
#> 
#> # Response level: probably
#> 
#> Parameter       | Log-Odds |   SE |        95% CI |    z |     p
#> ----------------------------------------------------------------
#> (Intercept)     |     0.47 | 0.29 | [-0.10, 1.04] | 1.62 | 0.105
#> religion        |     0.26 | 0.13 | [ 0.01, 0.51] | 2.01 | 0.044
#> gender [female] |     0.19 | 0.21 | [-0.22, 0.60] | 0.90 | 0.370
#> 
#> # Response level: probably not
#> 
#> Parameter       | Log-Odds |   SE |        95% CI |     z |     p
#> -----------------------------------------------------------------
#> (Intercept)     |     0.43 | 0.39 | [-0.33, 1.18] |  1.11 | 0.268
#> religion        |     0.01 | 0.17 | [-0.33, 0.35] |  0.07 | 0.945
#> gender [female] |    -0.16 | 0.28 | [-0.71, 0.39] | -0.57 | 0.566
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a Wald z-distribution approximation.
```

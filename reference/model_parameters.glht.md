# Parameters from Hypothesis Testing

Parameters from Hypothesis Testing.

## Usage

``` r
# S3 method for class 'glht'
model_parameters(
  model,
  ci = 0.95,
  exponentiate = FALSE,
  keep = NULL,
  drop = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- model:

  Object of class
  [`multcomp::glht()`](https://rdrr.io/pkg/multcomp/man/glht.html)
  (**multcomp**) or of class `PMCMR`, `trendPMCMR` or `osrt`
  (**PMCMRplus**).

- ci:

  Confidence Interval (CI) level. Default to `0.95` (`95%`).

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

## Examples

``` r
# \donttest{
if (require("multcomp", quietly = TRUE)) {
  # multiple linear model, swiss data
  lmod <- lm(Fertility ~ ., data = swiss)
  mod <- glht(
    model = lmod,
    linfct = c(
      "Agriculture = 0",
      "Examination = 0",
      "Education = 0",
      "Catholic = 0",
      "Infant.Mortality = 0"
    )
  )
  model_parameters(mod)
}
#> 
#> Attaching package: ‘mvtnorm’
#> The following object is masked from ‘package:mclust’:
#> 
#>     dmvnorm
#> 
#> Attaching package: ‘survival’
#> The following object is masked from ‘package:brms’:
#> 
#>     kidney
#> The following object is masked from ‘package:boot’:
#> 
#>     aml
#> 
#> Attaching package: ‘TH.data’
#> The following object is masked from ‘package:MASS’:
#> 
#>     geyser
#> # Fixed Effects
#> 
#> Parameter             | Coefficient |   SE |         95% CI | t(41) |      p
#> ----------------------------------------------------------------------------
#> Agriculture == 0      |       -0.17 | 0.07 | [-0.36,  0.01] | -2.45 | 0.079 
#> Examination == 0      |       -0.26 | 0.25 | [-0.93,  0.41] | -1.02 | 0.785 
#> Education == 0        |       -0.87 | 0.18 | [-1.36, -0.39] | -4.76 | < .001
#> Catholic == 0         |        0.10 | 0.04 | [ 0.01,  0.20] |  2.95 | 0.024 
#> Infant Mortality == 0 |        1.08 | 0.38 | [ 0.07,  2.09] |  2.82 | 0.033 
#> 
#> p-value adjustment method: single-step
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a Wald t-distribution approximation.
if (require("PMCMRplus", quietly = TRUE)) {
  model <- suppressWarnings(
    kwAllPairsConoverTest(count ~ spray, data = InsectSprays)
  )
  model_parameters(model)
}
#> Conover's all-pairs test
#> 
#> Group1 | Group2 | Statistic |      p | alternative | Distribution | p_adjustment
#> --------------------------------------------------------------------------------
#> B      |      A |      0.89 | 0.988  |   two.sided |            q |  single-step
#> C      |      A |    -13.58 | < .001 |   two.sided |            q |  single-step
#> C      |      B |    -14.47 | < .001 |   two.sided |            q |  single-step
#> D      |      A |     -8.87 | < .001 |   two.sided |            q |  single-step
#> D      |      B |     -9.76 | < .001 |   two.sided |            q |  single-step
#> D      |      C |      4.71 | 0.017  |   two.sided |            q |  single-step
#> E      |      A |    -10.95 | < .001 |   two.sided |            q |  single-step
#> E      |      B |    -11.84 | < .001 |   two.sided |            q |  single-step
#> E      |      C |      2.63 | 0.437  |   two.sided |            q |  single-step
#> E      |      D |     -2.09 | 0.681  |   two.sided |            q |  single-step
#> F      |      A |      1.15 | 0.964  |   two.sided |            q |  single-step
#> F      |      B |      0.26 | > .999 |   two.sided |            q |  single-step
#> F      |      C |     14.74 | < .001 |   two.sided |            q |  single-step
#> F      |      D |     10.02 | < .001 |   two.sided |            q |  single-step
#> F      |      E |     12.11 | < .001 |   two.sided |            q |  single-step
# }
```

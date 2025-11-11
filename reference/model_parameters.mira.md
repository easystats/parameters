# Parameters from multiply imputed repeated analyses

Format models of class `mira`, obtained from `mice::width.mids()`, or of
class `mipo`.

## Usage

``` r
# S3 method for class 'mira'
model_parameters(
  model,
  ci = 0.95,
  exponentiate = FALSE,
  p_adjust = NULL,
  keep = NULL,
  drop = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- model:

  An object of class `mira` or `mipo`.

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

- p_adjust:

  String value, if not `NULL`, indicates the method to adjust p-values.
  See [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) for
  details. Further possible adjustment methods are `"tukey"`,
  `"scheffe"`, `"sidak"`, `"sup-t"`, and `"none"` to explicitly disable
  adjustment for `emmGrid` objects (from **emmeans**). `"sup-t"`
  computes simultaneous confidence bands, also called sup-t confidence
  band (Montiel Olea & Plagborg-Møller, 2019).

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

  Arguments passed to or from other methods.

## Details

[`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
for objects of class `mira` works similar to `summary(mice::pool())`,
i.e. it generates the pooled summary of multiple imputed repeated
regression analyses.

## Examples

``` r
library(parameters)
data(nhanes2, package = "mice")
imp <- mice::mice(nhanes2)
#> 
#>  iter imp variable
#>   1   1  bmi  hyp  chl
#>   1   2  bmi  hyp  chl
#>   1   3  bmi  hyp  chl
#>   1   4  bmi  hyp  chl
#>   1   5  bmi  hyp  chl
#>   2   1  bmi  hyp  chl
#>   2   2  bmi  hyp  chl
#>   2   3  bmi  hyp  chl
#>   2   4  bmi  hyp  chl
#>   2   5  bmi  hyp  chl
#>   3   1  bmi  hyp  chl
#>   3   2  bmi  hyp  chl
#>   3   3  bmi  hyp  chl
#>   3   4  bmi  hyp  chl
#>   3   5  bmi  hyp  chl
#>   4   1  bmi  hyp  chl
#>   4   2  bmi  hyp  chl
#>   4   3  bmi  hyp  chl
#>   4   4  bmi  hyp  chl
#>   4   5  bmi  hyp  chl
#>   5   1  bmi  hyp  chl
#>   5   2  bmi  hyp  chl
#>   5   3  bmi  hyp  chl
#>   5   4  bmi  hyp  chl
#>   5   5  bmi  hyp  chl
fit <- with(data = imp, exp = lm(bmi ~ age + hyp + chl))
model_parameters(fit)
#> # Fixed Effects
#> 
#> Parameter   | Coefficient |   SE |          95% CI | Statistic |   df |      p
#> ------------------------------------------------------------------------------
#> (Intercept) |       19.00 | 3.86 | [ 10.39, 27.61] |      4.92 | 9.95 | < .001
#> age40-59    |       -4.97 | 2.25 | [-10.16,  0.22] |     -2.20 | 8.05 | 0.058 
#> age60-99    |       -7.12 | 2.60 | [-13.36, -0.87] |     -2.74 | 6.50 | 0.031 
#> hypyes      |        2.52 | 2.18 | [ -2.39,  7.44] |      1.16 | 9.28 | 0.277 
#> chl         |        0.05 | 0.02 | [ -0.00,  0.11] |      2.26 | 6.61 | 0.061 
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a Wald distribution approximation.
# \donttest{
# model_parameters() also works for models that have no "tidy"-method in mice
data(warpbreaks)
set.seed(1234)
warpbreaks$tension[sample(1:nrow(warpbreaks), size = 10)] <- NA
imp <- mice::mice(warpbreaks)
#> 
#>  iter imp variable
#>   1   1  tension
#>   1   2  tension
#>   1   3  tension
#>   1   4  tension
#>   1   5  tension
#>   2   1  tension
#>   2   2  tension
#>   2   3  tension
#>   2   4  tension
#>   2   5  tension
#>   3   1  tension
#>   3   2  tension
#>   3   3  tension
#>   3   4  tension
#>   3   5  tension
#>   4   1  tension
#>   4   2  tension
#>   4   3  tension
#>   4   4  tension
#>   4   5  tension
#>   5   1  tension
#>   5   2  tension
#>   5   3  tension
#>   5   4  tension
#>   5   5  tension
fit <- with(data = imp, expr = gee::gee(breaks ~ tension, id = wool))
#> Beginning Cgee S-function, @(#) geeformula.q 4.13 98/01/27
#> running glm to get initial regression estimate
#> (Intercept)    tensionM    tensionH 
#>    36.04762   -12.26984   -13.71429 
#> Beginning Cgee S-function, @(#) geeformula.q 4.13 98/01/27
#> running glm to get initial regression estimate
#> (Intercept)    tensionM    tensionH 
#>    35.04545   -10.29545   -12.98295 
#> Beginning Cgee S-function, @(#) geeformula.q 4.13 98/01/27
#> running glm to get initial regression estimate
#> (Intercept)    tensionM    tensionH 
#>   35.150000   -8.973529  -13.267647 
#> Beginning Cgee S-function, @(#) geeformula.q 4.13 98/01/27
#> running glm to get initial regression estimate
#> (Intercept)    tensionM    tensionH 
#>    36.66667   -13.26667   -14.50000 
#> Beginning Cgee S-function, @(#) geeformula.q 4.13 98/01/27
#> running glm to get initial regression estimate
#> (Intercept)    tensionM    tensionH 
#>    36.15000   -11.37222   -14.21250 

# does not work:
# summary(mice::pool(fit))

model_parameters(fit)
#> New names:
#> • `` -> `...6`
#> New names:
#> • `` -> `...6`
#> New names:
#> • `` -> `...6`
#> New names:
#> • `` -> `...6`
#> New names:
#> • `` -> `...6`
#> # Fixed Effects
#> 
#> Parameter   | Coefficient |   SE |          95% CI | Statistic |      df |      p
#> ---------------------------------------------------------------------------------
#> (Intercept) |       35.81 | 2.71 | [ 30.49, 41.13] |     13.21 |  646.80 | < .001
#> tensionM    |      -11.24 | 4.31 | [-19.76, -2.71] |     -2.61 |  121.44 | 0.010 
#> tensionH    |      -13.74 | 3.98 | [-21.54, -5.93] |     -3.45 | 4380.62 | < .001
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a Wald distribution approximation.
# }

# and it works with pooled results
data("nhanes2", package = "mice")
imp <- mice::mice(nhanes2)
#> 
#>  iter imp variable
#>   1   1  bmi  hyp  chl
#>   1   2  bmi  hyp  chl
#>   1   3  bmi  hyp  chl
#>   1   4  bmi  hyp  chl
#>   1   5  bmi  hyp  chl
#>   2   1  bmi  hyp  chl
#>   2   2  bmi  hyp  chl
#>   2   3  bmi  hyp  chl
#>   2   4  bmi  hyp  chl
#>   2   5  bmi  hyp  chl
#>   3   1  bmi  hyp  chl
#>   3   2  bmi  hyp  chl
#>   3   3  bmi  hyp  chl
#>   3   4  bmi  hyp  chl
#>   3   5  bmi  hyp  chl
#>   4   1  bmi  hyp  chl
#>   4   2  bmi  hyp  chl
#>   4   3  bmi  hyp  chl
#>   4   4  bmi  hyp  chl
#>   4   5  bmi  hyp  chl
#>   5   1  bmi  hyp  chl
#>   5   2  bmi  hyp  chl
#>   5   3  bmi  hyp  chl
#>   5   4  bmi  hyp  chl
#>   5   5  bmi  hyp  chl
fit <- with(data = imp, exp = lm(bmi ~ age + hyp + chl))
pooled <- mice::pool(fit)

model_parameters(pooled)
#> # Fixed Effects
#> 
#> Parameter   | Coefficient |   SE |          95% CI | Statistic |    df |      p
#> -------------------------------------------------------------------------------
#> (Intercept) |       19.05 | 3.39 | [ 11.82, 26.28] |      5.61 | 15.19 | < .001
#> age40-59    |       -4.97 | 1.86 | [ -9.02, -0.92] |     -2.67 | 12.12 | 0.020 
#> age60-99    |       -6.14 | 1.89 | [-10.15, -2.12] |     -3.25 | 15.35 | 0.005 
#> hypyes      |        2.11 | 2.29 | [ -3.38,  7.59] |      0.92 |  6.60 | 0.390 
#> chl         |        0.05 | 0.02 | [  0.01,  0.09] |      2.83 | 15.74 | 0.012 
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a Wald distribution approximation.
```

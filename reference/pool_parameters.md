# Pool Model Parameters

This function "pools" (i.e. combines) model parameters in a similar
fashion as
[`mice::pool()`](https://amices.org/mice/reference/pool.html). However,
this function pools parameters from `parameters_model` objects, as
returned by
[`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md).

## Usage

``` r
pool_parameters(
  x,
  exponentiate = FALSE,
  effects = "fixed",
  component = "all",
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  A list of `parameters_model` objects, as returned by
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md),
  or a list of model-objects that is supported by
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md).

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

  Which type of parameters to return, such as parameters for the
  conditional model, the zero-inflation part of the model, the
  dispersion term, or other auxiliary parameters be returned? Applies to
  models with zero-inflation and/or dispersion formula, or if parameters
  such as `sigma` should be included. May be abbreviated. Note that the
  *conditional* component is also called *count* or *mean* component,
  depending on the model. There are three convenient shortcuts:
  `component = "all"` returns all possible parameters. If
  `component = "location"`, location parameters such as `conditional`,
  `zero_inflated`, or `smooth_terms`, are returned (everything that are
  fixed or random effects - depending on the `effects` argument - but no
  auxiliary parameters). For `component = "distributional"` (or
  `"auxiliary"`), components like `sigma`, `dispersion`, or `beta` (and
  other auxiliary parameters) are returned.

- verbose:

  Toggle warnings and messages.

- ...:

  Arguments passed down to
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md),
  if `x` is a list of model-objects. Can be used, for instance, to
  specify arguments like `ci` or `ci_method` etc.

## Value

A data frame of indices related to the model's parameters.

## Details

Averaging of parameters follows Rubin's rules (*Rubin, 1987, p. 76*).
The pooled degrees of freedom is based on the Barnard-Rubin adjustment
for small samples (*Barnard and Rubin, 1999*).

## Note

Models with multiple components, (for instance, models with
zero-inflation, where predictors appear in the count and zero-inflation
part, or models with dispersion component) may fail in rare situations.
In this case, compute the pooled parameters for components separately,
using the `component` argument.

Some model objects do not return standard errors (e.g. objects of class
`htest`). For these models, no pooled confidence intervals nor p-values
are returned.

## References

Barnard, J. and Rubin, D.B. (1999). Small sample degrees of freedom with
multiple imputation. Biometrika, 86, 948-955. Rubin, D.B. (1987).
Multiple Imputation for Nonresponse in Surveys. New York: John Wiley and
Sons.

## Examples

``` r
# example for multiple imputed datasets
data("nhanes2", package = "mice")
imp <- mice::mice(nhanes2, printFlag = FALSE)
models <- lapply(1:5, function(i) {
  lm(bmi ~ age + hyp + chl, data = mice::complete(imp, action = i))
})
pool_parameters(models)
#> # Fixed Effects
#> 
#> Parameter   | Coefficient |   SE |          95% CI | Statistic |    df |      p
#> -------------------------------------------------------------------------------
#> (Intercept) |       19.96 | 3.76 | [ 11.95, 27.97] |      5.31 | 15.06 | < .001
#> age [40-59] |       -4.86 | 2.55 | [-11.19,  1.46] |     -1.90 |  5.72 | 0.108 
#> age [60-99] |       -7.04 | 2.26 | [-11.99, -2.10] |     -3.12 | 11.39 | 0.009 
#> hyp [yes]   |        2.28 | 2.07 | [ -2.20,  6.76] |      1.10 | 12.91 | 0.292 
#> chl         |        0.05 | 0.02 | [ -0.01,  0.10] |      2.07 |  8.45 | 0.070 
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a Wald distribution approximation.

# should be identical to:
m <- with(data = imp, exp = lm(bmi ~ age + hyp + chl))
summary(mice::pool(m))
#>          term    estimate  std.error statistic        df      p.value
#> 1 (Intercept) 19.96266999 3.75992725  5.309323 15.055044 0.0000864194
#> 2    age40-59 -4.86207490 2.55353981 -1.904053  5.717305 0.1079765082
#> 3    age60-99 -7.04168206 2.25570598 -3.121720 11.387585 0.0093505414
#> 4      hypyes  2.28037689 2.07344788  1.099799 12.907172 0.2915095255
#> 5         chl  0.04846359 0.02342499  2.068885  8.451932 0.0704917477

# For glm, mice used residual df, while `pool_parameters()` uses `Inf`
nhanes2$hyp <- datawizard::slide(as.numeric(nhanes2$hyp))
imp <- mice::mice(nhanes2, printFlag = FALSE)
models <- lapply(1:5, function(i) {
  glm(hyp ~ age + chl, family = binomial, data = mice::complete(imp, action = i))
})
m <- with(data = imp, exp = glm(hyp ~ age + chl, family = binomial))
# residual df
summary(mice::pool(m))$df
#> [1] 19.24998 19.25000 19.25000 10.88074
# df = Inf
pool_parameters(models)$df_error
#> [1] Inf Inf Inf Inf
# use residual df instead
pool_parameters(models, ci_method = "residual")$df_error
#> [1] 19.24807 19.24807 19.24807 10.88074
```

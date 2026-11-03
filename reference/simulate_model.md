# Simulated draws from model coefficients

Simulate draws from a statistical model to return a data frame of
estimates.

## Usage

``` r
simulate_model(model, iterations = 1000, ...)

# Default S3 method
simulate_model(model, iterations = 1000, component = "all", ...)
```

## Arguments

- model:

  Statistical model (no Bayesian models).

- iterations:

  The number of draws to simulate/bootstrap.

- ...:

  Arguments passed to
  [`insight::get_varcov()`](https://easystats.github.io/insight/reference/get_varcov.html),
  e.g. to allow simulated draws to be based on heteroscedasticity
  consistent variance covariance matrices.

- component:

  Should all parameters, parameters for the conditional model, for the
  zero-inflation part of the model, or the dispersion model be returned?
  Applies to models with zero-inflation and/or dispersion component.
  `component` may be one of `"conditional"`, `"zi"`, `"zero-inflated"`,
  `"dispersion"` or `"all"` (default). May be abbreviated.

## Value

A data frame.

## Details

### Technical Details

`simulate_model()` is a computationally faster alternative to
[`bootstrap_model()`](https://easystats.github.io/parameters/reference/bootstrap_model.md).
Simulated draws for coefficients are based on a multivariate normal
distribution
([`MASS::mvrnorm()`](https://rdrr.io/pkg/MASS/man/mvrnorm.html)) with
mean `mu = coef(model)` and variance `Sigma = vcov(model)`.

### Models with Zero-Inflation Component

For models from packages **glmmTMB**, **pscl**, **GLMMadaptive** and
**countreg**, the `component` argument can be used to specify which
parameters should be simulated. For all other models, parameters from
the conditional component (fixed effects) are simulated. This may
include smooth terms, but not random effects.

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

[`simulate_parameters()`](https://easystats.github.io/parameters/reference/simulate_parameters.md),
[`bootstrap_model()`](https://easystats.github.io/parameters/reference/bootstrap_model.md),
[`bootstrap_parameters()`](https://easystats.github.io/parameters/reference/bootstrap_parameters.md)

## Examples

``` r
model <- lm(Sepal.Length ~ Species * Petal.Width + Petal.Length, data = iris)
head(simulate_model(model))
#>   (Intercept) Speciesversicolor Speciesvirginica Petal.Width Petal.Length
#> 1    3.382625        -1.4663633        -2.050335   0.5031319    1.0297475
#> 2    3.669437        -1.7310700        -1.724811   0.7517460    0.7874629
#> 3    3.780805        -2.2502703        -2.154316   0.1565332    0.8179031
#> 4    3.656400        -1.6891191        -1.614304   0.5314344    0.8339277
#> 5    3.584545        -0.9983896        -2.042572   0.6975939    0.8601110
#> 6    3.594915        -0.9696320        -2.181565   0.8143316    0.8455198
#>   Speciesversicolor:Petal.Width Speciesvirginica:Petal.Width
#> 1                    -0.7918611                  -0.69285229
#> 2                    -0.2774898                  -0.62658240
#> 3                     0.4923955                   0.04443255
#> 4                    -0.2165162                  -0.58010541
#> 5                    -0.9396842                  -0.55574242
#> 6                    -0.9465295                  -0.55340520
# \donttest{
if (require("glmmTMB", quietly = TRUE)) {
  model <- glmmTMB(
    count ~ spp + mined + (1 | site),
    ziformula = ~mined,
    family = poisson(),
    data = Salamanders
  )
  head(simulate_model(model))
  head(simulate_model(model, component = "zero_inflated"))
}
#>   (Intercept)   minedno
#> 1   0.6561823 -1.744507
#> 2   0.4746029 -1.369783
#> 3   1.4623456 -2.259492
#> 4   0.8261901 -1.732386
#> 5   0.5340982 -1.501742
#> 6   0.5029579 -1.536482
# }
```

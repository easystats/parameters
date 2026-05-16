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
#> 1    3.574012         -1.091687        -2.645968   0.7228058    0.9229091
#> 2    3.576123         -1.156846        -2.692941   0.1918780    0.9706653
#> 3    3.457579         -1.176478        -2.415211   0.2099919    1.0239659
#> 4    3.145392         -1.480111        -2.225002   0.4251364    1.1602398
#> 5    3.625043         -1.035086        -1.547959   0.6903076    0.7974763
#> 6    3.518336         -1.321314        -2.392758   0.2483926    0.9441750
#>   Speciesversicolor:Petal.Width Speciesvirginica:Petal.Width
#> 1                    -1.0613760                  -0.43376143
#> 2                    -0.6168883                  -0.08876931
#> 3                    -0.7750784                  -0.34299438
#> 4                    -0.9273998                  -0.77380313
#> 5                    -0.6892322                  -0.68604660
#> 6                    -0.4205619                  -0.16306516
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
#> 1   1.0099605 -1.907060
#> 2   0.7995378 -2.068735
#> 3   0.8868391 -2.022511
#> 4   0.4257828 -1.537295
#> 5   0.9587299 -2.046318
#> 6   0.5438031 -1.481766
# }
```

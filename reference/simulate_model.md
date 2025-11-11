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
#> 1    3.219370        -1.3376489        -2.226057  1.96599191    0.9234830
#> 2    3.779695        -1.3414292        -2.065956  0.09107919    0.8758647
#> 3    3.670098        -1.2069177        -2.058668 -0.16998584    0.9107133
#> 4    3.721613        -1.4275981        -1.658296  0.44183272    0.8321936
#> 5    3.602876        -0.7660408        -1.725513  0.73505523    0.8151623
#> 6    3.488988        -1.3894220        -2.279503  0.75510498    0.9256022
#>   Speciesversicolor:Petal.Width Speciesvirginica:Petal.Width
#> 1                    -1.8547242                  -1.76767827
#> 2                    -0.3256344                  -0.08092977
#> 3                    -0.1118297                   0.14086821
#> 4                    -0.3232439                  -0.50010489
#> 5                    -1.0121992                  -0.63335677
#> 6                    -0.8598560                  -0.65672958
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
#> 1   0.4177734 -1.369456
#> 2   0.7725786 -1.836800
#> 3   0.7451907 -1.645524
#> 4   1.0511376 -2.239770
#> 5   0.7095631 -1.971235
#> 6   0.4580860 -1.468058
# }
```

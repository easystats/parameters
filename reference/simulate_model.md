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
#> 1    3.390504        -0.5260311       -2.4758805   0.8199033    0.9728208
#> 2    3.623282        -1.1121182       -2.3569496   0.1283160    0.9507441
#> 3    3.486585        -1.6481011       -2.3061233   0.5653314    0.9498825
#> 4    3.728747        -1.5571359       -0.8126597   0.1169709    0.8665544
#> 5    3.540501        -1.1946322       -2.4926423   0.7001382    0.9061457
#> 6    3.095856        -0.9117854       -1.8655495   1.3124308    1.0078892
#>   Speciesversicolor:Petal.Width Speciesvirginica:Petal.Width
#> 1                   -1.60159572                   -0.6975208
#> 2                   -0.65508107                   -0.1181986
#> 3                   -0.56737132                   -0.4861060
#> 4                   -0.08255035                   -0.6523850
#> 5                   -0.82194455                   -0.4302238
#> 6                   -1.69926524                   -1.4803751
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
#> 1   0.9243217 -1.941875
#> 2   0.3636440 -1.318482
#> 3   0.6219404 -1.595636
#> 4   0.9595035 -2.073931
#> 5   0.6756142 -2.087996
#> 6   0.6212309 -1.647160
# }
```

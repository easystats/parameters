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
#> 1    3.715178        -1.4083652        -2.119164   0.4733913    0.8291835
#> 2    3.557501        -0.9346293        -2.199960   0.5011723    0.9132586
#> 3    3.193264        -0.9808700        -2.281484   0.5934654    1.0664827
#> 4    3.260074        -1.4108927        -2.190112   0.7369086    1.0615367
#> 5    3.797678        -1.1108720        -1.790113  -0.1740471    0.8473906
#> 6    3.598220        -0.6319452        -1.646516   0.7522226    0.8235530
#>   Speciesversicolor:Petal.Width Speciesvirginica:Petal.Width
#> 1                   -0.42090265                   -0.3104572
#> 2                   -0.93883516                   -0.3984750
#> 3                   -1.18233756                   -0.7161228
#> 4                   -1.09005353                   -0.8902099
#> 5                   -0.09031684                    0.1225236
#> 6                   -1.15360201                   -0.6926479
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
#> 1   0.6348544 -1.639215
#> 2   1.1286780 -2.304267
#> 3   0.7141747 -1.856630
#> 4   0.8240554 -1.950274
#> 5   0.9187570 -2.094780
#> 6   0.6462241 -1.521267
# }
```

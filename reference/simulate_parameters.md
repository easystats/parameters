# Simulate Model Parameters

Compute simulated draws of parameters and their related indices such as
Confidence Intervals (CI) and p-values. Simulating parameter draws can
be seen as a (computationally faster) alternative to bootstrapping.

## Usage

``` r
simulate_parameters(model, ...)

# Default S3 method
simulate_parameters(
  model,
  iterations = 1000,
  centrality = "median",
  ci = 0.95,
  ci_method = "quantile",
  test = "p-value",
  ...
)
```

## Arguments

- model:

  Statistical model (no Bayesian models).

- ...:

  Arguments passed to
  [`insight::get_varcov()`](https://easystats.github.io/insight/reference/get_varcov.html),
  e.g. to allow simulated draws to be based on heteroscedasticity
  consistent variance covariance matrices.

- iterations:

  The number of draws to simulate/bootstrap.

- centrality:

  The point-estimates (centrality indices) to compute. Character
  (vector) or list with one or more of these options: `"median"`,
  `"mean"`, `"MAP"` (see
  [`map_estimate()`](https://easystats.github.io/bayestestR/reference/map_estimate.html)),
  `"trimmed"` (which is just `mean(x, trim = threshold)`), `"mode"` or
  `"all"`.

- ci:

  Value or vector of probability of the CI (between 0 and 1) to be
  estimated. Default to `0.95` (`95%`).

- ci_method:

  The type of index used for Credible Interval. Can be `"ETI"` (default,
  see
  [`eti()`](https://easystats.github.io/bayestestR/reference/eti.html)),
  `"HDI"` (see
  [`hdi()`](https://easystats.github.io/bayestestR/reference/hdi.html)),
  `"BCI"` (see
  [`bci()`](https://easystats.github.io/bayestestR/reference/bci.html)),
  `"SPI"` (see
  [`spi()`](https://easystats.github.io/bayestestR/reference/spi.html)),
  or `"SI"` (see
  [`si()`](https://easystats.github.io/bayestestR/reference/si.html)).

- test:

  The indices of effect existence to compute. Character (vector) or list
  with one or more of these options: `"p_direction"` (or `"pd"`),
  `"rope"`, `"p_map"`, `"p_significance"` (or `"ps"`), `"p_rope"`,
  `"equivalence_test"` (or `"equitest"`), `"bayesfactor"` (or `"bf"`) or
  `"all"` to compute all tests. For each "test", the corresponding
  **bayestestR** function is called (e.g.
  [`rope()`](https://easystats.github.io/bayestestR/reference/rope.html)
  or
  [`p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.html))
  and its results included in the summary output.

## Value

A data frame with simulated parameters.

## Details

### Technical Details

`simulate_parameters()` is a computationally faster alternative to
[`bootstrap_parameters()`](https://easystats.github.io/parameters/reference/bootstrap_parameters.md).
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

## Note

There is also a
[`plot()`-method](https://easystats.github.io/see/articles/parameters.html)
implemented in the [**see**-package](https://easystats.github.io/see/).

## References

Gelman A, Hill J. Data analysis using regression and
multilevel/hierarchical models. Cambridge; New York: Cambridge
University Press 2007: 140-143

## See also

[`bootstrap_model()`](https://easystats.github.io/parameters/reference/bootstrap_model.md),
[`bootstrap_parameters()`](https://easystats.github.io/parameters/reference/bootstrap_parameters.md),
[`simulate_model()`](https://easystats.github.io/parameters/reference/simulate_model.md)

## Examples

``` r
model <- lm(Sepal.Length ~ Species * Petal.Width + Petal.Length, data = iris)
simulate_parameters(model)
#> # Fixed Effects
#> 
#> Parameter                     | Coefficient |         95% CI |      p
#> ---------------------------------------------------------------------
#> (Intercept)                   |        3.53 | [ 3.22,  3.85] | < .001
#> Speciesversicolor             |       -1.13 | [-1.82, -0.49] | < .001
#> Speciesvirginica              |       -2.25 | [-3.10, -1.40] | < .001
#> Petal.Width                   |        0.44 | [-0.48,  1.26] | 0.346 
#> Petal.Length                  |        0.94 | [ 0.78,  1.09] | < .001
#> Speciesversicolor:Petal.Width |       -0.76 | [-1.76,  0.28] | 0.132 
#> Speciesvirginica:Petal.Width  |       -0.39 | [-1.31,  0.59] | 0.420 
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a simulated multivariate normal distribution
#>   approximation.

# \donttest{
if (require("glmmTMB", quietly = TRUE)) {
  model <- glmmTMB(
    count ~ spp + mined + (1 | site),
    ziformula = ~mined,
    family = poisson(),
    data = Salamanders
  )
  simulate_parameters(model, centrality = "mean")
  simulate_parameters(model, ci = c(.8, .95), component = "zero_inflated")
}
#> # Fixed Effects
#> 
#> Parameter   | Coefficient |         80% CI |         95% CI |      p
#> --------------------------------------------------------------------
#> (Intercept) |        0.80 | [ 0.45,  1.15] | [ 0.31,  1.33] | < .001
#> minedno     |       -1.85 | [-2.28, -1.45] | [-2.49, -1.29] | < .001
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a MCMC distribution approximation.
# }
```

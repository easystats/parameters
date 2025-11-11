# Parameters bootstrapping

Compute bootstrapped parameters and their related indices such as
Confidence Intervals (CI) and p-values.

## Usage

``` r
bootstrap_parameters(model, ...)

# Default S3 method
bootstrap_parameters(
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

  Statistical model.

- ...:

  Arguments passed to other methods, like
  [`bootstrap_model()`](https://easystats.github.io/parameters/reference/bootstrap_model.md)
  or
  [`bayestestR::describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.html).

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

  The indices to compute. Character (vector) with one or more of these
  options: `"p-value"` (or `"p"`), `"p_direction"` (or `"pd"`),
  `"rope"`, `"p_map"`, `"equivalence_test"` (or `"equitest"`),
  `"bayesfactor"` (or `"bf"`) or `"all"` to compute all tests. For each
  "test", the corresponding **bayestestR** function is called (e.g.
  [`bayestestR::rope()`](https://easystats.github.io/bayestestR/reference/rope.html)
  or
  [`bayestestR::p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.html))
  and its results included in the summary output.

## Value

A data frame summarizing the bootstrapped parameters.

## Details

This function first calls
[`bootstrap_model()`](https://easystats.github.io/parameters/reference/bootstrap_model.md)
to generate bootstrapped coefficients. The resulting replicated for each
coefficient are treated as "distribution", and is passed to
[`bayestestR::describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.html)
to calculate the related indices defined in the `"test"` argument.

Note that that p-values returned here are estimated under the assumption
of *translation equivariance*: that shape of the sampling distribution
is unaffected by the null being true or not. If this assumption does not
hold, p-values can be biased, and it is suggested to use proper
permutation tests to obtain non-parametric p-values.

## Using with **emmeans**

The output can be passed directly to the various functions from the
**emmeans** package, to obtain bootstrapped estimates, contrasts, simple
slopes, etc. and their confidence intervals. These can then be passed to
`model_parameter()` to obtain standard errors, p-values, etc. (see
example).

Note that that p-values returned here are estimated under the assumption
of *translation equivariance*: that shape of the sampling distribution
is unaffected by the null being true or not. If this assumption does not
hold, p-values can be biased, and it is suggested to use proper
permutation tests to obtain non-parametric p-values.

## References

Davison, A. C., & Hinkley, D. V. (1997). Bootstrap methods and their
application (Vol. 1). Cambridge university press.

## See also

[`bootstrap_model()`](https://easystats.github.io/parameters/reference/bootstrap_model.md),
[`simulate_parameters()`](https://easystats.github.io/parameters/reference/simulate_parameters.md),
[`simulate_model()`](https://easystats.github.io/parameters/reference/simulate_model.md)

## Examples

``` r
# \donttest{
set.seed(2)
model <- lm(Sepal.Length ~ Species * Petal.Width, data = iris)
b <- bootstrap_parameters(model)
print(b)
#> # Fixed Effects
#> 
#> Parameter                     | Coefficient |        95% CI |      p
#> --------------------------------------------------------------------
#> (Intercept)                   |        4.78 | [ 4.50, 5.00] | < .001
#> Speciesversicolor             |       -0.72 | [-1.62, 0.08] | 0.082 
#> Speciesvirginica              |        0.50 | [-0.67, 1.65] | 0.422 
#> Petal.Width                   |        0.91 | [ 0.22, 1.97] | 0.016 
#> Speciesversicolor:Petal.Width |        0.50 | [-0.66, 1.52] | 0.390 
#> Speciesvirginica:Petal.Width  |       -0.27 | [-1.36, 0.67] | 0.558 

# different type of bootstrapping
set.seed(2)
b <- bootstrap_parameters(model, type = "balanced")
print(b)
#> # Fixed Effects
#> 
#> Parameter                     | Coefficient |        95% CI |      p
#> --------------------------------------------------------------------
#> (Intercept)                   |        4.77 | [ 4.53, 5.00] | < .001
#> Speciesversicolor             |       -0.73 | [-1.67, 0.05] | 0.076 
#> Speciesvirginica              |        0.53 | [-0.71, 1.74] | 0.428 
#> Petal.Width                   |        0.93 | [ 0.24, 1.86] | 0.020 
#> Speciesversicolor:Petal.Width |        0.49 | [-0.56, 1.47] | 0.366 
#> Speciesvirginica:Petal.Width  |       -0.29 | [-1.34, 0.65] | 0.560 

est <- emmeans::emmeans(b, trt.vs.ctrl ~ Species)
#> NOTE: Results may be misleading due to involvement in interactions
print(model_parameters(est))
#> # Estimated Marginal Means 
#> 
#> Parameter  | Median |       95% CI |   pd
#> -----------------------------------------
#> setosa     |   5.89 | [5.25, 6.78] | 100%
#> versicolor |   5.75 | [5.63, 5.89] | 100%
#> virginica  |   6.05 | [5.52, 6.60] | 100%
#> 
#> # Contrasts 
#> 
#> Parameter           | Median |        95% CI |     pd
#> -----------------------------------------------------
#> versicolor - setosa |  -0.14 | [-1.04, 0.53] | 64.20%
#> virginica - setosa  |   0.13 | [-0.87, 1.01] | 60.10%
# }
```

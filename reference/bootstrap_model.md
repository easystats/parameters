# Model bootstrapping

Bootstrap a statistical model n times to return a data frame of
estimates.

## Usage

``` r
bootstrap_model(model, iterations = 1000, ...)

# Default S3 method
bootstrap_model(
  model,
  iterations = 1000,
  type = "ordinary",
  parallel = "no",
  n_cpus = 1,
  cluster = NULL,
  verbose = FALSE,
  ...
)
```

## Arguments

- model:

  Statistical model.

- iterations:

  The number of draws to simulate/bootstrap.

- ...:

  Arguments passed to or from other methods.

- type:

  Character string specifying the type of bootstrap. For mixed models of
  class `merMod` or `glmmTMB`, may be `"parametric"` (default) or
  `"semiparametric"` (see
  [`?lme4::bootMer`](https://rdrr.io/pkg/lme4/man/bootMer.html) for
  details). For all other models, see argument `sim` in
  [`?boot::boot`](https://rdrr.io/pkg/boot/man/boot.html) (defaults to
  `"ordinary"`).

- parallel:

  The type of parallel operation to be used (if any).

- n_cpus:

  Number of processes to be used in parallel operation.

- cluster:

  Optional cluster when `parallel = "snow"`. See
  [`?lme4::bootMer`](https://rdrr.io/pkg/lme4/man/bootMer.html) for
  details.

- verbose:

  Toggle warnings and messages.

## Value

A data frame of bootstrapped estimates.

## Details

By default, [`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html) is
used to generate bootstraps from the model data, which are then used to
[`update()`](https://rdrr.io/r/stats/update.html) the model, i.e. refit
the model with the bootstrapped samples. For `merMod` objects (**lme4**)
or models from **glmmTMB**, the
[`lme4::bootMer()`](https://rdrr.io/pkg/lme4/man/bootMer.html) function
is used to obtain bootstrapped samples.
[`bootstrap_parameters()`](https://easystats.github.io/parameters/reference/bootstrap_parameters.md)
summarizes the bootstrapped model estimates.

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

## See also

[`bootstrap_parameters()`](https://easystats.github.io/parameters/reference/bootstrap_parameters.md),
[`simulate_model()`](https://easystats.github.io/parameters/reference/simulate_model.md),
[`simulate_parameters()`](https://easystats.github.io/parameters/reference/simulate_parameters.md)

## Examples

``` r
# \donttest{
model <- lm(mpg ~ wt + factor(cyl), data = mtcars)
b <- bootstrap_model(model)
print(head(b))
#>   (Intercept)        wt factor(cyl)6 factor(cyl)8
#> 1    33.50110 -3.043621    -4.561783    -6.336212
#> 2    33.73882 -3.031721    -4.529576    -6.089992
#> 3    30.77663 -2.298175    -4.044469    -5.561024
#> 4    32.50119 -2.431631    -5.943601    -7.659333
#> 5    31.10429 -2.486001    -4.280528    -5.875406
#> 6    35.65625 -3.537588    -5.020713    -6.596277

est <- emmeans::emmeans(b, consec ~ cyl)
print(model_parameters(est))
#> # Estimated Marginal Means 
#> 
#> Parameter | Median |         95% CI |   pd
#> ------------------------------------------
#> 4         |  23.47 | [21.35, 26.20] | 100%
#> 6         |  19.39 | [18.61, 20.31] | 100%
#> 8         |  17.55 | [16.42, 19.22] | 100%
#> 
#> # Contrasts 
#> 
#> Parameter   | Median |         95% CI |     pd
#> ----------------------------------------------
#> cyl6 - cyl4 |  -4.08 | [-6.85, -1.92] | 99.90%
#> cyl8 - cyl6 |  -1.81 | [-3.32,  0.12] | 97.10%
# }
```

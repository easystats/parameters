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
  class `merMod` or `glmmTMB`, may be `"parametric"` (default). For
  `merMod` models only, may also be `"semiparametric"` (see
  [`?lme4::bootMer`](https://rdrr.io/pkg/lme4/man/bootMer.html) for
  details). Semiparametric bootstrapping resamples the residuals and
  keeps the random effects at their estimated values (`use.u = TRUE` in
  [`lme4::bootMer()`](https://rdrr.io/pkg/lme4/man/bootMer.html)). For
  all other models, see argument `sim` in
  [`?boot::boot`](https://rdrr.io/pkg/boot/man/boot.html) (defaults to
  `"ordinary"`). For models without their own method, including other
  mixed models, `"parametric"` draws new responses with
  [`simulate()`](https://rdrr.io/r/stats/simulate.html) and refits the
  model to them. Two kinds of model give an error. For the first kind,
  [`simulate()`](https://rdrr.io/r/stats/simulate.html) has no method,
  fails, or does not return one numeric response per row. For the second
  kind, the response in the formula is not one column of the data, for
  example `log(y)` or [`cbind()`](https://rdrr.io/r/base/cbind.html).
  For other mixed models, `"ordinary"` and `"balanced"` resample whole
  clusters (see section Details), and `"permutation"` and `"antithetic"`
  are not available.

- parallel:

  The type of parallel operation to be used (if any).

- n_cpus:

  Number of processes to be used in parallel operation.

- cluster:

  Optional cluster when `parallel = "snow"`. See
  [`?lme4::bootMer`](https://rdrr.io/pkg/lme4/man/bootMer.html) or
  [`?boot::boot`](https://rdrr.io/pkg/boot/man/boot.html) for details.

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

Other mixed models (for example from **ordinal**, **cplm** or **nlme**)
use a cluster bootstrap when `type` is `"ordinary"` or `"balanced"`:
each sample draws whole clusters (the levels of the grouping factor)
with replacement, not single rows. A cluster that is drawn twice counts
as two clusters. If the model has several grouping factors that are all
nested in one factor, directly or through other factors, the clusters
are the levels of that factor. If several factors qualify, the one with
the most levels that is not a fixed effect is used. Grouping factors
that are not nested in one factor give an error. A grouping factor that
is also a fixed effect gives an error, unless the formula has it only in
interactions with the resampled factor (for example `cond` in
`random = ~ 1 | subject/cond`, which has the grouping terms `subject`
and `subject:cond`). With few clusters, the cluster bootstrap has few
independent units to resample, and its standard errors can be too small.

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
#> 1    32.80447 -3.275128    -2.994874    -5.535547
#> 2    33.53835 -3.192280    -3.371982    -5.269348
#> 3    33.50110 -3.043621    -4.561783    -6.336212
#> 4    33.73882 -3.031721    -4.529576    -6.089992
#> 5    30.77663 -2.298175    -4.044469    -5.561024
#> 6    32.50119 -2.431631    -5.943601    -7.659333

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
#> cyl8 - cyl6 |  -1.81 | [-3.31,  0.12] | 97.10%
# }
```

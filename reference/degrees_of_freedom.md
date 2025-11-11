# Degrees of Freedom (DoF)

Estimate or extract degrees of freedom of models parameters.

## Usage

``` r
degrees_of_freedom(model, method = "analytical", ...)

dof(model, method = "analytical", ...)
```

## Arguments

- model:

  A statistical model.

- method:

  Type of approximation for the degrees of freedom. Can be one of the
  following:

  - `"residual"` (aka `"analytical"`) returns the residual degrees of
    freedom, which usually is what
    [`stats::df.residual()`](https://rdrr.io/r/stats/df.residual.html)
    returns. If a model object has no method to extract residual degrees
    of freedom, these are calculated as `n-p`, i.e. the number of
    observations minus the number of estimated parameters. If residual
    degrees of freedom cannot be extracted by either approach, returns
    `Inf`.

  - `"wald"` returns residual (aka analytical) degrees of freedom for
    models with t-statistic, `1` for models with Chi-squared statistic,
    and `Inf` for all other models. Also returns `Inf` if residual
    degrees of freedom cannot be extracted.

  - `"normal"` always returns `Inf`.

  - `"model"` returns model-based degrees of freedom, i.e. the number of
    (estimated) parameters.

  - For mixed models, can also be `"ml1"` (or `"m-l-1"`, approximation
    of degrees of freedom based on a "m-l-1" heuristic as suggested by
    *Elff et al. 2019*) or `"between-within"` (or `"betwithin"`).

  - For mixed models of class `merMod`, `type` can also be
    `"satterthwaite"` or `"kenward-roger"` (or `"kenward"`). See
    'Details'.

  Usually, when degrees of freedom are required to calculate p-values or
  confidence intervals, `type = "wald"` is likely to be the best choice
  in most cases.

- ...:

  Currently not used.

## Note

In many cases, `degrees_of_freedom()` returns the same as
`df.residuals()`, or `n-k` (number of observations minus number of
parameters). However, `degrees_of_freedom()` refers to the model's
*parameters* degrees of freedom of the distribution for the related test
statistic. Thus, for models with z-statistic, results from
`degrees_of_freedom()` and `df.residuals()` differ. Furthermore, for
other approximation methods like `"kenward"` or `"satterthwaite"`, each
model parameter can have a different degree of freedom.

## Examples

``` r
model <- lm(Sepal.Length ~ Petal.Length * Species, data = iris)
dof(model)
#> [1] 144

model <- glm(vs ~ mpg * cyl, data = mtcars, family = "binomial")
dof(model)
#> [1] 28
# \donttest{
model <- lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
dof(model)
#> [1] 146

if (require("rstanarm", quietly = TRUE)) {
  model <- stan_glm(
    Sepal.Length ~ Petal.Length * Species,
    data = iris,
    chains = 2,
    refresh = 0
  )
  dof(model)
}
#> This is rstanarm version 2.32.2
#> - See https://mc-stan.org/rstanarm/articles/priors for changes to default priors!
#> - Default priors may change, so it's safest to specify priors, even if equivalent to the defaults.
#> - For execution on a local, multicore CPU with excess RAM we recommend calling
#>   options(mc.cores = parallel::detectCores())
#> 
#> Attaching package: ‘rstanarm’
#> The following object is masked from ‘package:psych’:
#> 
#>     logit
#> The following object is masked from ‘package:boot’:
#> 
#>     logit
#> The following object is masked from ‘package:parameters’:
#> 
#>     compare_models
#> [1] 144
# }
```

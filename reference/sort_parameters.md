# Sort parameters by coefficient values

Sort parameters by coefficient values

## Usage

``` r
sort_parameters(x, ...)

# Default S3 method
sort_parameters(x, sort = "none", column = "Coefficient", ...)
```

## Arguments

- x:

  A data frame or a `parameters_model` object.

- ...:

  Arguments passed to or from other methods.

- sort:

  If `"none"` (default) do not sort, `"ascending"` sort by increasing
  coefficient value, or `"descending"` sort by decreasing coefficient
  value.

- column:

  The column containing model parameter estimates. This will be
  `"Coefficient"` (default) in *easystats* packages, `"estimate"` in
  *broom* package, etc.

## Value

A sorted data frame or original object.

## Examples

``` r
# creating object to sort (can also be a regular data frame)
mod <- model_parameters(stats::lm(wt ~ am * cyl, data = mtcars))

# original output
mod
#> Parameter   | Coefficient |   SE |        95% CI | t(28) |      p
#> -----------------------------------------------------------------
#> (Intercept) |        1.66 | 0.59 | [ 0.46, 2.86] |  2.82 | 0.009 
#> am          |       -0.96 | 0.79 | [-2.58, 0.67] | -1.21 | 0.238 
#> cyl         |        0.30 | 0.08 | [ 0.13, 0.47] |  3.68 | < .001
#> am × cyl    |        0.03 | 0.13 | [-0.23, 0.30] |  0.25 | 0.803 
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a Wald t-distribution approximation.

# sorted outputs
sort_parameters(mod, sort = "ascending")
#> Parameter   | Coefficient |   SE |        95% CI | t(28) |      p
#> -----------------------------------------------------------------
#> am          |       -0.96 | 0.79 | [-2.58, 0.67] | -1.21 | 0.238 
#> am × cyl    |        0.03 | 0.13 | [-0.23, 0.30] |  0.25 | 0.803 
#> cyl         |        0.30 | 0.08 | [ 0.13, 0.47] |  3.68 | < .001
#> (Intercept) |        1.66 | 0.59 | [ 0.46, 2.86] |  2.82 | 0.009 
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a Wald t-distribution approximation.
sort_parameters(mod, sort = "descending")
#> Parameter   | Coefficient |   SE |        95% CI | t(28) |      p
#> -----------------------------------------------------------------
#> (Intercept) |        1.66 | 0.59 | [ 0.46, 2.86] |  2.82 | 0.009 
#> cyl         |        0.30 | 0.08 | [ 0.13, 0.47] |  3.68 | < .001
#> am × cyl    |        0.03 | 0.13 | [-0.23, 0.30] |  0.25 | 0.803 
#> am          |       -0.96 | 0.79 | [-2.58, 0.67] | -1.21 | 0.238 
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a Wald t-distribution approximation.
```

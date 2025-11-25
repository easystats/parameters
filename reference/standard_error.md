# Standard Errors

`standard_error()` attempts to return standard errors of model
parameters.

## Usage

``` r
standard_error(model, ...)

# Default S3 method
standard_error(
  model,
  effects = "fixed",
  component = "all",
  vcov = NULL,
  vcov_args = NULL,
  verbose = TRUE,
  ...
)

# S3 method for class 'factor'
standard_error(model, force = FALSE, verbose = TRUE, ...)
```

## Arguments

- model:

  A model.

- ...:

  Arguments passed to or from other methods.

- effects:

  Should standard errors for fixed effects (`"fixed"`), random effects
  (`"random"`), or both (`"all"`) be returned? Only applies to mixed
  models. May be abbreviated. When standard errors for random effects
  are requested, for each grouping factor a list of standard errors (per
  group level) for random intercepts and slopes is returned.

- component:

  Model component for which standard errors should be shown. See the
  documentation for your object's class in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  or
  [`p_value()`](https://easystats.github.io/parameters/reference/p_value.md)
  for further details.

- vcov:

  Variance-covariance matrix used to compute uncertainty estimates
  (e.g., for robust standard errors). This argument accepts a covariance
  matrix, a function which returns a covariance matrix, or a string
  which identifies the function to be used to compute the covariance
  matrix.

  - A covariance matrix

  - A function which returns a covariance matrix (e.g.,
    [`stats::vcov()`](https://rdrr.io/r/stats/vcov.html))

  - A string which indicates the kind of uncertainty estimates to
    return.

    - Heteroskedasticity-consistent: `"HC"`, `"HC0"`, `"HC1"`, `"HC2"`,
      `"HC3"`, `"HC4"`, `"HC4m"`, `"HC5"`. See
      [`?sandwich::vcovHC`](https://sandwich.R-Forge.R-project.org/reference/vcovHC.html)

    - Cluster-robust: `"CR"`, `"CR0"`, `"CR1"`, `"CR1p"`, `"CR1S"`,
      `"CR2"`, `"CR3"`. See
      [`?clubSandwich::vcovCR`](http://jepusto.github.io/clubSandwich/reference/vcovCR.md)

    - Bootstrap: `"BS"`, `"xy"`, `"residual"`, `"wild"`, `"mammen"`,
      `"fractional"`, `"jackknife"`, `"norm"`, `"webb"`. See
      [`?sandwich::vcovBS`](https://sandwich.R-Forge.R-project.org/reference/vcovBS.html)

    - Other `sandwich` package functions: `"HAC"`, `"PC"`, `"CL"`,
      `"OPG"`, `"PL"`.

- vcov_args:

  List of arguments to be passed to the function identified by the
  `vcov` argument. This function is typically supplied by the
  **sandwich** or **clubSandwich** packages. Please refer to their
  documentation (e.g.,
  [`?sandwich::vcovHAC`](https://sandwich.R-Forge.R-project.org/reference/vcovHAC.html))
  to see the list of available arguments. If no estimation type
  (argument `type`) is given, the default type for `"HC"` equals the
  default from the **sandwich** package; for type `"CR"`, the default is
  set to `"CR3"`.

- verbose:

  Toggle warnings and messages.

- force:

  Logical, if `TRUE`, factors are converted to numerical values to
  calculate the standard error, with the lowest level being the value
  `1` (unless the factor has numeric levels, which are converted to the
  corresponding numeric value). By default, `NA` is returned for factors
  or character vectors.

## Value

A data frame with at least two columns: the parameter names and the
standard errors. Depending on the model, may also include columns for
model components etc.

## Note

For Bayesian models (from **rstanarm** or **brms**), the standard error
is the SD of the posterior samples.

## Examples

``` r
model <- lm(Petal.Length ~ Sepal.Length * Species, data = iris)
standard_error(model)
#>                        Parameter        SE
#> 1                    (Intercept) 0.5310388
#> 2                   Sepal.Length 0.1058237
#> 3              Speciesversicolor 0.6836543
#> 4               Speciesvirginica 0.6578142
#> 5 Sepal.Length:Speciesversicolor 0.1281447
#> 6  Sepal.Length:Speciesvirginica 0.1209952

# robust standard errors
standard_error(model, vcov = "HC3")
#>                        Parameter         SE
#> 1                    (Intercept) 0.42486667
#> 2                   Sepal.Length 0.08504442
#> 3              Speciesversicolor 0.67252996
#> 4               Speciesvirginica 0.58942889
#> 5 Sepal.Length:Speciesversicolor 0.12045791
#> 6  Sepal.Length:Speciesvirginica 0.10558799

# cluster-robust standard errors
standard_error(model,
  vcov = "vcovCL",
  vcov_args = list(cluster = iris$Species)
)
#>                        Parameter           SE
#> 1                    (Intercept) 7.771519e-16
#> 2                   Sepal.Length 1.537244e-16
#> 3              Speciesversicolor 1.748165e-15
#> 4               Speciesvirginica 5.991370e-15
#> 5 Sepal.Length:Speciesversicolor 3.050322e-16
#> 6  Sepal.Length:Speciesvirginica 9.111411e-16
```

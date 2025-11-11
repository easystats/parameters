# Reshape loadings between wide/long formats

Reshape loadings between wide/long formats.

## Usage

``` r
reshape_loadings(x, ...)

# S3 method for class 'parameters_efa'
reshape_loadings(x, threshold = NULL, ...)

# S3 method for class 'data.frame'
reshape_loadings(x, threshold = NULL, loadings_columns = NULL, ...)
```

## Arguments

- x:

  A data frame or a statistical model. For
  [`closest_component()`](https://easystats.github.io/parameters/reference/principal_components.md),
  the output of the
  [`principal_components()`](https://easystats.github.io/parameters/reference/principal_components.md)
  function.

- ...:

  Arguments passed to or from other methods.

- threshold:

  A value between 0 and 1 indicates which (absolute) values from the
  loadings should be removed. An integer higher than 1 indicates the n
  strongest loadings to retain. Can also be `"max"`, in which case it
  will only display the maximum loading per variable (the most simple
  structure).

- loadings_columns:

  Vector indicating the columns corresponding to loadings.

## Examples

``` r
if (require("psych")) {
  pca <- model_parameters(psych::fa(attitude, nfactors = 3))
  loadings <- reshape_loadings(pca)

  loadings
  reshape_loadings(loadings)
}
#> Variable   |   MR1 |   MR2 |   MR3 | Complexity | Uniqueness
#> ------------------------------------------------------------
#> rating     |  0.90 | -0.07 | -0.05 |       1.02 |       0.23
#> complaints |  0.97 | -0.06 |  0.04 |       1.01 |       0.10
#> privileges |  0.44 |  0.25 | -0.05 |       1.64 |       0.65
#> learning   |  0.47 |  0.54 | -0.28 |       2.51 |       0.24
#> raises     |  0.55 |  0.43 |  0.25 |       2.35 |       0.23
#> critical   |  0.16 |  0.17 |  0.48 |       1.46 |       0.67
#> advance    | -0.11 |  0.91 |  0.07 |       1.04 |       0.22
```

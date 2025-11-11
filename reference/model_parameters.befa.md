# Parameters from Bayesian Exploratory Factor Analysis

Format Bayesian Exploratory Factor Analysis objects from the BayesFM
package.

## Usage

``` r
# S3 method for class 'befa'
model_parameters(
  model,
  sort = FALSE,
  centrality = "median",
  dispersion = FALSE,
  ci = 0.95,
  ci_method = "eti",
  test = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- model:

  Bayesian EFA created by the
  [`BayesFM::befa`](https://rdrr.io/pkg/BayesFM/man/befa.html).

- sort:

  Sort the loadings.

- centrality:

  The point-estimates (centrality indices) to compute. Character
  (vector) or list with one or more of these options: `"median"`,
  `"mean"`, `"MAP"` (see
  [`map_estimate()`](https://easystats.github.io/bayestestR/reference/map_estimate.html)),
  `"trimmed"` (which is just `mean(x, trim = threshold)`), `"mode"` or
  `"all"`.

- dispersion:

  Logical, if `TRUE`, computes indices of dispersion related to the
  estimate(s) (`SD` and `MAD` for `mean` and `median`, respectively).
  Dispersion is not available for `"MAP"` or `"mode"` centrality
  indices.

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

- verbose:

  Toggle warnings.

- ...:

  Arguments passed to or from other methods.

## Value

A data frame of loadings.

## Examples

``` r
library(parameters)
# \donttest{
if (require("BayesFM")) {
  efa <- BayesFM::befa(mtcars, iter = 1000)
  results <- model_parameters(efa, sort = TRUE, verbose = FALSE)
  results
  efa_to_cfa(results, verbose = FALSE)
}
#> Loading required package: BayesFM
#> starting MCMC sampling...
#>     5%
#>    10%
#>    15%
#>    20%
#>    25%
#>    30%
#>    35%
#>    40%
#>    45%
#> done with burn-in period
#>    50%
#>    55%
#>    60%
#>    65%
#>    70%
#>    75%
#>    80%
#>    85%
#>    90%
#>    95%
#>   100%
#> done with sampling!
#> # Latent variables
#> F1 =~ am + mpg + vs
#> F2 =~ carb + cyl + disp + hp + wt
#> F3 =~ drat + gear + qsec
# }
```

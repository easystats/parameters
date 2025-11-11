# Parameters from BayesFactor objects

Parameters from `BFBayesFactor` objects from `{BayesFactor}` package.

## Usage

``` r
# S3 method for class 'BFBayesFactor'
model_parameters(
  model,
  centrality = "median",
  dispersion = FALSE,
  ci = 0.95,
  ci_method = "eti",
  test = "pd",
  rope_range = "default",
  rope_ci = 0.95,
  priors = TRUE,
  es_type = NULL,
  include_proportions = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- model:

  Object of class `BFBayesFactor`.

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

- rope_range:

  ROPE's lower and higher bounds. Should be a vector of two values
  (e.g., `c(-0.1, 0.1)`), `"default"` or a list of numeric vectors of
  the same length as numbers of parameters. If `"default"`, the bounds
  are set to `x +- 0.1*SD(response)`.

- rope_ci:

  The Credible Interval (CI) probability, corresponding to the
  proportion of HDI, to use for the percentage in ROPE.

- priors:

  Add the prior used for each parameter.

- es_type:

  The effect size of interest. Not that possibly not all effect sizes
  are applicable to the model object. See 'Details'. For Anova models,
  can also be a character vector with multiple effect size names.

- include_proportions:

  Logical that decides whether to include posterior cell
  proportions/counts for Bayesian contingency table analysis (from
  [`BayesFactor::contingencyTableBF()`](https://rdrr.io/pkg/BayesFactor/man/contingencyTableBF.html)).
  Defaults to `FALSE`, as this information is often redundant.

- verbose:

  Toggle off warnings.

- ...:

  Additional arguments to be passed to or from methods.

## Value

A data frame of indices related to the model's parameters.

## Details

The meaning of the extracted parameters:

- For
  [`BayesFactor::ttestBF()`](https://rdrr.io/pkg/BayesFactor/man/ttestBF.html):
  `Difference` is the raw difference between the means.

- For
  [`BayesFactor::correlationBF()`](https://rdrr.io/pkg/BayesFactor/man/correlationBF.html):
  `rho` is the linear correlation estimate (equivalent to Pearson's
  *r*).

- For
  [`BayesFactor::lmBF()`](https://rdrr.io/pkg/BayesFactor/man/lmBF.html)
  /
  [`BayesFactor::generalTestBF()`](https://rdrr.io/pkg/BayesFactor/man/generalTestBF.html)
  /
  [`BayesFactor::regressionBF()`](https://rdrr.io/pkg/BayesFactor/man/regressionBF.html)
  /
  [`BayesFactor::anovaBF()`](https://rdrr.io/pkg/BayesFactor/man/anovaBF.html):
  in addition to parameters of the fixed and random effects, there are:
  `mu` is the (mean-centered) intercept; `sig2` is the model's sigma;
  `g` / `g_*` are the *g* parameters; See the *Bayes Factors for ANOVAs*
  paper
  ([doi:10.1016/j.jmp.2012.08.001](https://doi.org/10.1016/j.jmp.2012.08.001)
  ).

## Examples

``` r
# \donttest{
# Bayesian t-test
model <- BayesFactor::ttestBF(x = rnorm(100, 1, 1))
model_parameters(model)
#> Bayesian t-test
#> 
#> Parameter  | Median |       95% CI |   pd |              Prior |       BF
#> -------------------------------------------------------------------------
#> Difference |   0.94 | [0.76, 1.12] | 100% | Cauchy (0 +- 0.71) | 7.42e+14
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a MCMC distribution approximation.
model_parameters(model, es_type = "cohens_d", ci = 0.9)
#> Bayesian t-test
#> 
#> Parameter  | Median |       90% CI | Cohen's d |     d 90% CI |   pd
#> --------------------------------------------------------------------
#> Difference |   0.94 | [0.79, 1.09] |      1.04 | [0.83, 1.24] | 100%
#> 
#> Parameter  |              Prior |       BF
#> ------------------------------------------
#> Difference | Cauchy (0 +- 0.71) | 7.42e+14
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a MCMC distribution approximation.

# Bayesian contingency table analysis
data(raceDolls)
bf <- BayesFactor::contingencyTableBF(
  raceDolls,
  sampleType = "indepMulti",
  fixedMargin = "cols"
)
model_parameters(bf,
  centrality = "mean",
  dispersion = TRUE,
  verbose = FALSE,
  es_type = "cramers_v"
)
#> Bayesian contingency table analysis
#> 
#> Parameter |   SD | Cramer's V (adj.) | Cramers 95% CI
#> -----------------------------------------------------
#> Ratio     | 0.08 |              0.14 |   [0.00, 0.30]
#> 
#> Parameter |                            Prior |   BF
#> ---------------------------------------------------
#> Ratio     | Independent multinomial (0 +- 1) | 1.81
# }
```

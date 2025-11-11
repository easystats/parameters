# "m-l-1" approximation for SEs, CIs and p-values

Approximation of degrees of freedom based on a "m-l-1" heuristic as
suggested by Elff et al. (2019).

## Usage

``` r
ci_ml1(model, ci = 0.95, ...)

dof_ml1(model)

p_value_ml1(model, dof = NULL, ...)
```

## Arguments

- model:

  A mixed model.

- ci:

  Confidence Interval (CI) level. Default to `0.95` (`95%`).

- ...:

  Additional arguments passed down to the underlying functions. E.g.,
  arguments like `vcov` or `vcov_args` can be used to compute confidence
  intervals using a specific variance-covariance matrix for the standard
  errors.

- dof:

  Degrees of Freedom.

## Value

A data frame.

## Details

### Small Sample Cluster corrected Degrees of Freedom

Inferential statistics (like p-values, confidence intervals and standard
errors) may be biased in mixed models when the number of clusters is
small (even if the sample size of level-1 units is high). In such cases
it is recommended to approximate a more accurate number of degrees of
freedom for such inferential statistics (see *Li and Redden 2015*). The
*m-l-1* heuristic is such an approach that uses a t-distribution with
fewer degrees of freedom (`dof_ml1()`) to calculate p-values
(`p_value_ml1()`) and confidence intervals (`ci(method = "ml1")`).

### Degrees of Freedom for Longitudinal Designs (Repeated Measures)

In particular for repeated measure designs (longitudinal data analysis),
the *m-l-1* heuristic is likely to be more accurate than simply using
the residual or infinite degrees of freedom, because `dof_ml1()` returns
different degrees of freedom for within-cluster and between-cluster
effects.

### Limitations of the "m-l-1" Heuristic

Note that the "m-l-1" heuristic is not applicable (or at least less
accurate) for complex multilevel designs, e.g. with cross-classified
clusters. In such cases, more accurate approaches like the Kenward-Roger
approximation
([`dof_kenward()`](https://easystats.github.io/parameters/reference/p_value_kenward.md))
is recommended. However, the "m-l-1" heuristic also applies to
generalized mixed models, while approaches like Kenward-Roger or
Satterthwaite are limited to linear mixed models only.

## References

- Elff, M.; Heisig, J.P.; Schaeffer, M.; Shikano, S. (2019). Multilevel
  Analysis with Few Clusters: Improving Likelihood-based Methods to
  Provide Unbiased Estimates and Accurate Inference, British Journal of
  Political Science.

- Li, P., Redden, D. T. (2015). Comparing denominator degrees of freedom
  approximations for the generalized linear mixed model in analyzing
  binary outcome in small sample cluster-randomized trials. BMC Medical
  Research Methodology, 15(1), 38.
  [doi:10.1186/s12874-015-0026-x](https://doi.org/10.1186/s12874-015-0026-x)

## See also

`dof_ml1()` is a small helper-function to calculate approximated degrees
of freedom of model parameters, based on the "m-l-1" heuristic.

## Examples

``` r
# \donttest{
if (require("lme4")) {
  model <- lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
  p_value_ml1(model)
}
#>      Parameter          p
#> 1  (Intercept) 0.96504927
#> 2 Sepal.Length 0.04534945
# }
```

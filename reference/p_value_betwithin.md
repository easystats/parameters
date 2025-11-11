# Between-within approximation for SEs, CIs and p-values

Approximation of degrees of freedom based on a "between-within"
heuristic.

## Usage

``` r
ci_betwithin(model, ci = 0.95, ...)

dof_betwithin(model)

p_value_betwithin(model, dof = NULL, ...)
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
*Between-within* denominator degrees of freedom approximation is
recommended in particular for (generalized) linear mixed models with
repeated measurements (longitudinal design). `dof_betwithin()`
implements a heuristic based on the between-within approach. **Note**
that this implementation does not return exactly the same results as
shown in *Li and Redden 2015*, but similar.

### Degrees of Freedom for Longitudinal Designs (Repeated Measures)

In particular for repeated measure designs (longitudinal data analysis),
the *between-within* heuristic is likely to be more accurate than simply
using the residual or infinite degrees of freedom, because
`dof_betwithin()` returns different degrees of freedom for
within-cluster and between-cluster effects.

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

`dof_betwithin()` is a small helper-function to calculate approximated
degrees of freedom of model parameters, based on the "between-within"
heuristic.

## Examples

``` r
# \donttest{
if (require("lme4")) {
  data(sleepstudy)
  model <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
  dof_betwithin(model)
  p_value_betwithin(model)
}
#>     Parameter            p
#> 1 (Intercept) 9.306054e-80
#> 2        Days 6.290140e-06
# }
```

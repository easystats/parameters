# Kenward-Roger approximation for SEs, CIs and p-values

An approximate F-test based on the Kenward-Roger (1997) approach.

## Usage

``` r
ci_kenward(model, ci = 0.95, ...)

dof_kenward(model)

p_value_kenward(model, dof = NULL)

se_kenward(model, ...)
```

## Arguments

- model:

  A statistical model.

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

Inferential statistics (like p-values, confidence intervals and standard
errors) may be biased in mixed models when the number of clusters is
small (even if the sample size of level-1 units is high). In such cases
it is recommended to approximate a more accurate number of degrees of
freedom for such inferential statistics. Unlike simpler approximation
heuristics like the "m-l-1" rule (`dof_ml1`), the Kenward-Roger
approximation is also applicable in more complex multilevel designs,
e.g. with cross-classified clusters. However, the "m-l-1" heuristic also
applies to generalized mixed models, while approaches like Kenward-Roger
or Satterthwaite are limited to linear mixed models only.

## References

Kenward, M. G., & Roger, J. H. (1997). Small sample inference for fixed
effects from restricted maximum likelihood. Biometrics, 983-997.

## See also

`dof_kenward()` and `se_kenward()` are small helper-functions to
calculate approximated degrees of freedom and standard errors for model
parameters, based on the Kenward-Roger (1997) approach.

[`dof_satterthwaite()`](https://easystats.github.io/parameters/reference/p_value_satterthwaite.md)
and
[`dof_ml1()`](https://easystats.github.io/parameters/reference/p_value_ml1.md)
approximate degrees of freedom based on Satterthwaite's method or the
"m-l-1" rule.

## Examples

``` r
# \donttest{
if (require("lme4", quietly = TRUE)) {
  model <- lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
  p_value_kenward(model)
}
#>      Parameter            p
#> 1  (Intercept) 9.605137e-01
#> 2 Sepal.Length 8.598429e-29
# }
```

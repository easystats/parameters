# Satterthwaite approximation for SEs, CIs and p-values

An approximate F-test based on the Satterthwaite (1946) approach.

## Usage

``` r
ci_satterthwaite(model, ci = 0.95, ...)

dof_satterthwaite(model)

p_value_satterthwaite(model, dof = NULL, ...)

se_satterthwaite(model)
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
heuristics like the "m-l-1" rule (`dof_ml1`), the Satterthwaite
approximation is also applicable in more complex multilevel designs.
However, the "m-l-1" heuristic also applies to generalized mixed models,
while approaches like Kenward-Roger or Satterthwaite are limited to
linear mixed models only.

## References

Satterthwaite FE (1946) An approximate distribution of estimates of
variance components. Biometrics Bulletin 2 (6):110–4.

## See also

`dof_satterthwaite()` and `se_satterthwaite()` are small
helper-functions to calculate approximated degrees of freedom and
standard errors for model parameters, based on the Satterthwaite (1946)
approach.

[`dof_kenward()`](https://easystats.github.io/parameters/reference/p_value_kenward.md)
and
[`dof_ml1()`](https://easystats.github.io/parameters/reference/p_value_ml1.md)
approximate degrees of freedom based on Kenward-Roger's method or the
"m-l-1" rule.

## Examples

``` r
# \donttest{
if (require("lme4", quietly = TRUE)) {
  model <- lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
  p_value_satterthwaite(model)
}
#>      Parameter            p
#> 1  (Intercept) 9.605145e-01
#> 2 Sepal.Length 7.882014e-29
# }
```

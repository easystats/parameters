# Summary information from random effects

This function extracts the different variance components of a mixed
model and returns the result as a data frame.

## Usage

``` r
random_parameters(model, component = "conditional")
```

## Arguments

- model:

  A mixed effects model (including `stanreg` models).

- component:

  Should all parameters, parameters for the conditional model, for the
  zero-inflation part of the model, or the dispersion model be returned?
  Applies to models with zero-inflation and/or dispersion component.
  `component` may be one of `"conditional"`, `"zi"`, `"zero-inflated"`,
  `"dispersion"` or `"all"` (default). May be abbreviated.

## Value

A data frame with random effects statistics for the variance components,
including number of levels per random effect group, as well as complete
observations in the model.

## Details

The variance components are obtained from
[`insight::get_variance()`](https://easystats.github.io/insight/reference/get_variance.html)
and are denoted as following:

### Within-group (or residual) variance

The residual variance, σ²_(ε), is the sum of the distribution-specific
variance and the variance due to additive dispersion. It indicates the
*within-group variance*.

### Between-group random intercept variance

The random intercept variance, or *between-group* variance for the
intercept (τ₀₀), is obtained from
[`VarCorr()`](https://rdrr.io/pkg/nlme/man/VarCorr.html). It indicates
how much groups or subjects differ from each other.

### Between-group random slope variance

The random slope variance, or *between-group* variance for the slopes
(τ₁₁) is obtained from
[`VarCorr()`](https://rdrr.io/pkg/nlme/man/VarCorr.html). This measure
is only available for mixed models with random slopes. It indicates how
much groups or subjects differ from each other according to their
slopes.

### Random slope-intercept correlation

The random slope-intercept correlation (ρ₀₁) is obtained from
[`VarCorr()`](https://rdrr.io/pkg/nlme/man/VarCorr.html). This measure
is only available for mixed models with random intercepts and slopes.

**Note:** For the within-group and between-group variance, variance and
standard deviations (which are simply the square root of the variance)
are shown.

## Examples

``` r
if (require("lme4")) {
  data(sleepstudy)
  model <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
  random_parameters(model)
}
#> # Random Effects
#> 
#> Within-Group Variance          654.94 (25.59)
#> Between-Group Variance
#>   Random Intercept (Subject)    612.1 (24.74)
#>   Random Slope (Subject.Days)   35.07  (5.92)
#> Correlations
#>   Subject.Days                   0.07
#> N (groups per factor)
#>   Subject                          18
#> Observations                      180
```

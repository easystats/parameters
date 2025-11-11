# Calculate calibrated p-values.

Compute calibrated p-values that can be interpreted probabilistically,
i.e. as posterior probability of H0 (given that H0 and H1 have equal
prior probabilities).

## Usage

``` r
p_calibrate(x, ...)

# Default S3 method
p_calibrate(x, type = "frequentist", verbose = TRUE, ...)
```

## Arguments

- x:

  A numeric vector of p-values, or a regression model object.

- ...:

  Currently not used.

- type:

  Type of calibration. Can be `"frequentist"` or `"bayesian"`. See
  'Details'.

- verbose:

  Toggle warnings.

## Value

A data frame with p-values and calibrated p-values.

## Details

The Bayesian calibration, i.e. when `type = "bayesian"`, can be
interpreted as the lower bound of the Bayes factor for H0 to H1, based
on the data. The full Bayes factor would then require multiplying by the
prior odds of H0 to H1. The frequentist calibration also has a Bayesian
interpretation; it is the posterior probability of H0, assuming that H0
and H1 have equal prior probabilities of 0.5 each (*Sellke et al.
2001*).

The calibration only works for p-values lower than or equal to `1/e`.

## References

Thomas Sellke, M. J Bayarri and James O Berger (2001) Calibration of p
Values for Testing Precise Null Hypotheses, The American Statistician,
55:1, 62-71,
[doi:10.1198/000313001300339950](https://doi.org/10.1198/000313001300339950)

## Examples

``` r
model <- lm(mpg ~ wt + as.factor(gear) + am, data = mtcars)
p_calibrate(model, verbose = FALSE)
#> Parameter        |      p | p (calibrated)
#> ------------------------------------------
#> (Intercept)      | < .001 |         < .001
#> wt               | < .001 |         < .001
#> as.factor(gear)4 | 0.242  |         0.483 
#> as.factor(gear)5 | 0.660  |               
#> am               | 0.925  |               
#> Calibrated p-values indicate the posterior probability of H0.
#> 
```

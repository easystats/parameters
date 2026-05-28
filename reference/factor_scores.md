# Extract factor scores from Factor Analysis (EFA) or Omega

`factor_scores()` extracts the factor scores from objects returned by
[`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html),
[`factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.md),
or [`psych::omega()`](https://rdrr.io/pkg/psych/man/omega.html)

## Usage

``` r
factor_scores(x, ...)
```

## Arguments

- x:

  An object returned by
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html),
  [`factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.md),
  or [`psych::omega()`](https://rdrr.io/pkg/psych/man/omega.html).

- ...:

  Currently unused.

## Value

A data frame with the factor scores. It simply extracts the `$scores`
element from the object and converts it into a data frame.

## See also

[`factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.md)

## Examples

``` r
data(mtcars)
out <- factor_analysis(mtcars[, 1:7], n = 2)
head(factor_scores(out))
#>                           MR2        MR1
#> Mazda RX4         -0.47283517 -0.7310481
#> Mazda RX4 Wag     -0.38011257 -0.4342613
#> Datsun 710        -0.99251655  0.4087635
#> Hornet 4 Drive     0.07181224  0.9131825
#> Hornet Sportabout  0.72282019 -0.3946493
#> Valiant            0.21348813  1.3394233
```

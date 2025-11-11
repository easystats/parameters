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
#>                          MR1        MR2
#> Mazda RX4         -0.4728356 -0.7310464
#> Mazda RX4 Wag     -0.3801129 -0.4342601
#> Datsun 710        -0.9925165  0.4087655
#> Hornet 4 Drive     0.0718127  0.9131816
#> Hornet Sportabout  0.7228201 -0.3946507
#> Valiant            0.2134888  1.3394217
```

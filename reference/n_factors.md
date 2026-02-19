# Number of components/factors to retain in PCA/FA

This function runs many existing procedures for determining how many
factors to retain/extract from factor analysis (FA) or dimension
reduction (PCA). It returns the number of factors based on the maximum
consensus between methods. In case of ties, it will keep the simplest
model and select the solution with the fewer factors.

## Usage

``` r
n_factors(
  x,
  type = "FA",
  rotation = "varimax",
  algorithm = "default",
  package = c("nFactors", "psych"),
  correlation_matrix = NULL,
  safe = TRUE,
  n_max = NULL,
  ...
)

n_components(
  x,
  type = "PCA",
  rotation = "varimax",
  algorithm = "default",
  package = c("nFactors", "psych"),
  correlation_matrix = NULL,
  safe = TRUE,
  ...
)
```

## Arguments

- x:

  A data frame.

- type:

  Can be `"FA"` or `"PCA"`, depending on what you want to do.

- rotation:

  Only used for VSS (Very Simple Structure criterion, see
  [`psych::VSS()`](https://rdrr.io/pkg/psych/man/VSS.html)). The
  rotation to apply. Can be `"none"`, `"varimax"`, `"quartimax"`,
  `"bentlerT"`, `"equamax"`, `"varimin"`, `"geominT"` and `"bifactor"`
  for orthogonal rotations, and `"promax"`, `"oblimin"`, `"simplimax"`,
  `"bentlerQ"`, `"geominQ"`, `"biquartimin"` and `"cluster"` for oblique
  transformations.

- algorithm:

  Factoring method used by VSS. Can be `"pa"` for Principal Axis Factor
  Analysis, `"minres"` for minimum residual (OLS) factoring, `"mle"` for
  Maximum Likelihood FA and `"pc"` for Principal Components. `"default"`
  will select `"minres"` if `type = "FA"` and `"pc"` if `type = "PCA"`.

- package:

  Package from which respective methods are used. Can be `"all"` or a
  vector containing `"nFactors"`, `"psych"`, `"PCDimension"`, `"fit"` or
  `"EGAnet"`. Note that `"fit"` (which actually also relies on the
  `psych` package) and `"EGAnet"` can be very slow for bigger datasets.
  Thus, the default is `c("nFactors", "psych")`. You must have the
  respective packages installed for the methods to be used.

- correlation_matrix:

  An optional correlation matrix that can be used (note that the data
  must still be passed as the first argument). If `NULL`, will compute
  it by running [`cor()`](https://rdrr.io/r/stats/cor.html) on the
  passed data.

- safe:

  If `TRUE`, the function will run all the procedures in try blocks, and
  will only return those that work and silently skip the ones that may
  fail.

- n_max:

  If set to a value (e.g., `10`), will drop from the results all methods
  that suggest a higher number of components. The interpretation becomes
  'from all the methods that suggested a number lower than n_max, the
  results are ...'.

- ...:

  Arguments passed to or from other methods.

## Value

A data frame.

## Details

`n_components()` is actually an alias for `n_factors()`, with different
defaults for the function arguments.

## Note

There is also a
[`plot()`-method](https://easystats.github.io/see/articles/parameters.html)
implemented in the [**see**-package](https://easystats.github.io/see/).
`n_components()` is a convenient short-cut for
`n_factors(type = "PCA")`.

## References

- Bartlett, M. S. (1950). Tests of significance in factor analysis.
  British Journal of statistical psychology, 3(2), 77-85.

- Bentler, P. M., & Yuan, K. H. (1996). Test of linear trend in
  eigenvalues of a covariance matrix with application to data analysis.
  British Journal of Mathematical and Statistical Psychology, 49(2),
  299-312.

- Cattell, R. B. (1966). The scree test for the number of factors.
  Multivariate behavioral research, 1(2), 245-276.

- Finch, W. H. (2019). Using Fit Statistic Differences to Determine the
  Optimal Number of Factors to Retain in an Exploratory Factor Analysis.
  Educational and Psychological Measurement.

- Zoski, K. W., & Jurs, S. (1996). An objective counterpart to the
  visual scree test for factor analysis: The standard error scree.
  Educational and Psychological Measurement, 56(3), 443-451.

- Zoski, K., & Jurs, S. (1993). Using multiple regression to determine
  the number of factors to retain in factor analysis. Multiple Linear
  Regression Viewpoints, 20(1), 5-9.

- Nasser, F., Benson, J., & Wisenbaker, J. (2002). The performance of
  regression-based variations of the visual scree for determining the
  number of common factors. Educational and psychological measurement,
  62(3), 397-419.

- Golino, H., Shi, D., Garrido, L. E., Christensen, A. P., Nieto, M. D.,
  Sadana, R., & Thiyagarajan, J. A. (2018). Investigating the
  performance of Exploratory Graph Analysis and traditional techniques
  to identify the number of latent factors: A simulation and tutorial.

- Golino, H. F., & Epskamp, S. (2017). Exploratory graph analysis: A new
  approach for estimating the number of dimensions in psychological
  research. PloS one, 12(6), e0174035.

- Revelle, W., & Rocklin, T. (1979). Very simple structure: An
  alternative procedure for estimating the optimal number of
  interpretable factors. Multivariate Behavioral Research, 14(4),
  403-414.

- Velicer, W. F. (1976). Determining the number of components from the
  matrix of partial correlations. Psychometrika, 41(3), 321-327.

## Examples

``` r
library(parameters)
n_factors(mtcars, type = "PCA")
#> # Method Agreement Procedure:
#> 
#> The choice of 3 dimensions is supported by 5 (29.41%) methods out of 17 (Bartlett, CNG, Scree (SE), Scree (R2), Velicer's MAP).

result <- n_factors(mtcars[1:5], type = "FA")
as.data.frame(result)
#>    n_Factors              Method       Family
#> 1          1             Bentler      Bentler
#> 2          1 Optimal coordinates        Scree
#> 3          1 Acceleration factor        Scree
#> 4          1   Parallel analysis        Scree
#> 5          1    Kaiser criterion        Scree
#> 6          1          Scree (SE)     Scree_SE
#> 7          1    VSS complexity 1          VSS
#> 8          1       Velicer's MAP Velicers_MAP
#> 9          1                 BIC          BIC
#> 10         2            Bartlett      Barlett
#> 11         2            Anderson      Barlett
#> 12         2              Lawley      Barlett
#> 13         2          Scree (R2)     Scree_SE
#> 14         2      BIC (adjusted)          BIC
#> 15         3    VSS complexity 2          VSS
summary(result)
#>   n_Factors n_Methods Variance_Cumulative
#> 1         1         9           0.7824146
#> 2         2         5           0.8583921
#> 3         3         1           0.8661856
# \donttest{
# Setting package = 'all' will increase the number of methods (but is slow)
n_factors(mtcars, type = "PCA", package = "all")
#> # Method Agreement Procedure:
#> 
#> The choice of 3 dimensions is supported by 7 (33.33%) methods out of 21 (Bartlett, CNG, Scree (SE), Scree (R2), EGA (glasso), EGA (TMFG), Velicer's MAP).
n_factors(mtcars, type = "FA", algorithm = "mle", package = "all")
#> # Method Agreement Procedure:
#> 
#> The choice of 3 dimensions is supported by 11 (40.74%) methods out of 27 (Bartlett, CNG, Scree (SE), Scree (R2), EGA (glasso), EGA (TMFG), Velicer's MAP, BIC, RMSR, CRMS, BIC).
# }
```

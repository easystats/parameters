# Dimensionality reduction (DR) / Features Reduction

This function performs a reduction in the parameter space (the number of
variables). It starts by creating a new set of variables, based on the
given method (the default method is "PCA", but other are available via
the `method` argument, such as "cMDS", "DRR" or "ICA"). Then, it names
this new dimensions using the original variables that correlates the
most with it. For instance, a variable named `'V1_0.97/V4_-0.88'` means
that the V1 and the V4 variables correlate maximally (with respective
coefficients of .97 and -.88) with this dimension. Although this
function can be useful in exploratory data analysis, it's best to
perform the dimension reduction step in a separate and dedicated stage,
as this is a very important process in the data analysis workflow.
`reduce_data()` is an alias for `reduce_parameters.data.frame()`.

## Usage

``` r
reduce_parameters(x, method = "PCA", n = "max", distance = "euclidean", ...)

reduce_data(x, method = "PCA", n = "max", distance = "euclidean", ...)
```

## Arguments

- x:

  A data frame or a statistical model. For
  [`closest_component()`](https://easystats.github.io/parameters/reference/principal_components.md),
  the output of the
  [`principal_components()`](https://easystats.github.io/parameters/reference/principal_components.md)
  function.

- method:

  The feature reduction method. Can be one of `"PCA"`, `"cMDS"`,
  `"DRR"`, `"ICA"` (see the 'Details' section).

- n:

  Number of components to extract. If `n="all"`, then `n` is set as the
  number of variables minus 1 (`ncol(x)-1`). If `n="auto"` (default) or
  `n=NULL`, the number of components is selected through
  [`n_factors()`](https://easystats.github.io/parameters/reference/n_factors.md)
  resp.
  [`n_components()`](https://easystats.github.io/parameters/reference/n_factors.md).
  Else, if `n` is a number, `n` components are extracted. If `n` exceeds
  number of variables in the data, it is automatically set to the
  maximum number (i.e. `ncol(x)`). In `reduce_parameters()`, can also be
  `"max"`, in which case it will select all the components that are
  maximally pseudo-loaded (i.e., correlated) by at least one variable.

- distance:

  The distance measure to be used. Only applies when `method = "cMDS"`.
  This must be one of `"euclidean"`, `"maximum"`, `"manhattan"`,
  `"canberra"`, `"binary"` or `"minkowski"`. Any unambiguous substring
  can be given.

- ...:

  Arguments passed to or from other methods.

## Details

The different methods available are described below:

### Supervised Methods

- **PCA**: See
  [`principal_components()`](https://easystats.github.io/parameters/reference/principal_components.md).

- **cMDS / PCoA**: Classical Multidimensional Scaling (cMDS) takes a set
  of dissimilarities (i.e., a distance matrix) and returns a set of
  points such that the distances between the points are approximately
  equal to the dissimilarities.

- **DRR**: Dimensionality Reduction via Regression (DRR) is a very
  recent technique extending PCA (*Laparra et al., 2015*). Starting from
  a rotated PCA, it predicts redundant information from the remaining
  components using non-linear regression. Some of the most notable
  advantages of performing DRR are avoidance of multicollinearity
  between predictors and overfitting mitigation. DRR tends to perform
  well when the first principal component is enough to explain most of
  the variation in the predictors. Requires the **DRR** package to be
  installed.

- **ICA**: Performs an Independent Component Analysis using the FastICA
  algorithm. Contrary to PCA, which attempts to find uncorrelated
  sources (through least squares minimization), ICA attempts to find
  independent sources, i.e., the source space that maximizes the
  "non-gaussianity" of all sources. Contrary to PCA, ICA does not rank
  each source, which makes it a poor tool for dimensionality reduction.
  Requires the **fastICA** package to be installed.

See also [package
vignette](https://easystats.github.io/parameters/articles/parameters_reduction.html).

## References

- Nguyen, L. H., and Holmes, S. (2019). Ten quick tips for effective
  dimensionality reduction. PLOS Computational Biology, 15(6).

- Laparra, V., Malo, J., and Camps-Valls, G. (2015). Dimensionality
  reduction via regression in hyperspectral imagery. IEEE Journal of
  Selected Topics in Signal Processing, 9(6), 1026-1036.

## Examples

``` r
data(iris)
model <- lm(Sepal.Width ~ Species * Sepal.Length + Petal.Width, data = iris)
model
#> 
#> Call:
#> lm(formula = Sepal.Width ~ Species * Sepal.Length + Petal.Width, 
#>     data = iris)
#> 
#> Coefficients:
#>                    (Intercept)               Speciesversicolor  
#>                        -0.4731                          1.2981  
#>               Speciesvirginica                    Sepal.Length  
#>                         1.2252                          0.7515  
#>                    Petal.Width  Speciesversicolor:Sepal.Length  
#>                         0.5662                         -0.5503  
#>  Speciesvirginica:Sepal.Length  
#>                        -0.5883  
#> 
reduce_parameters(model)
#> 
#> Call:
#> lm(formula = Sepal.Width ~ `Petal.Width_0.98/Species.setosa_-0.90/Sepal.Length_0.89/Species.virginica_0.78` + 
#>     `Species.versicolor_-0.99`, data = cbind(model_data, y))
#> 
#> Coefficients:
#>                                                                      (Intercept)  
#>                                                                          3.05733  
#> `Petal.Width_0.98/Species.setosa_-0.90/Sepal.Length_0.89/Species.virginica_0.78`  
#>                                                                         -0.08903  
#>                                                       `Species.versicolor_-0.99`  
#>                                                                          0.14879  
#> 

out <- reduce_data(iris, method = "PCA", n = "max")
head(out)
#>   Petal.Length_0.99/Petal.Width_0.97/Species.setosa_-0.94/Sepal.Length_0.86/Species.virginica_0.73
#> 1                                                                                        -2.803852
#> 2                                                                                        -2.633035
#> 3                                                                                        -2.866923
#> 4                                                                                        -2.808656
#> 5                                                                                        -2.907343
#> 6                                                                                        -2.668523
#>   Species.versicolor_0.93 Sepal.Width_0.62
#> 1             -0.65195900        0.1365792
#> 2             -0.09924539       -0.8296167
#> 3             -0.26560467       -0.5984029
#> 4             -0.14622405       -0.8154592
#> 5             -0.73579102        0.2543209
#> 6             -1.14741717        1.0076406
```

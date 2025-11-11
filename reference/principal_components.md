# Principal Component Analysis (PCA) and Factor Analysis (FA)

The functions `principal_components()` and `factor_analysis()` can be
used to perform a principal component analysis (PCA) or a factor
analysis (FA). They return the loadings as a data frame, and various
methods and functions are available to access / display other
information (see the 'Details' section).

## Usage

``` r
factor_analysis(x, ...)

# S3 method for class 'data.frame'
factor_analysis(
  x,
  n = "auto",
  rotation = "oblimin",
  factor_method = "minres",
  sort = FALSE,
  threshold = NULL,
  standardize = FALSE,
  ...
)

# S3 method for class 'matrix'
factor_analysis(
  x,
  n = "auto",
  rotation = "oblimin",
  factor_method = "minres",
  n_obs = NULL,
  sort = FALSE,
  threshold = NULL,
  standardize = FALSE,
  ...
)

principal_components(x, ...)

rotated_data(x, verbose = TRUE)

# S3 method for class 'data.frame'
principal_components(
  x,
  n = "auto",
  rotation = "none",
  sparse = FALSE,
  sort = FALSE,
  threshold = NULL,
  standardize = TRUE,
  ...
)

# S3 method for class 'parameters_efa'
print_html(x, digits = 2, sort = FALSE, threshold = NULL, labels = NULL, ...)

# S3 method for class 'parameters_efa'
predict(
  object,
  newdata = NULL,
  names = NULL,
  keep_na = TRUE,
  verbose = TRUE,
  ...
)

# S3 method for class 'parameters_efa'
print(x, digits = 2, sort = FALSE, threshold = NULL, labels = NULL, ...)

# S3 method for class 'parameters_efa'
sort(x, ...)

closest_component(x)
```

## Arguments

- x:

  A data frame or a statistical model. For `closest_component()`, the
  output of the `principal_components()` function.

- ...:

  Arguments passed to or from other methods.

- n:

  Number of components to extract. If `n="all"`, then `n` is set as the
  number of variables minus 1 (`ncol(x)-1`). If `n="auto"` (default) or
  `n=NULL`, the number of components is selected through
  [`n_factors()`](https://easystats.github.io/parameters/reference/n_factors.md)
  resp.
  [`n_components()`](https://easystats.github.io/parameters/reference/n_factors.md).
  Else, if `n` is a number, `n` components are extracted. If `n` exceeds
  number of variables in the data, it is automatically set to the
  maximum number (i.e. `ncol(x)`). In
  [`reduce_parameters()`](https://easystats.github.io/parameters/reference/reduce_parameters.md),
  can also be `"max"`, in which case it will select all the components
  that are maximally pseudo-loaded (i.e., correlated) by at least one
  variable.

- rotation:

  If not `"none"`, the PCA / FA will be computed using the **psych**
  package. Possible options include `"varimax"`, `"quartimax"`,
  `"promax"`, `"oblimin"`, `"simplimax"`, or `"cluster"` (and more). See
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html) for details.
  The default is `"none"` for PCA, and `"oblimin"` for FA.

- factor_method:

  The factoring method to be used. Passed to the `fm` argument in
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html). Defaults to
  `"minres"` (minimum residual). Other options include `"uls"`, `"ols"`,
  `"wls"`, `"gls"`, `"ml"`, `"minchi"`, `"minrank"`, `"old.min"`, and
  `"alpha"`. See [`?psych::fa`](https://rdrr.io/pkg/psych/man/fa.html)
  for details.

- sort:

  Sort the loadings.

- threshold:

  A value between 0 and 1 indicates which (absolute) values from the
  loadings should be removed. An integer higher than 1 indicates the n
  strongest loadings to retain. Can also be `"max"`, in which case it
  will only display the maximum loading per variable (the most simple
  structure).

- standardize:

  A logical value indicating whether the variables should be
  standardized (centered and scaled) to have unit variance before the
  analysis (in general, such scaling is advisable). **Note:** This
  defaults to `TRUE` for PCA, but to `FALSE` for FA (because
  `factor_analysis()` computes a correlation matrix and uses that
  r-matrix for the factor analysis by default - therefore,
  standardization of the raw variables is unnecessary, and even
  undesirable when using `cor = "poly"`).

- n_obs:

  An integer or a matrix.

  - **Integer:** Number of observations in the original data set if `x`
    is a correlation matrix. Required to compute correct fit indices.

  - **Matrix:** A matrix where each cell `[i, j]` specifies the number
    of pairwise complete observations used to compute the correlation
    between variable `i` and variable `j` in the input `x`. It is
    crucial when `x` is a correlation matrix (rather than raw data),
    especially if that matrix was derived from a dataset containing
    missing values using pairwise deletion. Providing a matrix allows
    [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html) to accurately
    calculate statistical measures, such as chi-square fit statistics,
    by accounting for the varying sample sizes that contribute to each
    individual correlation coefficient.

- verbose:

  Toggle warnings.

- sparse:

  Whether to compute sparse PCA (SPCA, using
  [`sparsepca::spca()`](https://rdrr.io/pkg/sparsepca/man/spca.html)).
  SPCA attempts to find sparse loadings (with few nonzero values), which
  improves interpretability and avoids overfitting. Can be `TRUE` or
  `"robust"` (see
  [`sparsepca::robspca()`](https://rdrr.io/pkg/sparsepca/man/robspca.html)).

- digits:

  Argument for [`print()`](https://rdrr.io/r/base/print.html), indicates
  the number of digits (rounding) to be used.

- labels:

  Argument for [`print()`](https://rdrr.io/r/base/print.html), character
  vector of same length as columns in `x`. If provided, adds an
  additional column with the labels.

- object:

  An object of class `parameters_pca`, `parameters_efa` or `psych_efa`.

- newdata:

  An optional data frame in which to look for variables with which to
  predict. If omitted, the fitted values are used.

- names:

  Optional character vector to name columns of the returned data frame.

- keep_na:

  Logical, if `TRUE`, predictions also return observations with missing
  values from the original data, hence the number of rows of predicted
  data and original data is equal.

## Value

A data frame of loadings. For `factor_analysis()`, this data frame is
also of class `parameters_efa()`. Objects from `principal_components()`
are of class `parameters_pca()`.

## Details

### Methods and Utilities

- [`n_components()`](https://easystats.github.io/parameters/reference/n_factors.md)
  and
  [`n_factors()`](https://easystats.github.io/parameters/reference/n_factors.md)
  automatically estimates the optimal number of dimensions to retain.

- [`performance::check_factorstructure()`](https://easystats.github.io/performance/reference/check_factorstructure.html)
  checks the suitability of the data for factor analysis using the
  sphericity (see
  [`performance::check_sphericity_bartlett()`](https://easystats.github.io/performance/reference/check_factorstructure.html))
  and the KMO (see
  [`performance::check_kmo()`](https://easystats.github.io/performance/reference/check_factorstructure.html))
  measure.

- [`performance::check_itemscale()`](https://easystats.github.io/performance/reference/check_itemscale.html)
  computes various measures of internal consistencies applied to the
  (sub)scales (i.e., components) extracted from the PCA.

- Running [`summary()`](https://rdrr.io/r/base/summary.html) returns
  information related to each component/factor, such as the explained
  variance and the Eivenvalues.

- Running
  [`get_scores()`](https://easystats.github.io/parameters/reference/get_scores.md)
  computes scores for each subscale.

- [`factor_scores()`](https://easystats.github.io/parameters/reference/factor_scores.md)
  extracts the factor scores from objects returned by
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html),
  `factor_analysis()`, or
  [`psych::omega()`](https://rdrr.io/pkg/psych/man/omega.html).

- Running `closest_component()` will return a numeric vector with the
  assigned component index for each column from the original data frame.

- Running `rotated_data()` will return the rotated data, including
  missing values, so it matches the original data frame.

- [`performance::item_omega()`](https://easystats.github.io/performance/reference/item_omega.html)
  is a convenient wrapper around
  [`psych::omega()`](https://rdrr.io/pkg/psych/man/omega.html), which
  provides some additional methods to work seamlessly within the
  *easystats* framework.

- [`performance::check_normality()`](https://easystats.github.io/performance/reference/check_normality.html)
  checks residuals from objects returned by
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html),
  `factor_analysis()`,
  [`performance::item_omega()`](https://easystats.github.io/performance/reference/item_omega.html),
  or [`psych::omega()`](https://rdrr.io/pkg/psych/man/omega.html) for
  normality.

- [`performance::model_performance()`](https://easystats.github.io/performance/reference/model_performance.html)
  returns fit-indices for objects returned by
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html),
  `factor_analysis()`, or
  [`psych::omega()`](https://rdrr.io/pkg/psych/man/omega.html).

- Running
  [[`plot()`](https://rdrr.io/r/graphics/plot.default.html)](https://easystats.github.io/see/articles/parameters.html#principal-component-analysis)
  visually displays the loadings (that requires the
  [**see**-package](https://easystats.github.io/see/) to work).

### Complexity

Complexity represents the number of latent components needed to account
for the observed variables. Whereas a perfect simple structure solution
has a complexity of 1 in that each item would only load on one factor, a
solution with evenly distributed items has a complexity greater than 1
(*Hofman, 1978; Pettersson and Turkheimer, 2010*).

### Uniqueness

Uniqueness represents the variance that is 'unique' to the variable and
not shared with other variables. It is equal to `1 - communality`
(variance that is shared with other variables). A uniqueness of `0.20`
suggests that `20%` or that variable's variance is not shared with other
variables in the overall factor model. The greater 'uniqueness' the
lower the relevance of the variable in the factor model.

### MSA

MSA represents the Kaiser-Meyer-Olkin Measure of Sampling Adequacy
(*Kaiser and Rice, 1974*) for each item. It indicates whether there is
enough data for each factor give reliable results for the PCA. The value
should be \> 0.6, and desirable values are \> 0.8 (*Tabachnick and
Fidell, 2013*).

### PCA or FA?

There is a simplified rule of thumb that may help do decide whether to
run a factor analysis or a principal component analysis:

- Run *factor analysis* if you assume or wish to test a theoretical
  model of *latent factors* causing observed variables.

- Run *principal component analysis* If you want to simply *reduce* your
  correlated observed variables to a smaller set of important
  independent composite variables.

(Source: [CrossValidated](https://stats.stackexchange.com/q/1576/54740))

### Computing Item Scores

Use
[`get_scores()`](https://easystats.github.io/parameters/reference/get_scores.md)
to compute scores for the "subscales" represented by the extracted
principal components or factors.
[`get_scores()`](https://easystats.github.io/parameters/reference/get_scores.md)
takes the results from `principal_components()` or `factor_analysis()`
and extracts the variables for each component found by the PCA. Then,
for each of these "subscales", raw means are calculated (which equals
adding up the single items and dividing by the number of items). This
results in a sum score for each component from the PCA, which is on the
same scale as the original, single items that were used to compute the
PCA. One can also use
[`predict()`](https://rdrr.io/r/stats/predict.html) to back-predict
scores for each component, to which one can provide `newdata` or a
vector of `names` for the components.

### Explained Variance and Eingenvalues

Use [`summary()`](https://rdrr.io/r/base/summary.html) to get the
Eigenvalues and the explained variance for each extracted component. The
eigenvectors and eigenvalues represent the "core" of a PCA: The
eigenvectors (the principal components) determine the directions of the
new feature space, and the eigenvalues determine their magnitude. In
other words, the eigenvalues explain the variance of the data along the
new feature axes.

## References

- Kaiser, H.F. and Rice. J. (1974). Little jiffy, mark iv. Educational
  and Psychological Measurement, 34(1):111–117

- Hofmann, R. (1978). Complexity and simplicity as objective indices
  descriptive of factor solutions. Multivariate Behavioral Research,
  13:2, 247-250,
  [doi:10.1207/s15327906mbr1302_9](https://doi.org/10.1207/s15327906mbr1302_9)

- Pettersson, E., & Turkheimer, E. (2010). Item selection, evaluation,
  and simple structure in personality data. Journal of research in
  personality, 44(4), 407-420,
  [doi:10.1016/j.jrp.2010.03.002](https://doi.org/10.1016/j.jrp.2010.03.002)

- Tabachnick, B. G., and Fidell, L. S. (2013). Using multivariate
  statistics (6th ed.). Boston: Pearson Education.

## Examples

``` r
library(parameters)

# \donttest{
# Principal Component Analysis (PCA) -------------------
principal_components(mtcars[, 1:7], n = "all", threshold = 0.2)
#> # Loadings from Principal Component Analysis (no rotation)
#> 
#> Variable |   PC1 |   PC2 |  PC3 |   PC4 |  PC5 |   PC6 | Complexity
#> -------------------------------------------------------------------
#> mpg      | -0.93 |       |      | -0.30 |      |       |       1.30
#> cyl      |  0.96 |       |      |       |      | -0.21 |       1.18
#> disp     |  0.95 |       |      | -0.23 |      |       |       1.16
#> hp       |  0.87 |  0.36 |      |       | 0.30 |       |       1.64
#> drat     | -0.75 |  0.48 | 0.44 |       |      |       |       2.47
#> wt       |  0.88 | -0.35 | 0.26 |       |      |       |       1.54
#> qsec     | -0.54 | -0.81 |      |       |      |       |       1.96
#> 
#> The 6 principal components accounted for 99.30% of the total variance of the original data (PC1 = 72.66%, PC2 = 16.52%, PC3 = 4.93%, PC4 = 2.26%, PC5 = 1.85%, PC6 = 1.08%).
#> 

# Automated number of components
principal_components(mtcars[, 1:4], n = "auto")
#> # Loadings from Principal Component Analysis (no rotation)
#> 
#> Variable |   PC1 | Complexity
#> -----------------------------
#> mpg      | -0.93 |       1.00
#> cyl      |  0.96 |       1.00
#> disp     |  0.95 |       1.00
#> hp       |  0.91 |       1.00
#> 
#> The unique principal component accounted for 87.55% of the total variance of the original data.
#> 

# labels can be useful if variable names are not self-explanatory
print(
  principal_components(mtcars[, 1:4], n = "auto"),
  labels = c(
    "Miles/(US) gallon",
    "Number of cylinders",
    "Displacement (cu.in.)",
    "Gross horsepower"
  )
)
#> # Loadings from Principal Component Analysis (no rotation)
#> 
#> Variable | Label                 |   PC1 | Complexity
#> -----------------------------------------------------
#> mpg      | Miles/(US) gallon     | -0.93 |       1.00
#> cyl      | Number of cylinders   |  0.96 |       1.00
#> disp     | Displacement (cu.in.) |  0.95 |       1.00
#> hp       | Gross horsepower      |  0.91 |       1.00

# Sparse PCA
principal_components(mtcars[, 1:7], n = 4, sparse = TRUE)
#> # Loadings from Principal Component Analysis (no rotation)
#> 
#> Variable |   PC1 |   PC2 |   PC3 |   PC4 | Complexity
#> -----------------------------------------------------
#> mpg      | -0.92 |  0.03 | -0.11 | -0.31 |       1.27
#> cyl      |  1.00 |  0.07 | -0.07 | -0.05 |       1.03
#> disp     |  0.96 | -0.06 |  0.08 | -0.23 |       1.14
#> hp       |  0.74 |  0.32 |  0.07 |  0.00 |       1.38
#> drat     | -0.68 |  0.46 |  0.47 | -0.03 |       2.62
#> wt       |  1.03 | -0.32 |  0.24 | -0.03 |       1.31
#> qsec     | -0.49 | -0.85 |  0.17 |  0.00 |       1.69
#> 
#> The 4 principal components accounted for 96.42% of the total variance of the original data (PC1 = 72.75%, PC2 = 16.53%, PC3 = 4.91%, PC4 = 2.24%).
#> 
principal_components(mtcars[, 1:7], n = 4, sparse = "robust")
#> # Loadings from Principal Component Analysis (no rotation)
#> 
#> Variable |   PC1 |   PC2 |   PC3 |   PC4 | Complexity
#> -----------------------------------------------------
#> mpg      | -0.92 |  0.03 | -0.11 | -0.31 |       1.27
#> cyl      |  1.00 |  0.07 | -0.07 | -0.05 |       1.03
#> disp     |  0.96 | -0.06 |  0.08 | -0.23 |       1.14
#> hp       |  0.74 |  0.32 |  0.07 |  0.00 |       1.38
#> drat     | -0.68 |  0.46 |  0.47 | -0.03 |       2.62
#> wt       |  1.03 | -0.32 |  0.24 | -0.03 |       1.31
#> qsec     | -0.49 | -0.85 |  0.17 |  0.00 |       1.69
#> 
#> The 4 principal components accounted for 96.42% of the total variance of the original data (PC1 = 72.75%, PC2 = 16.53%, PC3 = 4.91%, PC4 = 2.24%).
#> 

# Rotated PCA
principal_components(mtcars[, 1:7],
  n = 2, rotation = "oblimin",
  threshold = "max", sort = TRUE
)
#> # Rotated loadings from Principal Component Analysis (oblimin-rotation)
#> 
#> Variable |   TC1 |   TC2 | Complexity | Uniqueness |  MSA
#> ---------------------------------------------------------
#> wt       |  0.98 |       |       1.03 |       0.10 | 0.77
#> drat     | -0.95 |       |       1.19 |       0.21 | 0.85
#> disp     |  0.89 |       |       1.07 |       0.08 | 0.85
#> mpg      | -0.87 |       |       1.07 |       0.13 | 0.87
#> cyl      |  0.78 |       |       1.38 |       0.08 | 0.87
#> qsec     |       | -0.98 |       1.00 |       0.06 | 0.61
#> hp       |       |  0.61 |       1.97 |       0.10 | 0.90
#> 
#> The 2 principal components (oblimin rotation) accounted for 89.18% of the total variance of the original data (TC1 = 63.90%, TC2 = 25.28%).
#> 
principal_components(mtcars[, 1:7], n = 2, threshold = 2, sort = TRUE)
#> # Loadings from Principal Component Analysis (no rotation)
#> 
#> Variable |  PC1 |   PC2 | Complexity
#> ------------------------------------
#> cyl      | 0.96 |       |       1.02
#> disp     | 0.95 |       |       1.02
#> mpg      |      |       |       1.02
#> wt       |      |       |       1.30
#> hp       |      |       |       1.33
#> drat     |      |  0.48 |       1.71
#> qsec     |      | -0.81 |       1.75
#> 
#> The 2 principal components accounted for 89.18% of the total variance of the original data (PC1 = 72.66%, PC2 = 16.52%).
#> 

pca <- principal_components(mtcars[, 1:5], n = 2, rotation = "varimax")
pca # Print loadings
#> # Rotated loadings from Principal Component Analysis (varimax-rotation)
#> 
#> Variable |   RC1 |   RC2 | Complexity | Uniqueness |  MSA
#> ---------------------------------------------------------
#> mpg      | -0.77 |  0.53 |       1.77 |       0.14 | 0.92
#> cyl      |  0.81 | -0.52 |       1.70 |       0.08 | 0.84
#> disp     |  0.77 | -0.56 |       1.82 |       0.10 | 0.88
#> hp       |  0.95 | -0.16 |       1.05 |       0.06 | 0.81
#> drat     | -0.27 |  0.95 |       1.16 |       0.03 | 0.80
#> 
#> The 2 principal components (varimax rotation) accounted for 92.00% of the total variance of the original data (RC1 = 56.46%, RC2 = 35.54%).
#> 
summary(pca) # Print information about the factors
#> # (Explained) Variance of Components
#> 
#> Parameter                       |   RC1 |   RC2
#> -----------------------------------------------
#> Eigenvalues                     | 4.038 | 0.562
#> Variance Explained              | 0.565 | 0.355
#> Variance Explained (Cumulative) | 0.565 | 0.920
#> Variance Explained (Proportion) | 0.614 | 0.386
predict(pca, names = c("Component1", "Component2")) # Back-predict scores
#>     Component1  Component2
#> 1  -0.23906186  0.38058456
#> 2  -0.23906186  0.38058456
#> 3  -0.87523403  0.30496365
#> 4  -0.83256507 -1.21853890
#> 5   0.37716377 -0.82007976
#> 6  -1.10127278 -1.86114104
#> 7   1.23453249 -0.26702364
#> 8  -1.30024506 -0.23007562
#> 9  -0.74129623  0.41446358
#> 10 -0.02461777  0.47612740
#> 11  0.02534454  0.45386195
#> 12  0.29853804 -0.88051969
#> 13  0.26641941 -0.86620618
#> 14  0.34136287 -0.89960437
#> 15  0.93247714 -1.25088059
#> 16  1.06844670 -1.03682781
#> 17  1.22799380 -0.41588998
#> 18 -1.30762202  0.71198099
#> 19 -0.60701035  2.14073148
#> 20 -1.25685030  0.99325379
#> 21 -0.90720118  0.02588588
#> 22 -0.15711335 -1.72688554
#> 23  0.18137850 -1.00150788
#> 24  1.72116683  0.68171810
#> 25  0.36060813 -0.98161661
#> 26 -1.12513535  0.63056351
#> 27 -0.46730967  1.39310467
#> 28 -1.05428733  0.43877634
#> 29  2.24906373  1.75900256
#> 30  0.13210722  0.33766027
#> 31  2.24244855  1.06973229
#> 32 -0.42316749  0.86380202

# which variables from the original data belong to which extracted component?
closest_component(pca)
#>  mpg  cyl disp   hp drat 
#>    1    1    1    1    2 
# }

# Factor Analysis (FA) ------------------------

factor_analysis(mtcars[, 1:7], n = "all", threshold = 0.2, rotation = "Promax")
#> # Rotated loadings from Factor Analysis (Promax-rotation)
#> 
#> Variable |   MR1 |   MR3 |   MR2 |  MR6 |  MR4 |  MR5 | Complexity | Uniqueness
#> -------------------------------------------------------------------------------
#> mpg      | -0.56 |  0.28 |       |      |      |      |       1.94 |       0.08
#> cyl      |       | -0.58 | -0.34 |      |      | 0.24 |       2.28 |       0.01
#> disp     |  0.48 | -0.37 |       |      | 0.28 |      |       2.91 |       0.03
#> hp       |       |       | -0.43 | 0.45 |      |      |       2.17 |       0.12
#> drat     |       |  1.01 |       |      |      |      |       1.08 |       0.24
#> wt       |  1.07 |       |  0.20 |      |      |      |       1.08 |   5.00e-03
#> qsec     |       |       |  1.01 |      |      |      |       1.04 |       0.14
#> 
#> The 6 latent factors (Promax rotation) accounted for 90.99% of the total variance of the original data (MR1 = 31.21%, MR3 = 28.13%, MR2 = 23.60%, MR6 = 5.81%, MR4 = 1.49%, MR5 = 0.75%).
#> 
factor_analysis(mtcars[, 1:7], n = 2, threshold = "max", sort = TRUE)
#> # Rotated loadings from Factor Analysis (oblimin-rotation)
#> 
#> Variable |   MR1 |  MR2 | Complexity | Uniqueness
#> -------------------------------------------------
#> wt       |  1.00 |      |       1.07 |       0.10
#> disp     |  0.92 |      |       1.02 |       0.09
#> mpg      | -0.88 |      |       1.02 |       0.15
#> drat     | -0.84 |      |       1.14 |       0.39
#> cyl      |  0.82 |      |       1.23 |       0.08
#> hp       |  0.60 |      |       1.94 |       0.18
#> qsec     |       | 1.00 |       1.00 |   4.75e-03
#> 
#> The 2 latent factors (oblimin rotation) accounted for 85.83% of the total variance of the original data (MR1 = 63.85%, MR2 = 21.99%).
#> 
factor_analysis(mtcars[, 1:7], n = 2, rotation = "none", threshold = 2, sort = TRUE)
#> # Loadings from Factor Analysis (no rotation)
#> 
#> Variable |  MR1 |  MR2 | Complexity | Uniqueness
#> ------------------------------------------------
#> cyl      | 0.96 |      |       1.01 |       0.08
#> disp     | 0.95 |      |       1.03 |       0.09
#> mpg      |      |      |       1.03 |       0.15
#> wt       |      | 0.36 |       1.33 |       0.10
#> hp       |      |      |       1.23 |       0.18
#> drat     |      |      |       1.49 |       0.39
#> qsec     |      | 0.83 |       1.74 |   4.75e-03
#> 
#> The 2 latent factors accounted for 85.83% of the total variance of the original data (MR1 = 70.67%, MR2 = 15.16%).
#> 

efa <- factor_analysis(mtcars[, 1:5], n = 2)
summary(efa)
#> # (Explained) Variance of Components
#> 
#> Parameter                       |   MR1 |   MR2
#> -----------------------------------------------
#> Eigenvalues                     | 3.908 | 0.398
#> Variance Explained              | 0.531 | 0.331
#> Variance Explained (Cumulative) | 0.531 | 0.861
#> Variance Explained (Proportion) | 0.616 | 0.384
#> 
#> # Factor Correlations
#> 
#> Factor |    MR1 |    MR2
#> ------------------------
#> MR1    |  1.000 | -0.647
#> MR2    | -0.647 |  1.000
predict(efa, verbose = FALSE)
#>           MR1         MR2
#> 1   0.2845528 -0.54283674
#> 2   0.2845528 -0.54283674
#> 3   0.9024152 -0.77336128
#> 4  -0.3183886 -0.57046758
#> 5  -0.9461598  0.39041871
#> 6  -0.4955643 -0.68125903
#> 7  -0.8538301  1.43513473
#> 8   0.6806493 -1.24168944
#> 9   0.8412617 -0.73810505
#> 10  0.2449176 -0.35370543
#> 11  0.1907012 -0.36178666
#> 12 -0.8034782  0.44546524
#> 13 -0.7686248  0.45066032
#> 14 -0.8499494  0.43853847
#> 15 -1.5827127  0.79657207
#> 16 -1.4851863  0.95143952
#> 17 -1.1160586  1.21381686
#> 18  1.3609673 -1.12141178
#> 19  1.5876686 -1.31016252
#> 20  1.4944469 -1.12240438
#> 21  0.7696658 -0.72565105
#> 22 -1.1894582 -0.02829063
#> 23 -1.0026052 -0.01404053
#> 24 -0.6533523  1.45113448
#> 25 -1.0723893  0.39243905
#> 26  1.1625846 -1.15083452
#> 27  1.2175257 -0.75985383
#> 28  1.2704330 -0.42860113
#> 29 -0.2969720  1.77639903
#> 30  0.3866779  0.42813430
#> 31 -0.2138043  2.82273115
#> 32  0.9695135 -0.52558562

# \donttest{
# Automated number of components
factor_analysis(mtcars[, 1:4], n = "auto")
#> Warning: convergence not obtained in GPFoblq. 1000 iterations used.
#> # Rotated loadings from Factor Analysis (oblimin-rotation)
#> 
#> Variable |   MR1 | Complexity | Uniqueness
#> ------------------------------------------
#> mpg      | -0.90 |       1.00 |       0.19
#> cyl      |  0.96 |       1.00 |       0.08
#> disp     |  0.93 |       1.00 |       0.13
#> hp       |  0.86 |       1.00 |       0.26
#> 
#> The unique latent factor (oblimin rotation) accounted for 83.55% of the total variance of the original data.
#> 
# }
```

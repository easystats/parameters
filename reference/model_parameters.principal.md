# Parameters from PCA, FA, CFA, SEM

Format structural models from the **psych** or **FactoMineR** packages.
There is a [`summary()`](https://rdrr.io/r/base/summary.html) method for
the returned output from
[`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md),
to show further information. See 'Examples'.

## Usage

``` r
# S3 method for class 'lavaan'
model_parameters(
  model,
  ci = 0.95,
  standardize = FALSE,
  component = c("regression", "correlation", "loading", "defined"),
  keep = NULL,
  drop = NULL,
  verbose = TRUE,
  ...
)

# S3 method for class 'principal'
model_parameters(
  model,
  sort = FALSE,
  threshold = NULL,
  labels = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- model:

  Model object.

- ci:

  Confidence Interval (CI) level. Default to `0.95` (`95%`).

- standardize:

  Return standardized parameters (standardized coefficients). Can be
  `TRUE` (or `"all"` or `"std.all"`) for standardized estimates based on
  both the variances of observed and latent variables; `"latent"` (or
  `"std.lv"`) for standardized estimates based on the variances of the
  latent variables only; or `"no_exogenous"` (or `"std.nox"`) for
  standardized estimates based on both the variances of observed and
  latent variables, but not the variances of exogenous covariates. See
  [`lavaan::standardizedsolution`](https://rdrr.io/pkg/lavaan/man/standardizedSolution.html)
  for details.

- component:

  What type of links to return. Can be `"all"` or some of
  `c("regression", "correlation", "loading", "variance", "mean")`.

- keep:

  Character containing a regular expression pattern that describes the
  parameters that should be included (for `keep`) or excluded (for
  `drop`) in the returned data frame. `keep` may also be a named list of
  regular expressions. All non-matching parameters will be removed from
  the output. If `keep` is a character vector, every parameter name in
  the *"Parameter"* column that matches the regular expression in `keep`
  will be selected from the returned data frame (and vice versa, all
  parameter names matching `drop` will be excluded). Furthermore, if
  `keep` has more than one element, these will be merged with an `OR`
  operator into a regular expression pattern like this:
  `"(one|two|three)"`. If `keep` is a named list of regular expression
  patterns, the names of the list-element should equal the column name
  where selection should be applied. This is useful for model objects
  where
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  returns multiple columns with parameter components, like in
  `model_parameters.lavaan()`. Note that the regular expression pattern
  should match the parameter names as they are stored in the returned
  data frame, which can be different from how they are printed. Inspect
  the `$Parameter` column of the parameters table to get the exact
  parameter names.

- drop:

  See `keep`.

- verbose:

  Toggle warnings.

- ...:

  Arguments passed to or from other methods.

- sort:

  Sort the loadings.

- threshold:

  A value between 0 and 1 indicates which (absolute) values from the
  loadings should be removed. An integer higher than 1 indicates the n
  strongest loadings to retain. Can also be `"max"`, in which case it
  will only display the maximum loading per variable (the most simple
  structure).

- labels:

  A character vector containing labels to be added to the loadings data.
  Usually, the question related to the item.

## Value

A data frame of indices or loadings.

## Details

For the structural models obtained with **psych**, the following indices
are present:

- **Complexity** (Hoffman's, 1978; Pettersson and Turkheimer, 2010)
  represents the number of latent components needed to account for the
  observed variables. Whereas a perfect simple structure solution has a
  complexity of 1 in that each item would only load on one factor, a
  solution with evenly distributed items has a complexity greater than
  1.

- **Uniqueness** represents the variance that is 'unique' to the
  variable and not shared with other variables. It is equal to
  `1 – communality` (variance that is shared with other variables). A
  uniqueness of `0.20` suggests that `20%` or that variable's variance
  is not shared with other variables in the overall factor model. The
  greater 'uniqueness' the lower the relevance of the variable in the
  factor model.

- **MSA** represents the Kaiser-Meyer-Olkin Measure of Sampling Adequacy
  (Kaiser and Rice, 1974) for each item. It indicates whether there is
  enough data for each factor give reliable results for the PCA. The
  value should be \> 0.6, and desirable values are \> 0.8 (Tabachnick
  and Fidell, 2013).

## Note

There is also a
[[`plot()`](https://rdrr.io/r/graphics/plot.default.html)-method](https://easystats.github.io/see/articles/parameters.html)
for `lavaan` models implemented in the
[**see**-package](https://easystats.github.io/see/).

## References

- Kaiser, H.F. and Rice. J. (1974). Little jiffy, mark iv. Educational
  and Psychological Measurement, 34(1):111–117

- Pettersson, E., and Turkheimer, E. (2010). Item selection, evaluation,
  and simple structure in personality data. Journal of research in
  personality, 44(4), 407-420.

- Revelle, W. (2016). How To: Use the psych package for Factor Analysis
  and data reduction.

- Tabachnick, B. G., and Fidell, L. S. (2013). Using multivariate
  statistics (6th ed.). Boston: Pearson Education.

- Rosseel Y (2012). lavaan: An R Package for Structural Equation
  Modeling. Journal of Statistical Software, 48(2), 1-36.

- Merkle EC , Rosseel Y (2018). blavaan: Bayesian Structural Equation
  Models via Parameter Expansion. Journal of Statistical Software,
  85(4), 1-30. http://www.jstatsoft.org/v85/i04/

## Examples

``` r
library(parameters)
# \donttest{
# Principal Component Analysis (PCA) ---------
data(attitude)
pca <- psych::principal(attitude)
model_parameters(pca)
#> # Rotated loadings from Principal Component Analysis (varimax-rotation)
#> 
#> Variable   |  PC1 | Complexity | Uniqueness
#> -------------------------------------------
#> rating     | 0.80 |       1.00 |       0.37
#> complaints | 0.85 |       1.00 |       0.28
#> privileges | 0.68 |       1.00 |       0.53
#> learning   | 0.83 |       1.00 |       0.32
#> raises     | 0.86 |       1.00 |       0.26
#> critical   | 0.36 |       1.00 |       0.87
#> advance    | 0.58 |       1.00 |       0.66
#> 
#> The unique principal component (varimax rotation) accounted for 53.09% of the total variance of the original data.
#> 
summary(model_parameters(pca))
#> # (Explained) Variance of Components
#> 
#> Parameter                       |   PC1
#> ---------------------------------------
#> Eigenvalues                     | 3.716
#> Variance Explained              | 0.531
#> Variance Explained (Cumulative) | 0.531
#> Variance Explained (Proportion) | 1.000

pca <- psych::principal(attitude, nfactors = 3, rotate = "none")
model_parameters(pca, sort = TRUE, threshold = 0.2)
#> # Loadings from Principal Component Analysis (no rotation)
#> 
#> Variable   |  PC1 |   PC2 |   PC3 | Complexity | Uniqueness
#> -----------------------------------------------------------
#> raises     | 0.86 |       |       |       1.10 |       0.22
#> complaints | 0.85 | -0.36 |  0.21 |       1.48 |       0.11
#> learning   | 0.83 |       | -0.30 |       1.27 |       0.23
#> rating     | 0.80 | -0.42 |  0.24 |       1.74 |       0.13
#> privileges | 0.68 |       |       |       1.18 |       0.49
#> advance    | 0.58 |  0.61 | -0.46 |       2.85 |       0.08
#> critical   | 0.36 |  0.64 |  0.65 |       2.54 |       0.04
#> 
#> The 3 principal components accounted for 81.49% of the total variance of the original data (PC1 = 53.09%, PC2 = 16.30%, PC3 = 12.10%).
#> 

principal_components(attitude, n = 3, sort = TRUE, threshold = 0.2)
#> # Loadings from Principal Component Analysis (no rotation)
#> 
#> Variable   |  PC1 |   PC2 |   PC3 | Complexity
#> ----------------------------------------------
#> raises     | 0.86 |       |       |       1.10
#> complaints | 0.85 |  0.36 | -0.21 |       1.48
#> learning   | 0.83 |       |  0.30 |       1.27
#> rating     | 0.80 |  0.42 | -0.24 |       1.74
#> privileges | 0.68 |       |       |       1.18
#> advance    | 0.58 | -0.61 |  0.46 |       2.85
#> critical   | 0.36 | -0.64 | -0.65 |       2.54
#> 
#> The 3 principal components accounted for 81.49% of the total variance of the original data (PC1 = 53.09%, PC2 = 16.30%, PC3 = 12.10%).
#> 


# Exploratory Factor Analysis (EFA) ---------
efa <- psych::fa(attitude, nfactors = 3)
model_parameters(efa,
  threshold = "max", sort = TRUE,
  labels = as.character(1:ncol(attitude))
)
#> # Rotated loadings from Factor Analysis (oblimin-rotation)
#> 
#> Variable   | Label |  MR1 |  MR2 |  MR3 | Complexity | Uniqueness
#> -----------------------------------------------------------------
#> complaints |     2 | 0.97 |      |      |       1.01 |       0.10
#> rating     |     1 | 0.90 |      |      |       1.02 |       0.23
#> raises     |     5 | 0.55 |      |      |       2.35 |       0.23
#> privileges |     3 | 0.44 |      |      |       1.64 |       0.65
#> advance    |     7 |      | 0.91 |      |       1.04 |       0.22
#> learning   |     4 |      | 0.54 |      |       2.51 |       0.24
#> critical   |     6 |      |      | 0.48 |       1.46 |       0.67
#> 
#> The 3 latent factors (oblimin rotation) accounted for 66.60% of the total variance of the original data (MR1 = 38.19%, MR2 = 22.69%, MR3 = 5.72%).
#> 


# Omega ---------
data(mtcars)
omega <- psych::omega(mtcars, nfactors = 3, plot = FALSE)
params <- model_parameters(omega)
params
#> # Rotated loadings from Omega (oblimin-rotation)
#> 
#> Variable |        g |   F1* |      F2* |       F3* |   h2 |   u2 |       p2 | Complexity
#> ----------------------------------------------------------------------------------------
#> mpg-     |     0.58 | -0.67 |     0.09 |      0.29 | 0.88 | 0.12 |     0.38 |       2.40
#> cyl      |     0.70 | -0.61 |     0.28 |      0.07 | 0.96 | 0.04 |     0.52 |       2.33
#> disp     |     0.59 | -0.71 |     0.18 |      0.11 | 0.89 | 0.11 |     0.39 |       2.13
#> hp       |     0.77 | -0.31 |     0.23 |      0.36 | 0.87 | 0.13 |     0.68 |       2.00
#> drat-    |     0.27 | -0.79 |     0.06 |     -0.07 | 0.71 | 0.29 |     0.10 |       1.26
#> wt       |     0.43 | -0.79 |    -0.04 |      0.31 | 0.91 | 0.09 |     0.20 |       1.87
#> qsec-    |     0.81 |  0.19 |     0.50 |      0.06 | 0.95 | 0.05 |     0.70 |       1.81
#> vs-      |     0.74 | -0.27 |     0.38 |      0.05 | 0.77 | 0.23 |     0.71 |       1.81
#> am-      | 8.38e-03 | -0.89 |    -0.15 | -9.51e-03 | 0.81 | 0.19 | 8.63e-05 |       1.06
#> gear     |     0.03 |  0.87 | 9.01e-03 |      0.32 | 0.87 | 0.13 | 9.03e-04 |       1.27
#> carb     |     0.68 |  0.06 |     0.10 |      0.63 | 0.87 | 0.13 |     0.53 |       2.06
summary(params)
#> # Omega Statistics
#> 
#> Statistic            | Coefficient
#> ----------------------------------
#> Alpha                |        0.88
#> G.6                  |        0.97
#> Omega (hierarchical) |        0.57
#> Omega (asymptotic H) |        0.58
#> Omega (total)        |        0.97
#> 
#> # Omega Coefficients
#> 
#> Composite | Omega (total) | Omega (hierarchical) | Omega (group)
#> ----------------------------------------------------------------
#> g         |          0.97 |                 0.57 |          0.26
#> F1*       |          0.90 |                 0.31 |          0.59
#> F2*       |          0.91 |                 0.69 |          0.22
#> F3*       |          0.87 |                 0.60 |          0.28
#> 
#> # Variances
#> 
#> Composite | Total (%) | General Factor (%) | Group Factor (%)
#> -------------------------------------------------------------
#> g         |     97.28 |              56.64 |            26.42
#> F1*       |     90.12 |              31.07 |            59.05
#> F2*       |     91.37 |              69.32 |            22.04
#> F3*       |     87.36 |              59.65 |            27.71
# }


# lavaan -------------------------------------
# Confirmatory Factor Analysis (CFA) ---------

data(HolzingerSwineford1939, package = "lavaan")
structure <- " visual  =~ x1 + x2 + x3
               textual =~ x4 + x5 + x6
               speed   =~ x7 + x8 + x9 "
model <- lavaan::cfa(structure, data = HolzingerSwineford1939)
model_parameters(model)
#> # Loading 
#> 
#> Link          | Coefficient |   SE |       95% CI |     z |      p
#> ------------------------------------------------------------------
#> visual =~ x1  |        1.00 | 0.00 | [1.00, 1.00] |       | < .001
#> visual =~ x2  |        0.55 | 0.10 | [0.36, 0.75] |  5.55 | < .001
#> visual =~ x3  |        0.73 | 0.11 | [0.52, 0.94] |  6.68 | < .001
#> textual =~ x4 |        1.00 | 0.00 | [1.00, 1.00] |       | < .001
#> textual =~ x5 |        1.11 | 0.07 | [0.98, 1.24] | 17.01 | < .001
#> textual =~ x6 |        0.93 | 0.06 | [0.82, 1.03] | 16.70 | < .001
#> speed =~ x7   |        1.00 | 0.00 | [1.00, 1.00] |       | < .001
#> speed =~ x8   |        1.18 | 0.16 | [0.86, 1.50] |  7.15 | < .001
#> speed =~ x9   |        1.08 | 0.15 | [0.79, 1.38] |  7.15 | < .001
#> 
#> # Correlation 
#> 
#> Link              | Coefficient |   SE |       95% CI |    z |      p
#> ---------------------------------------------------------------------
#> visual ~~ textual |        0.41 | 0.07 | [0.26, 0.55] | 5.55 | < .001
#> visual ~~ speed   |        0.26 | 0.06 | [0.15, 0.37] | 4.66 | < .001
#> textual ~~ speed  |        0.17 | 0.05 | [0.08, 0.27] | 3.52 | < .001
model_parameters(model, standardize = TRUE)
#> # Loading 
#> 
#> Link          | Coefficient |   SE |       95% CI |     z |      p
#> ------------------------------------------------------------------
#> visual =~ x1  |        0.77 | 0.05 | [0.66, 0.88] | 14.04 | < .001
#> visual =~ x2  |        0.42 | 0.06 | [0.31, 0.54] |  7.11 | < .001
#> visual =~ x3  |        0.58 | 0.06 | [0.47, 0.69] | 10.54 | < .001
#> textual =~ x4 |        0.85 | 0.02 | [0.81, 0.90] | 37.78 | < .001
#> textual =~ x5 |        0.86 | 0.02 | [0.81, 0.90] | 38.27 | < .001
#> textual =~ x6 |        0.84 | 0.02 | [0.79, 0.88] | 35.88 | < .001
#> speed =~ x7   |        0.57 | 0.05 | [0.47, 0.67] | 10.71 | < .001
#> speed =~ x8   |        0.72 | 0.05 | [0.62, 0.82] | 14.31 | < .001
#> speed =~ x9   |        0.67 | 0.05 | [0.56, 0.77] | 13.02 | < .001
#> 
#> # Correlation 
#> 
#> Link              | Coefficient |   SE |       95% CI |    z |      p
#> ---------------------------------------------------------------------
#> visual ~~ textual |        0.46 | 0.06 | [0.33, 0.58] | 7.19 | < .001
#> visual ~~ speed   |        0.47 | 0.07 | [0.33, 0.61] | 6.46 | < .001
#> textual ~~ speed  |        0.28 | 0.07 | [0.15, 0.42] | 4.12 | < .001

# filter parameters
model_parameters(
  model,
  parameters = list(
    To = "^(?!visual)",
    From = "^(?!(x7|x8))"
  )
)
#> # Loading 
#> 
#> Link          | Coefficient |   SE |       95% CI |     z |      p
#> ------------------------------------------------------------------
#> visual =~ x1  |        1.00 | 0.00 | [1.00, 1.00] |       | < .001
#> visual =~ x2  |        0.55 | 0.10 | [0.36, 0.75] |  5.55 | < .001
#> visual =~ x3  |        0.73 | 0.11 | [0.52, 0.94] |  6.68 | < .001
#> textual =~ x4 |        1.00 | 0.00 | [1.00, 1.00] |       | < .001
#> textual =~ x5 |        1.11 | 0.07 | [0.98, 1.24] | 17.01 | < .001
#> textual =~ x6 |        0.93 | 0.06 | [0.82, 1.03] | 16.70 | < .001
#> speed =~ x7   |        1.00 | 0.00 | [1.00, 1.00] |       | < .001
#> speed =~ x8   |        1.18 | 0.16 | [0.86, 1.50] |  7.15 | < .001
#> speed =~ x9   |        1.08 | 0.15 | [0.79, 1.38] |  7.15 | < .001
#> 
#> # Correlation 
#> 
#> Link              | Coefficient |   SE |       95% CI |    z |      p
#> ---------------------------------------------------------------------
#> visual ~~ textual |        0.41 | 0.07 | [0.26, 0.55] | 5.55 | < .001
#> visual ~~ speed   |        0.26 | 0.06 | [0.15, 0.37] | 4.66 | < .001
#> textual ~~ speed  |        0.17 | 0.05 | [0.08, 0.27] | 3.52 | < .001

# Structural Equation Model (SEM) ------------

data(PoliticalDemocracy, package = "lavaan")
structure <- "
  # latent variable definitions
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + a*y2 + b*y3 + c*y4
    dem65 =~ y5 + a*y6 + b*y7 + c*y8
  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
  # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
"
model <- lavaan::sem(structure, data = PoliticalDemocracy)
model_parameters(model)
#> # Loading 
#> 
#> Link            | Coefficient |   SE |       95% CI |     z |      p
#> --------------------------------------------------------------------
#> ind60 =~ x1     |        1.00 | 0.00 | [1.00, 1.00] |       | < .001
#> ind60 =~ x2     |        2.18 | 0.14 | [1.91, 2.45] | 15.75 | < .001
#> ind60 =~ x3     |        1.82 | 0.15 | [1.52, 2.12] | 11.97 | < .001
#> dem60 =~ y1     |        1.00 | 0.00 | [1.00, 1.00] |       | < .001
#> dem60 =~ y2 (a) |        1.19 | 0.14 | [0.92, 1.46] |  8.55 | < .001
#> dem60 =~ y3 (b) |        1.17 | 0.12 | [0.94, 1.41] |  9.76 | < .001
#> dem60 =~ y4 (c) |        1.25 | 0.12 | [1.02, 1.48] | 10.71 | < .001
#> dem65 =~ y5     |        1.00 | 0.00 | [1.00, 1.00] |       | < .001
#> dem65 =~ y6 (a) |        1.19 | 0.14 | [0.92, 1.46] |  8.55 | < .001
#> dem65 =~ y7 (b) |        1.17 | 0.12 | [0.94, 1.41] |  9.76 | < .001
#> dem65 =~ y8 (c) |        1.25 | 0.12 | [1.02, 1.48] | 10.71 | < .001
#> 
#> # Regression 
#> 
#> Link          | Coefficient |   SE |       95% CI |     z |      p
#> ------------------------------------------------------------------
#> dem60 ~ ind60 |        1.47 | 0.39 | [0.70, 2.24] |  3.75 | < .001
#> dem65 ~ ind60 |        0.60 | 0.23 | [0.16, 1.04] |  2.66 | 0.008 
#> dem65 ~ dem60 |        0.87 | 0.07 | [0.72, 1.01] | 11.55 | < .001
#> 
#> # Correlation 
#> 
#> Link     | Coefficient |   SE |        95% CI |    z |     p
#> ------------------------------------------------------------
#> y1 ~~ y5 |        0.58 | 0.36 | [-0.11, 1.28] | 1.64 | 0.102
#> y2 ~~ y4 |        1.44 | 0.69 | [ 0.09, 2.79] | 2.09 | 0.036
#> y2 ~~ y6 |        2.18 | 0.74 | [ 0.74, 3.63] | 2.96 | 0.003
#> y3 ~~ y7 |        0.71 | 0.61 | [-0.49, 1.91] | 1.16 | 0.244
#> y4 ~~ y8 |        0.36 | 0.44 | [-0.51, 1.23] | 0.82 | 0.414
#> y6 ~~ y8 |        1.37 | 0.58 | [ 0.24, 2.50] | 2.38 | 0.017
model_parameters(model, standardize = TRUE)
#> # Loading 
#> 
#> Link            | Coefficient |   SE |       95% CI |     z |      p
#> --------------------------------------------------------------------
#> ind60 =~ x1     |        0.92 | 0.02 | [0.88, 0.97] | 40.08 | < .001
#> ind60 =~ x2     |        0.97 | 0.02 | [0.94, 1.01] | 59.14 | < .001
#> ind60 =~ x3     |        0.87 | 0.03 | [0.81, 0.93] | 28.09 | < .001
#> dem60 =~ y1     |        0.85 | 0.04 | [0.77, 0.93] | 20.92 | < .001
#> dem60 =~ y2 (a) |        0.69 | 0.06 | [0.57, 0.81] | 11.58 | < .001
#> dem60 =~ y3 (b) |        0.76 | 0.05 | [0.66, 0.86] | 14.70 | < .001
#> dem60 =~ y4 (c) |        0.84 | 0.04 | [0.76, 0.92] | 20.12 | < .001
#> dem65 =~ y5     |        0.82 | 0.04 | [0.73, 0.90] | 18.52 | < .001
#> dem65 =~ y6 (a) |        0.75 | 0.05 | [0.65, 0.86] | 14.01 | < .001
#> dem65 =~ y7 (b) |        0.80 | 0.05 | [0.71, 0.89] | 17.40 | < .001
#> dem65 =~ y8 (c) |        0.83 | 0.04 | [0.75, 0.91] | 19.79 | < .001
#> 
#> # Regression 
#> 
#> Link          | Coefficient |   SE |       95% CI |     z |      p
#> ------------------------------------------------------------------
#> dem60 ~ ind60 |        0.45 | 0.10 | [0.25, 0.65] |  4.33 | < .001
#> dem65 ~ ind60 |        0.19 | 0.07 | [0.05, 0.33] |  2.64 | 0.008 
#> dem65 ~ dem60 |        0.88 | 0.05 | [0.78, 0.98] | 17.24 | < .001
#> 
#> # Correlation 
#> 
#> Link     | Coefficient |   SE |        95% CI |    z |      p
#> -------------------------------------------------------------
#> y1 ~~ y5 |        0.28 | 0.14 | [ 0.00, 0.56] | 1.97 | 0.049 
#> y2 ~~ y4 |        0.29 | 0.11 | [ 0.07, 0.52] | 2.55 | 0.011 
#> y2 ~~ y6 |        0.36 | 0.10 | [ 0.17, 0.54] | 3.71 | < .001
#> y3 ~~ y7 |        0.17 | 0.13 | [-0.09, 0.43] | 1.26 | 0.208 
#> y4 ~~ y8 |        0.11 | 0.13 | [-0.14, 0.36] | 0.86 | 0.388 
#> y6 ~~ y8 |        0.34 | 0.11 | [ 0.12, 0.55] | 3.08 | 0.002 
```

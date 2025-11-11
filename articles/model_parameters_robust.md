# Robust Estimation of Standard Errors, Confidence Intervals, and p-values

The
[`model_parameters()`](https://easystats.github.io/parameters/articles/model_parameters.html)
function also allows the computation of standard errors, confidence
intervals, and *p*-values based on various covariance matrices:
heteroskedasticity-consistent, cluster-robust, bootstrap, etc. This
functionality relies on the
[`sandwich`](https://cran.r-project.org/web/packages/sandwich/vignettes/sandwich-CL.pdf)
and
[`clubSandwich`](https://cran.r-project.org/web/packages/clubSandwich/index.html)
packages. This means that all models supported by either of these
packages should work with
[`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md).

The computation of robust standard errors is controlled by two
arguments:

1.  `vcov`: accepts 3 types of arguments
    - A covariance matrix
    - A function which returns a covariance matrix (e.g.,
      [`stats::vcov()`](https://rdrr.io/r/stats/vcov.html))
    - A string which indicates the kind of uncertainty estimates to
      return.
      - Heteroskedasticity-consistent: `"vcovHC"`, `"HC"`, `"HC0"`,
        `"HC1"`, `"HC2"`, `"HC3"`, `"HC4"`, `"HC4m"`, `"HC5"`
      - Cluster-robust: `"vcovCR"`, `"CR0"`, `"CR1"`, `"CR1p"`,
        `"CR1S"`, `"CR2"`, `"CR3"`
      - Bootstrap: `"vcovBS"`, `"xy"`, `"residual"`, `"wild"`,
        `"mammen"`, `"webb"`
      - Other `sandwich` functions: `"vcovHAC"`, `"vcovPC"`, `"vcovCL"`,
        `"vcovPL"`
2.  `vcov_args`: list of arguments passed to the *sandwich* or
    *clubSandwich* function used to compute the covariance matrix. See
    for example
    [`?sandwich::vcovHAC`](https://sandwich.R-Forge.R-project.org/reference/vcovHAC.html)

## Linear Regression Models

### Robust Covariance Matrix Estimation from Model Parameters

Let us start with a simple example, which uses a
heteroskedasticity-consistent covariance matrix estimation with
estimation-type “HC3” (i.e. `sandwich::vcovHC(type = "HC3")`).

First let’s create a simple linear regression model, which we know
violates homoscedasticity assumption, and thus robust estimation methods
are to be considered.

``` r

data(cars)
model <- lm(dist ~ speed, data = cars)

library(performance)
check_heteroscedasticity(model)
#> Warning: Heteroscedasticity (non-constant error variance) detected (p = 0.031).
```

We would extract model parameters both with and without robust
estimation to highlight the difference it makes to standard errors,
confidence intervals, *t*-statistic, and *p*-values. Also, note that the
coefficient estimate remains unchanged.

``` r

# model parameters, where SE, CI, and p-values are *not* based on robust estimation
model_parameters(model)
#> Parameter   | Coefficient |   SE |          95% CI | t(48) |      p
#> -------------------------------------------------------------------
#> (Intercept) |      -17.58 | 6.76 | [-31.17, -3.99] | -2.60 | 0.012 
#> speed       |        3.93 | 0.42 | [  3.10,  4.77] |  9.46 | < .001

# model parameters, where SE, CI, and p-values are based on robust estimation
mp <- model_parameters(model, vcov = "HC3")
mp
#> Parameter   | Coefficient |   SE |          95% CI | t(48) |      p
#> -------------------------------------------------------------------
#> (Intercept) |      -17.58 | 5.93 | [-29.51, -5.65] | -2.96 | 0.005 
#> speed       |        3.93 | 0.43 | [  3.07,  4.79] |  9.20 | < .001

# compare standard errors to result from sandwich-package
mp$SE
#> [1] 5.93 0.43
unname(sqrt(diag(sandwich::vcovHC(model))))
#> [1] 5.93 0.43
```

### Cluster-Robust Covariance Matrix Estimation (sandwich)

If a different type of covariance matrix estimation is required, use the
`vcov`-argument. This argument accepts the name of a function from the
*sandwich* or *clubSandwich* packages as a string, such as `"vcovCL"`
(or just its suffix `"CL"`). *parameters* will then call the
corresponding function with the content of `vcov_args` as arguments.

The specific estimation type can be controlled by passing a `type`
argument via `vcov_args`. See
[`?sandwich::vcovCL`](https://sandwich.R-Forge.R-project.org/reference/vcovCL.html)
for information about the different types of covariance matrices that
this function can produce (`HC0` to `HC3`). In the next example, we use
a clustered covariance matrix estimation with `HC1`-estimation type.

``` r

# let's create a more complicated model
data(iris)
model <- lm(Petal.Length ~ Sepal.Length * Species + Sepal.Width, data = iris)

# change estimation-type
mp <- model_parameters(
  model,
  vcov = "CL", # type of covariance matrix
  vcov_args = list(type = "HC1") # type of robust estimation
)

mp
#> Parameter                           | Coefficient |   SE |        95% CI
#> ------------------------------------------------------------------------
#> (Intercept)                         |        0.87 | 0.42 | [ 0.03, 1.70]
#> Sepal Length                        |        0.04 | 0.11 | [-0.18, 0.26]
#> Species [versicolor]                |       -0.78 | 0.65 | [-2.07, 0.51]
#> Species [virginica]                 |       -0.41 | 0.59 | [-1.57, 0.75]
#> Sepal Width                         |        0.11 | 0.08 | [-0.05, 0.27]
#> Sepal Length × Species [versicolor] |        0.61 | 0.12 | [ 0.37, 0.85]
#> Sepal Length × Species [virginica]  |        0.68 | 0.11 | [ 0.46, 0.90]
#> 
#> Parameter                           | t(143) |      p
#> -----------------------------------------------------
#> (Intercept)                         |   2.05 | 0.042 
#> Sepal Length                        |   0.40 | 0.692 
#> Species [versicolor]                |  -1.19 | 0.237 
#> Species [virginica]                 |  -0.70 | 0.483 
#> Sepal Width                         |   1.38 | 0.170 
#> Sepal Length × Species [versicolor] |   4.96 | < .001
#> Sepal Length × Species [virginica]  |   6.15 | < .001

# compare standard errors to result from sandwich-package
mp$SE
#> [1] 0.422 0.111 0.653 0.587 0.079 0.123 0.111
unname(sqrt(diag(sandwich::vcovCL(model))))
#> [1] 0.422 0.111 0.653 0.587 0.079 0.123 0.111
```

Usually, clustered covariance matrix estimation is used when there is a
cluster-structure in the data. The variable indicating the
cluster-structure can be defined in
[`sandwich::vcovCL()`](https://sandwich.R-Forge.R-project.org/reference/vcovCL.html)
with the `cluster`-argument. In
[`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md),
additional arguments that should be passed down to functions from the
*sandwich* package can be specified in `vcov_args`:

``` r

iris$cluster <- factor(rep(LETTERS[1:8], length.out = nrow(iris)))

# change estimation-type, defining additional arguments
mp <- model_parameters(
  model,
  vcov = "vcovCL",
  vcov_args = list(type = "HC1", cluster = iris$cluster)
)

mp
#> Parameter                           | Coefficient |   SE |        95% CI
#> ------------------------------------------------------------------------
#> (Intercept)                         |        0.87 | 0.34 | [ 0.20, 1.53]
#> Sepal Length                        |        0.04 | 0.07 | [-0.10, 0.19]
#> Species [versicolor]                |       -0.78 | 0.52 | [-1.80, 0.25]
#> Species [virginica]                 |       -0.41 | 0.26 | [-0.94, 0.11]
#> Sepal Width                         |        0.11 | 0.07 | [-0.03, 0.25]
#> Sepal Length × Species [versicolor] |        0.61 | 0.10 | [ 0.42, 0.80]
#> Sepal Length × Species [virginica]  |        0.68 | 0.05 | [ 0.58, 0.78]
#> 
#> Parameter                           | t(143) |      p
#> -----------------------------------------------------
#> (Intercept)                         |   2.57 | 0.011 
#> Sepal Length                        |   0.61 | 0.540 
#> Species [versicolor]                |  -1.49 | 0.137 
#> Species [virginica]                 |  -1.56 | 0.120 
#> Sepal Width                         |   1.52 | 0.131 
#> Sepal Length × Species [versicolor] |   6.29 | < .001
#> Sepal Length × Species [virginica]  |  13.28 | < .001

# compare standard errors to result from sandwich-package
mp$SE
#> [1] 0.337 0.072 0.519 0.264 0.072 0.097 0.051
unname(sqrt(diag(sandwich::vcovCL(model, cluster = iris$cluster))))
#> [1] 0.337 0.072 0.519 0.264 0.072 0.097 0.051
```

### Cluster-Robust Covariance Matrix Estimation (clubSandwich)

using
[`clubSandwich::vcovCR()`](http://jepusto.github.io/clubSandwich/reference/vcovCR.md).
Thus, when `vcov = "CR"`, the related function from the `clubSandwich`
package is called. Note that this function *requires* the specification
of the `cluster`-argument.

``` r

# create fake-cluster-variable, to demonstrate cluster robust standard errors
iris$cluster <- factor(rep(LETTERS[1:8], length.out = nrow(iris)))

# cluster-robust estimation
mp <- model_parameters(
  model,
  vcov = "vcovCR",
  vcov_args = list(type = "CR1", cluster = iris$cluster)
)
mp
#> Parameter                           | Coefficient |   SE |        95% CI
#> ------------------------------------------------------------------------
#> (Intercept)                         |        0.87 | 0.33 | [ 0.21, 1.52]
#> Sepal Length                        |        0.04 | 0.07 | [-0.10, 0.18]
#> Species [versicolor]                |       -0.78 | 0.51 | [-1.78, 0.23]
#> Species [virginica]                 |       -0.41 | 0.26 | [-0.92, 0.10]
#> Sepal Width                         |        0.11 | 0.07 | [-0.03, 0.25]
#> Sepal Length × Species [versicolor] |        0.61 | 0.09 | [ 0.42, 0.79]
#> Sepal Length × Species [virginica]  |        0.68 | 0.05 | [ 0.58, 0.78]
#> 
#> Parameter                           | t(143) |      p
#> -----------------------------------------------------
#> (Intercept)                         |   2.62 | 0.010 
#> Sepal Length                        |   0.63 | 0.531 
#> Species [versicolor]                |  -1.53 | 0.129 
#> Species [virginica]                 |  -1.60 | 0.112 
#> Sepal Width                         |   1.55 | 0.123 
#> Sepal Length × Species [versicolor] |   6.42 | < .001
#> Sepal Length × Species [virginica]  |  13.56 | < .001

# compare standard errors to result from clubSsandwich-package
mp$SE
#> [1] 0.330 0.070 0.508 0.259 0.071 0.095 0.050

unname(sqrt(diag(clubSandwich::vcovCR(model, type = "CR1", cluster = iris$cluster))))
#> [1] 0.330 0.070 0.508 0.259 0.071 0.095 0.050
```

### Robust Covariance Matrix Estimation on Standardized Model Parameters

Finally, robust estimation can be combined with
[standardization](https://easystats.github.io/parameters/articles/standardize_parameters_effsize.html).
However, robust covariance matrix estimation only works for
`standardize = "refit"`.

``` r

# model parameters, robust estimation on standardized model
model_parameters(model, standardize = "refit", vcov = "HC3")
#> Parameter                           | Coefficient |   SE |         95% CI
#> -------------------------------------------------------------------------
#> (Intercept)                         |       -1.30 | 0.07 | [-1.44, -1.16]
#> Sepal Length                        |        0.02 | 0.06 | [-0.09,  0.13]
#> Species [versicolor]                |        1.57 | 0.09 | [ 1.40,  1.74]
#> Species [virginica]                 |        2.02 | 0.09 | [ 1.84,  2.20]
#> Sepal Width                         |        0.03 | 0.02 | [-0.01,  0.07]
#> Sepal Length × Species [versicolor] |        0.28 | 0.06 | [ 0.16,  0.41]
#> Sepal Length × Species [virginica]  |        0.32 | 0.06 | [ 0.21,  0.43]
#> 
#> Parameter                           | t(143) |      p
#> -----------------------------------------------------
#> (Intercept)                         | -18.70 | < .001
#> Sepal Length                        |   0.37 | 0.711 
#> Species [versicolor]                |  17.84 | < .001
#> Species [virginica]                 |  22.49 | < .001
#> Sepal Width                         |   1.32 | 0.190 
#> Sepal Length × Species [versicolor] |   4.65 | < .001
#> Sepal Length × Species [virginica]  |   5.75 | < .001
```

## Linear Mixed-Effects Regression Models

### Robust Covariance Matrix Estimation for Mixed Models

For linear mixed-effects models, that by definition have a clustered
(*hierarchical* or *multilevel*) structure in the data, it is also
possible to estimate a cluster-robust covariance matrix. This is
possible due to the *clubSandwich* package, thus we need to define the
same arguments as in the above example.

``` r

library(lme4)
data(iris)
set.seed(1234)
iris$grp <- as.factor(sample(1:3, nrow(iris), replace = TRUE))

# fit example model
model <- lme4::lmer(
  Sepal.Length ~ Species * Sepal.Width + Petal.Length + (1 | grp),
  data = iris
)

# model parameters without robust estimation
model_parameters(model)
#> # Fixed Effects
#> 
#> Parameter                          | Coefficient |   SE |         95% CI
#> ------------------------------------------------------------------------
#> (Intercept)                        |        1.55 | 0.40 | [ 0.76,  2.35]
#> Species [versicolor]               |        0.41 | 0.55 | [-0.67,  1.50]
#> Species [virginica]                |       -0.41 | 0.58 | [-1.56,  0.74]
#> Sepal Width                        |        0.66 | 0.11 | [ 0.44,  0.89]
#> Petal Length                       |        0.82 | 0.07 | [ 0.69,  0.95]
#> Species [versicolor] × Sepal Width |       -0.48 | 0.19 | [-0.85, -0.12]
#> Species [virginica] × Sepal Width  |       -0.36 | 0.18 | [-0.71,  0.00]
#> 
#> Parameter                          | t(141) |      p
#> ----------------------------------------------------
#> (Intercept)                        |   3.87 | < .001
#> Species [versicolor]               |   0.75 | 0.454 
#> Species [virginica]                |  -0.70 | 0.483 
#> Sepal Width                        |   5.83 | < .001
#> Petal Length                       |  12.52 | < .001
#> Species [versicolor] × Sepal Width |  -2.60 | 0.010 
#> Species [virginica] × Sepal Width  |  -1.99 | 0.048 
#> 
#> # Random Effects
#> 
#> Parameter           | Coefficient |   SE |       95% CI
#> -------------------------------------------------------
#> SD (Intercept: grp) |        0.08 | 0.05 | [0.02, 0.29]
#> SD (Residual)       |        0.30 | 0.02 | [0.27, 0.33]

# model parameters with cluster robust estimation
model_parameters(
  model,
  vcov = "vcovCR",
  vcov_args = list(type = "CR1", cluster = iris$grp)
)
#> # Fixed Effects
#> 
#> Parameter                          | Coefficient |   SE |         95% CI
#> ------------------------------------------------------------------------
#> (Intercept)                        |        1.55 | 0.40 | [ 0.76,  2.35]
#> Species [versicolor]               |        0.41 | 0.80 | [-1.17,  1.99]
#> Species [virginica]                |       -0.41 | 0.19 | [-0.78, -0.03]
#> Sepal Width                        |        0.66 | 0.10 | [ 0.46,  0.86]
#> Petal Length                       |        0.82 | 0.05 | [ 0.72,  0.91]
#> Species [versicolor] × Sepal Width |       -0.48 | 0.35 | [-1.18,  0.21]
#> Species [virginica] × Sepal Width  |       -0.36 | 0.11 | [-0.57, -0.15]
#> 
#> Parameter                          | t(141) |      p
#> ----------------------------------------------------
#> (Intercept)                        |   3.87 | < .001
#> Species [versicolor]               |   0.51 | 0.608 
#> Species [virginica]                |  -2.15 | 0.033 
#> Sepal Width                        |   6.64 | < .001
#> Petal Length                       |  17.27 | < .001
#> Species [versicolor] × Sepal Width |  -1.37 | 0.172 
#> Species [virginica] × Sepal Width  |  -3.39 | < .001
#> 
#> # Random Effects
#> 
#> Parameter           | Coefficient |   SE |       95% CI
#> -------------------------------------------------------
#> SD (Intercept: grp) |        0.08 | 0.05 | [0.02, 0.29]
#> SD (Residual)       |        0.30 | 0.02 | [0.27, 0.33]
```

Notice that robust estimation returns different standard errors,
confidence intervals, test statistic and *p*-values compared to the
standard estimation. Also, note that the coefficient estimate remains
unchanged.

### Robust Covariance Matrix Estimation on Standardized Mixed Model Parameters

Once again, robust estimation can be combined with standardization for
linear mixed-effects models as well and works only with
`standardize = "refit"`.

``` r

# model parameters, cluster robust estimation of standardized mixed model
model_parameters(
  model,
  standardize = "refit",
  vcov = "vcovCR",
  vcov_args = list(type = "CR1", cluster = iris$grp)
)
#> # Fixed Effects
#> 
#> Parameter                          | Coefficient |   SE |         95% CI
#> ------------------------------------------------------------------------
#> (Intercept)                        |        0.97 | 0.08 | [ 0.82,  1.12]
#> Species [versicolor]               |       -1.29 | 0.33 | [-1.95, -0.63]
#> Species [virginica]                |       -1.81 | 0.23 | [-2.26, -1.37]
#> Sepal Width                        |        0.35 | 0.05 | [ 0.24,  0.45]
#> Petal Length                       |        1.74 | 0.10 | [ 1.54,  1.94]
#> Species [versicolor] × Sepal Width |       -0.25 | 0.19 | [-0.62,  0.11]
#> Species [virginica] × Sepal Width  |       -0.19 | 0.06 | [-0.30, -0.08]
#> 
#> Parameter                          | t(141) |      p
#> ----------------------------------------------------
#> (Intercept)                        |  12.55 | < .001
#> Species [versicolor]               |  -3.85 | < .001
#> Species [virginica]                |  -8.01 | < .001
#> Sepal Width                        |   6.64 | < .001
#> Petal Length                       |  17.27 | < .001
#> Species [versicolor] × Sepal Width |  -1.37 | 0.172 
#> Species [virginica] × Sepal Width  |  -3.39 | < .001
#> 
#> # Random Effects
#> 
#> Parameter           | Coefficient |   SE |       95% CI
#> -------------------------------------------------------
#> SD (Intercept: grp) |        0.10 | 0.06 | [0.03, 0.35]
#> SD (Residual)       |        0.36 | 0.02 | [0.32, 0.40]
```

Notice how drastically some of the *p*-values change between
robust-unstandardized model and robust-standardized model.

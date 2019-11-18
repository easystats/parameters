
# parameters <img src='man/figures/logo.png' align="right" height="139" />

[![CRAN](http://www.r-pkg.org/badges/version/parameters)](https://cran.r-project.org/package=parameters)
[![downloads](http://cranlogs.r-pkg.org/badges/parameters)](https://cran.r-project.org/package=parameters)
[![Build
Status](https://travis-ci.org/easystats/parameters.svg?branch=master)](https://travis-ci.org/easystats/parameters)
[![codecov](https://codecov.io/gh/easystats/parameters/branch/master/graph/badge.svg)](https://codecov.io/gh/easystats/parameters)

***Describe and understand your model’s parameters\!***

`parameters`’ primary goal is to provide utilities for processing the
parameters of various statistical models. Beyond computing
***p*-values**, **CIs**, **Bayesian indices** and other measures for a
wide variety of models, this package implements features like
**bootstrapping** of parameters and models, **feature reduction**
(feature extraction and variable selection).

## Installation

Run the following:

``` r
install.packages("devtools")
devtools::install_github("easystats/parameters")
```

``` r
library("parameters")
```

## Documentation

[![Documentation](https://img.shields.io/badge/documentation-parameters-orange.svg?colorB=E91E63)](https://easystats.github.io/parameters/)
[![Blog](https://img.shields.io/badge/blog-easystats-orange.svg?colorB=FF9800)](https://easystats.github.io/blog/posts/)
[![Features](https://img.shields.io/badge/features-parameters-orange.svg?colorB=2196F3)](https://easystats.github.io/parameters/reference/index.html)

Click on the buttons above to access the package
[**documentation**](https://easystats.github.io/parameters/) and the
[**easystats blog**](https://easystats.github.io/blog/posts/), and
check-out these vignettes:

  - [**Parameters
    description**](https://easystats.github.io/parameters/articles/model_parameters.html)
  - [**Bootstrapped
    parameters**](https://easystats.github.io/parameters/articles/bootstrapping.html)
  - [**Parameters
    selection**](https://easystats.github.io/parameters/articles/parameters_selection.html)
  - [**Feature reduction (PCA, cMDS,
    ICA…)**](https://easystats.github.io/parameters/articles/parameters_reduction.html)
  - [**Structural models (EFA, CFA,
    SEM…)**](https://easystats.github.io/parameters/articles/efa_cfa.html)

# Features

## Model’s parameters description

<img src='man/figures/figure1.png' align="center" />

The
[`model_parameters()`](https://easystats.github.io/parameters/articles/model_parameters.html)
function (that can be accessed via the `parameters()` shortcut) allows
you to extract the parameters and their characteristics from various
models in a consistent way. It can be considered as a lightweight
alternative to [`broom::tidy()`](https://github.com/tidymodels/broom),
with some notable differences:

  - The column names of the returned data frame are **specific** to
    their content. For instance, the column containing the statistic is
    named following the statistic name, i.e., *t*, *z*, etc., instead of
    a generic name such as *statistic* (**however**, you can get
    standardized (generic) column names using
    [`standardize_names()`](https://easystats.github.io/parameters/reference/standardize_names.html)).
  - It is able to compute or extract indices not available by default,
    such as ***p*-values**, **CIs**, etc.
  - It includes **feature engineering** capabilities, including
    parameters
    [**bootstrapping**](https://easystats.github.io/parameters/articles/bootstrapping.html).

### Classical Regression Models

``` r
model <- lm(Sepal.Width ~ Petal.Length * Species + Petal.Width, data = iris)

# regular model parameters
model_parameters(model)
# Parameter                           | Coefficient |   SE |         95% CI |     t |  df |      p
# ------------------------------------------------------------------------------------------------
# (Intercept)                         |        2.89 | 0.36 | [ 2.18,  3.60] |  8.01 | 143 | < .001
# Petal.Length                        |        0.26 | 0.25 | [-0.22,  0.75] |  1.07 | 143 | 0.287 
# Species [versicolor]                |       -1.66 | 0.53 | [-2.71, -0.62] | -3.14 | 143 | 0.002 
# Species [virginica]                 |       -1.92 | 0.59 | [-3.08, -0.76] | -3.28 | 143 | 0.001 
# Petal.Width                         |        0.62 | 0.14 | [ 0.34,  0.89] |  4.41 | 143 | < .001
# Petal.Length * Species [versicolor] |       -0.09 | 0.26 | [-0.61,  0.42] | -0.36 | 143 | 0.721 
# Petal.Length * Species [virginica]  |       -0.13 | 0.26 | [-0.64,  0.38] | -0.50 | 143 | 0.618

# standardized parameters
model_parameters(model, standardize = "refit")
# Parameter                           | Coefficient (std.) |   SE |         95% CI |     t |  df |      p
# -------------------------------------------------------------------------------------------------------
# (Intercept)                         |               3.59 | 1.30 | [ 1.01,  6.17] |  8.01 | 143 | 0.007 
# Petal.Length                        |               1.07 | 1.00 | [-0.91,  3.04] |  1.07 | 143 | 0.287 
# Species [versicolor]                |              -4.62 | 1.31 | [-7.21, -2.03] | -3.14 | 143 | < .001
# Species [virginica]                 |              -5.51 | 1.38 | [-8.23, -2.79] | -3.28 | 143 | < .001
# Petal.Width                         |               1.08 | 0.24 | [ 0.59,  1.56] |  4.41 | 143 | < .001
# Petal.Length * Species [versicolor] |              -0.38 | 1.06 | [-2.48,  1.72] | -0.36 | 143 | 0.721 
# Petal.Length * Species [virginica]  |              -0.52 | 1.04 | [-2.58,  1.54] | -0.50 | 143 | 0.618
```

### Mixed Models

``` r
library(lme4)

model <- lmer(Sepal.Width ~ Petal.Length + (1|Species), data = iris)

# model parameters with p-values based on Kenward-Roger approximation
model_parameters(model, p_method = "kenward", ci_method = "kenward")
# Parameter    | Coefficient |   SE |       95% CI |    t |     df |      p
# -------------------------------------------------------------------------
# (Intercept)  |        2.00 | 0.57 | [0.08, 3.92] | 3.56 |   2.67 | 0.046 
# Petal.Length |        0.28 | 0.06 | [0.16, 0.40] | 4.75 | 140.99 | < .001
```

Besides many types of regression models and packages, it also works for
other types of models, such as [**structural
models**](https://easystats.github.io/parameters/articles/efa_cfa.html)
(EFA, CFA, SEM…).

``` r
library(psych)

model <- psych::fa(attitude, nfactors = 3)
model_parameters(model)
# # Rotated loadings from Principal Component Analysis (oblimin-rotation)
# 
# Variable   |   MR1 |   MR2 |   MR3 | Complexity | Uniqueness
# ------------------------------------------------------------
# rating     |  0.90 | -0.07 | -0.05 |       1.02 |       0.23
# complaints |  0.97 | -0.06 |  0.04 |       1.01 |       0.10
# privileges |  0.44 |  0.25 | -0.05 |       1.64 |       0.65
# learning   |  0.47 |  0.54 | -0.28 |       2.51 |       0.24
# raises     |  0.55 |  0.43 |  0.25 |       2.35 |       0.23
# critical   |  0.16 |  0.17 |  0.48 |       1.46 |       0.67
# advance    | -0.11 |  0.91 |  0.07 |       1.04 |       0.22
# 
# The 3 latent factors (oblimin rotation) accounted for 66.60% of the total variance of the original data (MR1 = 38.19%, MR2 = 22.69%, MR3 = 5.72%).
```

## Variable and parameters selection

<img src='man/figures/figure2.png' align="center" />

``` r
library(dplyr)

lm(disp ~ ., data = mtcars) %>% 
  parameters_selection() %>% 
  model_parameters()
# Parameter   | Coefficient |     SE |            95% CI |     t | df |      p
# ----------------------------------------------------------------------------
# (Intercept) |      141.70 | 125.67 | [-116.62, 400.02] |  1.13 | 26 | 0.270 
# cyl         |       13.14 |   7.90 | [  -3.10,  29.38] |  1.66 | 26 | 0.108 
# hp          |        0.63 |   0.20 | [   0.22,   1.03] |  3.18 | 26 | 0.004 
# wt          |       80.45 |  12.22 | [  55.33, 105.57] |  6.58 | 26 | < .001
# qsec        |      -14.68 |   6.14 | [ -27.31,  -2.05] | -2.39 | 26 | 0.024 
# carb        |      -28.75 |   5.60 | [ -40.28, -17.23] | -5.13 | 26 | < .001
```

The
[`parameters_selection()`](https://easystats.github.io/parameters/articles/parameters_selection.html)
can also help you quickly select and retain the most relevant predictors
using methods tailored for the model type. This function also works for
mixed or Bayesian models:

``` r
library(rstanarm)

stan_glm(mpg ~ ., data = mtcars, refresh = 0) %>% 
  parameters_selection() %>% 
  model_parameters()
# Parameter   | Median |         89% CI |     pd | % in ROPE |  Rhat |  ESS |               Prior
# -----------------------------------------------------------------------------------------------
# (Intercept) |  19.90 | [-0.59, 44.44] | 92.62% |     1.23% | 1.000 | 2348 | Normal (0 +- 60.27)
# wt          |  -3.98 | [-5.92, -1.88] | 99.75% |     0.32% | 1.001 | 2159 | Normal (0 +- 15.40)
# cyl         |  -0.48 | [-1.91,  0.76] | 71.43% |    46.02% | 1.000 | 2651 |  Normal (0 +- 8.44)
# hp          |  -0.02 | [-0.04,  0.01] | 89.22% |      100% | 1.000 | 2766 |  Normal (0 +- 0.22)
# am          |   2.93 | [-0.01,  5.77] | 94.90% |     7.42% | 1.000 | 2813 | Normal (0 +- 15.07)
# qsec        |   0.80 | [-0.18,  1.73] | 91.33% |    35.23% | 1.000 | 2273 |  Normal (0 +- 8.43)
# disp        |   0.01 | [-0.01,  0.03] | 87.08% |      100% | 1.002 | 2601 |  Normal (0 +- 0.12)
```

## Miscellaneous

This packages also contains a lot of [**other useful
functions**](https://easystats.github.io/parameters/reference/index.html):

### Describe a Distribution

``` r
x <- rnorm(300)
describe_distribution(x)
```

``` r
knitr::kable(describe_distribution(rnorm(300)), digits = 1)
```

|  Mean | SD | Min | Max | Skewness | Kurtosis |   n | n\_Missing |
| ----: | -: | --: | --: | -------: | -------: | --: | ---------: |
| \-0.1 |  1 | \-3 |   3 |        0 |    \-0.3 | 300 |          0 |

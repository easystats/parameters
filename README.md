
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
**standardization** or **bootstrapping** of parameters and models,
**feature reduction** (feature extraction and variable selection) as
well as conversion between indices of **effect size**.

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

<!-- end list -->

``` r
library(lme4)

model <- lmer(Sepal.Width ~ Petal.Length + (1|Species), data = iris)
model_parameters(model)
# Parameter    | Coefficient |   SE |       95% CI |    t |      p
# ----------------------------------------------------------------
# (Intercept)  |        2.00 | 0.56 | [0.90, 3.10] | 3.56 | < .001
# Petal.Length |        0.28 | 0.06 | [0.17, 0.40] | 4.75 | < .001
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
# Parameter   | Coefficient |     SE |             95% CI |     t | df |      p
# -----------------------------------------------------------------------------
# (Intercept) |      141.70 | 125.67 | [-116.62,  400.02] |  1.13 | 26 | > .1  
# cyl         |       13.14 |   7.90 | [  -3.10,   29.38] |  1.66 | 26 | > .1  
# hp          |        0.63 |   0.20 | [   0.22,    1.03] |  3.18 | 26 | < .01 
# wt          |       80.45 |  12.22 | [  55.33,  105.57] |  6.58 | 26 | < .001
# qsec        |      -14.68 |   6.14 | [ -27.31,   -2.05] | -2.39 | 26 | < .05 
# carb        |      -28.75 |   5.60 | [ -40.28,  -17.23] | -5.13 | 26 | < .001
```

The
[`parameters_selection()`](https://easystats.github.io/parameters/articles/parameters_selection.html)
can also help you quickly select and retain the most relevant predictors
using methods tailored for the model type. This function also works for
mixed or Bayesian models:

``` r
library(rstanarm)

model <- stan_glm(mpg ~ ., data = mtcars) %>% 
  parameters_selection() %>% 
  model_parameters()
```

    # Parameter   | Median |         89% CI |     pd | % in ROPE |  Rhat |  ESS |               Prior
    # -----------------------------------------------------------------------------------------------
    # (Intercept) |  19.31 | [-5.95, 41.92] | 90.55% |     1.15% | 1.001 | 1083 | Normal (0 +- 60.27)
    # wt          |  -3.97 | [-6.00, -1.91] | 99.70% |     0.90% | 1.000 | 1222 | Normal (0 +- 15.40)
    # cyl         |  -0.44 | [-1.66,  0.98] | 70.90% |    46.95% | 0.999 | 1250 |  Normal (0 +- 8.44)
    # hp          |  -0.02 | [-0.04,  0.00] | 89.70% |      100% | 1.003 | 1191 |  Normal (0 +- 0.22)
    # am          |   3.00 | [ 0.08,  6.08] | 94.70% |     6.55% | 0.999 | 1480 | Normal (0 +- 15.07)
    # qsec        |   0.83 | [-0.12,  1.91] | 90.80% |    33.35% | 1.002 | 1049 |  Normal (0 +- 8.43)
    # disp        |   0.01 | [-0.01,  0.03] | 86.10% |      100% | 0.999 | 1300 |  Normal (0 +- 0.12)

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

| Mean | SD | Min | Max | Skewness | Kurtosis |   n | n\_Missing |
| ---: | -: | --: | --: | -------: | -------: | --: | ---------: |
|    0 |  1 | \-2 |   4 |      0.1 |      0.1 | 300 |          0 |

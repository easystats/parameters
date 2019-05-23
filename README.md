
# parameters <img src='man/figures/logo.png' align="right" height="139" />

[![Build
Status](https://travis-ci.org/easystats/parameters.svg?branch=master)](https://travis-ci.org/easystats/parameters)
[![codecov](https://codecov.io/gh/easystats/parameters/branch/master/graph/badge.svg)](https://codecov.io/gh/easystats/parameters)
[![HitCount](http://hits.dwyl.io/easystats/parameters.svg)](http://hits.dwyl.io/easystats/parameters)
[![Documentation](https://img.shields.io/badge/documentation-parameters-orange.svg?colorB=E91E63)](https://easystats.github.io/parameters/)

***Describe and understand your model’s parameters\!***

`parameters`’s primary goal is to provide utilities for processing the
parameters of various statistical models. Beyond computing p-values,
CIs, and other indices for a wide variety of models, this package
implements features like standardization, normalization or bootstrapping
of parameters and models or conversion between indices of effect size.

## Installation

Run the following:

``` r
install.packages("devtools")
devtools::install_github("easystats/parameters")
```

``` r
library("parameters")
```

## Examples

### Describe models’ parameters

#### General Linear Models

``` r
model <- lm(mpg ~ wt + cyl, data = mtcars)
model_parameters(model, standardize = TRUE)
```

| Parameter   |   beta |   SE | CI\_low | CI\_high |      t | DoF\_residual | p | Std\_beta |
| :---------- | -----: | ---: | ------: | -------: | -----: | ------------: | -: | --------: |
| (Intercept) |  39.69 | 1.71 |   36.18 |    43.19 |  23.14 |            29 | 0 |      0.00 |
| wt          | \-3.19 | 0.76 |  \-4.74 |   \-1.64 | \-4.22 |            29 | 0 |    \-0.52 |
| cyl         | \-1.51 | 0.41 |  \-2.36 |   \-0.66 | \-3.64 |            29 | 0 |    \-0.45 |

#### Bootstrapped Models

``` r
model <- lm(mpg ~ wt + cyl, data = mtcars)
model_parameters(model, bootstrap = TRUE)
```

| Parameter   | Median | CI\_low | CI\_high |  pd | ROPE\_Percentage |
| :---------- | -----: | ------: | -------: | --: | ---------------: |
| (Intercept) |  39.87 |   35.40 |    44.04 | 100 |                0 |
| cyl         | \-1.46 |  \-2.28 |   \-0.83 | 100 |                0 |
| wt          | \-3.26 |  \-4.72 |   \-1.92 | 100 |                0 |

#### Bayesian Models

``` r
library(rstanarm)
model <- stan_glm(mpg ~ wt + cyl, data = mtcars)
model_parameters(model)
```

| Parameter   | Median | CI\_low | CI\_high |     pd | ROPE\_Percentage |  ESS | Rhat | Prior\_Distribution | Prior\_Location | Prior\_Scale |
| :---------- | -----: | ------: | -------: | -----: | ---------------: | ---: | ---: | :------------------ | --------------: | -----------: |
| (Intercept) |  39.63 |   36.77 |    42.46 | 100.00 |             0.00 | 4862 |    1 | normal              |               0 |        60.27 |
| cyl         | \-1.50 |  \-2.20 |   \-0.78 |  99.90 |             2.05 | 1788 |    1 | normal              |               0 |         8.44 |
| wt          | \-3.20 |  \-4.58 |   \-2.00 |  99.98 |             0.08 | 1666 |    1 | normal              |               0 |        15.40 |


# parameters <img src='man/figures/logo.png' align="right" height="139" />

[![CRAN](http://www.r-pkg.org/badges/version/parameters)](https://cran.r-project.org/package=parameters)
[![downloads](http://cranlogs.r-pkg.org/badges/parameters)](https://cran.r-project.org/package=parameters)
[![Build
Status](https://travis-ci.org/easystats/parameters.svg?branch=master)](https://travis-ci.org/easystats/parameters)
[![codecov](https://codecov.io/gh/easystats/parameters/branch/master/graph/badge.svg)](https://codecov.io/gh/easystats/parameters)

***Describe and understand your model’s parameters\!***

`parameters`’s primary goal is to provide utilities for processing the
parameters of various statistical models. Beyond computing *p*-values,
**CIs**, and other indices for a wide variety of models, this package
implements features like **standardization** or **bootstrapping** of
parameters and models, **feature reduction** (feature extraction and
variable selection) as well as conversion between indices of **effect
size**.

## Installation

Run the following:

``` r
install.packages("devtools")
devtools::install_github("easystats/parameters")
```

``` r
library("parameters")
```

# Features

[![Documentation](https://img.shields.io/badge/documentation-parameters-orange.svg?colorB=E91E63)](https://easystats.github.io/parameters/)
[![Blog](https://img.shields.io/badge/blog-easystats-orange.svg?colorB=FF9800)](https://easystats.github.io/blog/posts/)
[![Features](https://img.shields.io/badge/features-parameters-orange.svg?colorB=2196F3)](https://easystats.github.io/parameters/reference/index.html)

Click on the buttons above to access the package
[**documentation**](https://easystats.github.io/parameters/) and the
[**easystats blog**](https://easystats.github.io/blog/posts/), and
check-out these vignettes:

  - [Parameters’
    standardization](https://easystats.github.io/parameters/articles/standardization.html)
  - [Model
    bootstrapping](https://easystats.github.io/parameters/articles/bootstrapping.html)

## Model’s parameters description

The `model_parameters` function allows you to extract the parameters and
their characteristics from various models in a consistent way. It could
be considered as an alternative to
[broom::tidy()](https://github.com/tidymodels/broom), with some notable
differences:

  - The names of the returned dataframe are **specific** to their
    content. For instance, the column containing the statistic is named
    following the statistic name, *i.e.*, `t`, `z`, etc.
  - It is able to compute or extract indices not available by default,
    such as ***p* values**, **CIs**, etc.
  - It includes **feature engineering** capabilities, including
    [**bootstrapping**](https://easystats.github.io/parameters/articles/bootstrapping.html)
    and
    [**standardization**](https://easystats.github.io/parameters/articles/standardization.html)
    of parameters.

### Correlations

#### Frequentist

``` r
model <- cor.test(iris$Sepal.Length, iris$Sepal.Width)
model_parameters(model)
```

| Parameter1        | Parameter2       |     r |   t | DoF |   p | CI\_low | CI\_high | CI\_level | Method  |
| :---------------- | :--------------- | ----: | --: | --: | --: | ------: | -------: | --------: | :------ |
| iris$Sepal.Length | iris$Sepal.Width | \-0.1 | \-1 | 148 | 0.2 |   \-0.3 |        0 |         1 | Pearson |

#### Bayesian

``` r
library(BayesFactor)

model <- BayesFactor::correlationBF(iris$Sepal.Length, iris$Sepal.Width)
model_parameters(model)
```

| Parameter | Median | CI\_low | CI\_high | pd | ROPE\_Percentage | Prior\_Distribution | Prior\_Location | Prior\_Scale |  BF |
| :-------- | -----: | ------: | -------: | -: | ---------------: | :------------------ | --------------: | -----------: | --: |
| rho       |  \-0.1 |   \-0.2 |        0 | 92 |               43 | cauchy              |               0 |          0.3 | 0.5 |

### t-tests

#### Frequentist

``` r
df <- iris
df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")

model <- t.test(Sepal.Length ~ Sepal.Big, data=df)
model_parameters(model)
```

| Parameter    | Group     | Mean\_Group1 | Mean\_Group2 | Difference | t | DoF |   p | CI\_low | CI\_high | CI\_level | Method                  |
| :----------- | :-------- | -----------: | -----------: | ---------: | -: | --: | --: | ------: | -------: | --------: | :---------------------- |
| Sepal.Length | Sepal.Big |            6 |            6 |      \-0.2 | 1 | 142 | 0.2 |   \-0.1 |      0.4 |         1 | Welch Two Sample t-test |

#### Bayesian

``` r
model <- BayesFactor::ttestBF(formula = Sepal.Length ~ Sepal.Big, data=df)
model_parameters(model)
```

| Parameter  | Median | CI\_low | CI\_high |  pd | ROPE\_Percentage | Prior\_Distribution | Prior\_Location | Prior\_Scale |  BF |
| :--------- | -----: | ------: | -------: | --: | ---------------: | :------------------ | --------------: | -----------: | --: |
| Difference |      6 |       6 |        6 | 100 |                0 | cauchy              |               0 |          0.7 | 0.4 |

### ANOVAs

#### Simple

``` r
model <- aov(Sepal.Length ~ Sepal.Big, data = df)
model_parameters(model, omega_squared = TRUE)
```

| Parameter | Sum\_Squares | DoF | Mean\_Square | F |   p | Omega\_Sq |
| :-------- | -----------: | --: | -----------: | -: | --: | --------: |
| Sepal.Big |            1 |   1 |          1.1 | 2 | 0.2 |         0 |
| Residuals |          101 | 148 |          0.7 |   |     |           |

#### Repeated measures

``` r
model <- aov(Sepal.Length ~ Sepal.Big + Error(Species), data = df)
model_parameters(model)
```

| Group   | Parameter | Sum\_Squares | DoF | Mean\_Square |    F |   p |
| :------ | :-------- | -----------: | --: | -----------: | ---: | --: |
| Species | Sepal.Big |           28 |   1 |         28.3 |  0.8 | 0.5 |
| Species | Residuals |           35 |   1 |         34.9 |      |     |
| Within  | Sepal.Big |            5 |   1 |          4.7 | 20.2 | 0.0 |
| Within  | Residuals |           34 | 146 |          0.2 |      |     |

``` r
library(lme4)
model <- anova(lmer(Sepal.Length ~ Sepal.Big + (1 | Species), data = df))
model_parameters(model)
```

| Parameter | Sum\_Squares | DoF | Mean\_Square |  F |
| :-------- | -----------: | --: | -----------: | -: |
| Sepal.Big |            5 |   1 |            5 | 20 |

### General Linear Models (GLM)

``` r
model <- glm(vs ~ wt + cyl, data = mtcars, family = "binomial")
model_parameters(model, standardize = "refit")
```

| Parameter   | Coefficient | SE | CI\_low | CI\_high |   z | DoF\_residual |   p | Std\_Coefficient |
| :---------- | ----------: | -: | ------: | -------: | --: | ------------: | --: | ---------------: |
| (Intercept) |          11 |  4 |     4.8 |       23 |   2 |            29 | 0.0 |            \-0.8 |
| wt          |           2 |  2 |   \-0.5 |        6 |   1 |            29 | 0.2 |              2.1 |
| cyl         |         \-3 |  1 |   \-6.9 |      \-1 | \-2 |            29 | 0.0 |            \-5.2 |

### Bootstrapped models

``` r
model <- lm(mpg ~ drat * cyl, data = mtcars)
model_parameters(model, bootstrap = TRUE)
```

|   | Parameter   | Coefficient | CI\_low | CI\_high |   p |
| - | :---------- | ----------: | ------: | -------: | --: |
| 1 | (Intercept) |         0.9 |  \-43.7 |     37.5 | 0.9 |
| 3 | drat        |         9.3 |   \-0.5 |     20.9 | 0.1 |
| 2 | cyl         |         2.0 |   \-3.4 |      8.3 | 0.4 |
| 4 | drat:cyl    |       \-1.2 |   \-3.0 |      0.2 | 0.1 |

### Mixed models

``` r
library(lme4)

model <- lmer(Sepal.Width ~ Petal.Length + (1|Species), data = iris)
model_parameters(model, standardize = "refit")
```

| Parameter    | Coefficient |  SE | CI\_low | CI\_high | t | p | Std\_Coefficient |
| :----------- | ----------: | --: | ------: | -------: | -: | -: | ---------------: |
| (Intercept)  |         2.0 | 0.6 |   \-1.9 |      5.9 | 4 | 0 |                0 |
| Petal.Length |         0.3 | 0.1 |   \-0.3 |      0.8 | 5 | 0 |                1 |

### Bayesian models

``` r
library(rstanarm)

model <- stan_glm(mpg ~ wt + cyl, data = mtcars)
model_parameters(model)
```

|   | Parameter   | Median | CI\_low | CI\_high |  pd | ROPE\_Percentage |  ESS | Rhat | Prior\_Distribution | Prior\_Location | Prior\_Scale |
| - | :---------- | -----: | ------: | -------: | --: | ---------------: | ---: | ---: | :------------------ | --------------: | -----------: |
| 1 | (Intercept) |     40 |      37 |     42.5 | 100 |                0 | 5250 |    1 | normal              |               0 |           60 |
| 3 | wt          |    \-3 |     \-4 |    \-1.9 | 100 |                0 | 1963 |    1 | normal              |               0 |           15 |
| 2 | cyl         |    \-2 |     \-2 |    \-0.8 | 100 |                2 | 1997 |    1 | normal              |               0 |            8 |

## Variable and parameters selection

### General Linear Models (GLM)

``` r
library(dplyr)

lm(disp ~ ., data = mtcars) %>% 
  parameters_selection() %>% 
  model_parameters()
```

| Parameter   | Coefficient |    SE | CI\_low | CI\_high |   t | DoF\_residual |   p | Std\_Coefficient |
| :---------- | ----------: | ----: | ------: | -------: | --: | ------------: | --: | ---------------: |
| (Intercept) |       141.7 | 125.7 | \-116.6 |      400 |   1 |            26 | 0.3 |              0.0 |
| cyl         |        13.1 |   7.9 |   \-3.1 |       29 |   2 |            26 | 0.1 |              0.2 |
| hp          |         0.6 |   0.2 |     0.2 |        1 |   3 |            26 | 0.0 |              0.3 |
| wt          |        80.5 |  12.2 |    55.3 |      106 |   7 |            26 | 0.0 |              0.6 |
| qsec        |      \-14.7 |   6.1 |  \-27.3 |      \-2 | \-2 |            26 | 0.0 |            \-0.2 |
| carb        |      \-28.8 |   5.6 |  \-40.3 |     \-17 | \-5 |            26 | 0.0 |            \-0.4 |

### Mixed models

``` r
library(lme4)

lmer(Sepal.Length ~ Sepal.Width * Petal.Length * Petal.Width + (1|Species), data = iris)  %>%
  parameters_selection() %>%
  model_parameters()
```

| Parameter                            | Coefficient |  SE | CI\_low | CI\_high |     t |   p | Std\_Coefficient |
| :----------------------------------- | ----------: | --: | ------: | -------: | ----: | --: | ---------------: |
| (Intercept)                          |         1.8 | 0.9 |   \-1.7 |      5.3 |   1.9 | 0.1 |            \-0.2 |
| Petal.Length                         |         0.9 | 0.5 |   \-0.8 |      2.6 |   1.8 | 0.1 |              0.3 |
| Petal.Length:Petal.Width             |         0.3 | 0.2 |   \-0.3 |      0.8 |   1.2 | 0.2 |              1.6 |
| Petal.Width                          |       \-2.0 | 1.5 |     2.0 |    \-6.0 | \-1.3 | 0.2 |            \-0.4 |
| Sepal.Width                          |         0.8 | 0.3 |   \-0.7 |      2.2 |   2.8 | 0.0 |            \-0.1 |
| Sepal.Width:Petal.Length             |       \-0.1 | 0.2 |     0.1 |    \-0.3 | \-0.6 | 0.5 |              0.1 |
| Sepal.Width:Petal.Length:Petal.Width |         0.0 | 0.1 |     0.0 |    \-0.1 | \-0.6 | 0.5 |              0.2 |
| Sepal.Width:Petal.Width              |         0.3 | 0.5 |   \-0.3 |      1.0 |   0.7 | 0.5 |              0.0 |

### Bayesian models

``` r
library(rstanarm)

model <- stan_glm(mpg ~ ., data = mtcars) %>% 
  parameters_selection() %>% 
  model_parameters()
```

|   | Parameter   | Median | CI\_low | CI\_high |  pd | ROPE\_Percentage |  ESS | Rhat | Prior\_Distribution | Prior\_Location | Prior\_Scale |
| - | :---------- | -----: | ------: | -------: | --: | ---------------: | ---: | ---: | :------------------ | --------------: | -----------: |
| 1 | (Intercept) |   20.2 |   \-1.9 |     42.1 |  92 |              1.1 | 2483 |    1 | normal              |               0 |         60.3 |
| 7 | wt          |  \-3.9 |   \-5.9 |    \-1.9 | 100 |              0.3 | 2860 |    1 | normal              |               0 |         15.4 |
| 3 | cyl         |  \-0.5 |   \-1.8 |      0.8 |  72 |             48.2 | 2781 |    1 | normal              |               0 |          8.4 |
| 5 | hp          |    0.0 |     0.0 |      0.0 |  90 |            100.0 | 2907 |    1 | normal              |               0 |          0.2 |
| 2 | am          |    2.9 |     0.2 |      5.9 |  95 |              7.0 | 3082 |    1 | normal              |               0 |         15.1 |
| 6 | qsec        |    0.8 |   \-0.2 |      1.7 |  91 |             36.2 | 2449 |    1 | normal              |               0 |          8.4 |
| 4 | disp        |    0.0 |     0.0 |      0.0 |  87 |            100.0 | 2750 |    1 | normal              |               0 |          0.1 |

## Variable and features extraction

### How many factors to retain in Factor Analysis (FA)

``` r
n_factors(attitude)
> # Method Agreement Procedure:
> 
> The choice of 1 dimensions is supported by 4 (40.00%) methods out of 10 (EGA (glasso), EAG (TMFG), VSS complexity 1, Velicer's MAP).
```

## Miscellaneous

### Describe a Distribution

``` r
x <- rnorm(300)
describe_distribution(x)
>    Mean SD Min Max Skewness Kurtosis n_Obs n_Missing
> 1 -0.03  1  -4   3    -0.08      0.1   300         0
```

### Standardization and normalization

``` r
df <- standardize(iris)
summary(df$Sepal.Length)
>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
>    -1.9    -0.9    -0.1     0.0     0.7     2.5

df <- normalize(iris)
summary(df$Sepal.Length)
>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
>     0.0     0.2     0.4     0.4     0.6     1.0
```

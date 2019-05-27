
# parameters <img src='man/figures/logo.png' align="right" height="139" />

[![CRAN](http://www.r-pkg.org/badges/version/parameters)](https://cran.r-project.org/package=parameters)
[![downloads](http://cranlogs.r-pkg.org/badges/parameters)](https://cran.r-project.org/package=parameters)
[![Build
Status](https://travis-ci.org/easystats/parameters.svg?branch=master)](https://travis-ci.org/easystats/parameters)
[![codecov](https://codecov.io/gh/easystats/parameters/branch/master/graph/badge.svg)](https://codecov.io/gh/easystats/parameters)

***Describe and understand your model’s parameters\!***

`parameters`’s primary goal is to provide utilities for processing the
parameters of various statistical models. Beyond computing *p*-values,
CIs, and other indices for a wide variety of models, this package
implements features like standardization, normalization or bootstrapping
of parameters and models, as well as conversion between indices of
effect size.

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

  - [Parameters’
    standardization](https://easystats.github.io/parameters/articles/standardization.html)
  - [Model
    bootstrapping](https://easystats.github.io/parameters/articles/bootstrapping.html)

# Features

## Model’s parameters description

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

``` r
model <- aov(Sepal.Length ~ Sepal.Big, data = df)
model_parameters(model)
```

| Parameter | Sum\_Squares | DoF | Mean\_Square | F |   p | Omega\_Squared\_partial |
| :-------- | -----------: | --: | -----------: | -: | --: | ----------------------: |
| Sepal.Big |            1 |   1 |          1.1 | 2 | 0.2 |                       0 |
| Residuals |          101 | 148 |          0.7 |   |     |                         |

``` r
model <- anova(lm(Sepal.Length ~ Sepal.Big, data = df))
model_parameters(model)
```

| Parameter | Sum\_Squares | DoF | Mean\_Square | F |   p | Omega\_Squared\_partial |
| :-------- | -----------: | --: | -----------: | -: | --: | ----------------------: |
| Sepal.Big |            1 |   1 |          1.1 | 2 | 0.2 |                       0 |
| Residuals |          101 | 148 |          0.7 |   |     |                         |

``` r
model <- aov(Sepal.Length ~ Sepal.Big + Error(Species), data = df)
model_parameters(model)
```

| Group   | Parameter | Sum\_Squares | DoF | Mean\_Square |    F |   p | Omega\_Squared\_partial |
| :------ | :-------- | -----------: | --: | -----------: | ---: | --: | ----------------------: |
| Species | Sepal.Big |           28 |   1 |         28.3 |  0.8 | 0.5 |                       0 |
| Species | Residuals |           35 |   1 |         34.9 |      |     |                         |
| Within  | Sepal.Big |            5 |   1 |          4.7 | 20.2 | 0.0 |                       0 |
| Within  | Residuals |           34 | 146 |          0.2 |      |     |                         |

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

| Parameter   | beta | SE | CI\_low | CI\_high |   z | DoF\_residual |   p | Std\_beta |
| :---------- | ---: | -: | ------: | -------: | --: | ------------: | --: | --------: |
| (Intercept) |   11 |  4 |     4.8 |       23 |   2 |            29 | 0.0 |     \-0.8 |
| wt          |    2 |  2 |   \-0.5 |        6 |   1 |            29 | 0.2 |       2.1 |
| cyl         |  \-3 |  1 |   \-6.9 |      \-1 | \-2 |            29 | 0.0 |     \-5.2 |

<!-- ### Mixed models -->

<!-- ```{r, warning=FALSE, message=FALSE, eval=FALSE} -->

<!-- library(lme4) -->

<!-- model <- lmer(Sepal.Width ~ Petal.Length + (1|Species), data = iris) -->

<!-- model_parameters(model, standardize = "refit") -->

<!-- ``` -->

<!-- ```{r, warning=FALSE, message=FALSE, echo=FALSE} -->

<!-- library(lme4) -->

<!-- model <- lmer(Sepal.Width ~ Petal.Length + (1|Species), data = iris) -->

<!-- knitr::kable(model_parameters(model, standardize = "refit"), digits=1) -->

<!-- ``` -->

### Bootstrapped models

``` r
model <- lm(mpg ~ wt + cyl, data = mtcars)
model_parameters(model, bootstrap = TRUE)
```

| Parameter   | Median | CI\_low | CI\_high |  pd | ROPE\_Percentage |
| :---------- | -----: | ------: | -------: | --: | ---------------: |
| (Intercept) |     40 |      35 |     43.5 | 100 |                0 |
| cyl         |    \-2 |     \-2 |    \-0.7 | 100 |                0 |
| wt          |    \-3 |     \-5 |    \-1.8 | 100 |                0 |

### Bayesian models

``` r
library(rstanarm)

model <- stan_glm(mpg ~ wt + cyl, data = mtcars)
model_parameters(model)
```

| Parameter   | Median | CI\_low | CI\_high |  pd | ROPE\_Percentage |  ESS | Rhat | Prior\_Distribution | Prior\_Location | Prior\_Scale |
| :---------- | -----: | ------: | -------: | --: | ---------------: | ---: | ---: | :------------------ | --------------: | -----------: |
| (Intercept) |     40 |      37 |     42.9 | 100 |                0 | 5250 |    1 | normal              |               0 |           60 |
| cyl         |    \-2 |     \-2 |    \-0.8 | 100 |                2 | 1997 |    1 | normal              |               0 |            8 |
| wt          |    \-3 |     \-4 |    \-1.8 | 100 |                0 | 1963 |    1 | normal              |               0 |           15 |

## Miscellaenous

### Describe a Distribution

    ##     Type Type_Confidence Median MAD Min Max Skewness
    ## 1 normal              60  -0.06   1  -3   3     0.03
    ##   Kurtosis n_Obs n_Missing
    ## 1        3   300         0

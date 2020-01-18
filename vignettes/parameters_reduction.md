Feature Reduction (PCA, cMDS, ICA…)
================

  - [Quick and Exploratory Method](#quick-and-exploratory-method)
  - [Principal Component Analysis
    (PCA)](#principal-component-analysis-pca)
      - [Using the `psych` package for
        PCA](#using-the-psych-package-for-pca)
  - [References](#references)

Also known as [**feature extraction** or **dimension
reduction**](https://en.wikipedia.org/wiki/Feature_extraction) in
machine learning, the goal of variable reduction is to **reduce the
number of predictors** by derivating, from a set of measured data, new
variables intended to be informative and non-redundant. This method can
be used to **simplify models**, which can benefit model interpretation,
shorten fitting time, and improve generalization (by reducing
overfitting).

# Quick and Exploratory Method

Let’s start by fitting a multiple regression with the `attitude`
dataset, available is base R, to predict the overall **rating** by
employees of their organization with the remaining variables (handling
of employee **complaints**, special **privileges**, opportunity of
**learning**, **raises**, a feedback considered too **critical** and
opportunity of **advancement**).

``` r
library(dplyr)
library(parameters)

model <- lm(rating ~ ., data = attitude)
parameters(model)
```

    > Parameter   | Coefficient |    SE |          95% CI |     t | df |      p
    > -------------------------------------------------------------------------
    > (Intercept) |       10.79 | 11.59 | [-13.19, 34.76] |  0.93 | 23 | 0.362 
    > complaints  |        0.61 |  0.16 | [  0.28,  0.95] |  3.81 | 23 | < .001
    > privileges  |       -0.07 |  0.14 | [ -0.35,  0.21] | -0.54 | 23 | 0.596 
    > learning    |        0.32 |  0.17 | [ -0.03,  0.67] |  1.90 | 23 | 0.070 
    > raises      |        0.08 |  0.22 | [ -0.38,  0.54] |  0.37 | 23 | 0.715 
    > critical    |        0.04 |  0.15 | [ -0.27,  0.34] |  0.26 | 23 | 0.796 
    > advance     |       -0.22 |  0.18 | [ -0.59,  0.15] | -1.22 | 23 | 0.236

We can explore a reduction of the number of parameters with the
`reduce_parameters()` function.

``` r
newmodel <- reduce_parameters(model)
parameters(newmodel)
```

    > Parameter                                                              | Coefficient |   SE |         95% CI |     t | df |      p
    > ----------------------------------------------------------------------------------------------------------------------------------
    > (Intercept)                                                            |       64.63 | 1.57 | [61.41, 67.85] | 41.19 | 27 | < .001
    > raises_0.88/learning_0.82/complaints_0.78/privileges_0.70/advance_0.68 |        4.62 | 0.90 | [ 2.78,  6.46] |  5.16 | 27 | < .001
    > critical_0.80                                                          |       -3.41 | 1.59 | [-6.67, -0.14] | -2.14 | 27 | 0.041

This quickly *hints* toward the fact that the model could be represented
via **two “latent” dimensions**, one correlated with all the positive
things that a company has to offer, and the other one related to the
amount of negative critiques received by the employees. These two
dimensions have a positive and negative relationship with the company
rating, respectively.

> What does `reduce_parameters()` exactly do?

This function performs a reduction in the parameters space (the number
of variables). It starts by creating a new set of variables, based on a
given method (the default method is “**PCA**”, but other are available
via the `method` argument, such as “**cMDS**”, “**DRR**” or “**ICA**”).
Then, it names this new dimensions using the original variables that
*correlate* the most with it. For instance, a variable named
‘V1\_0.97/V4\_-0.88’ means that the V1 and the V4 variables correlate
maximally (with respective coefficients of .97 and -.88) with this
dimension.

``` r
reduce_parameters(model, method = "cMDS") %>% 
  parameters()
```

    > Parameter                                                 | Coefficient |   SE |         95% CI |     t | df |      p
    > ---------------------------------------------------------------------------------------------------------------------
    > (Intercept)                                               |       64.63 | 1.41 | [61.73, 67.53] | 45.80 | 26 | < .001
    > raises_0.85/complaints_0.84/learning_0.83/privileges_0.74 |        0.43 | 0.07 | [ 0.28,  0.57] |  6.14 | 26 | < .001
    > advance_-0.60                                             |        0.32 | 0.13 | [ 0.04,  0.59] |  2.36 | 26 | 0.026 
    > critical_-0.65                                            |       -0.24 | 0.15 | [-0.56,  0.07] | -1.61 | 26 | 0.120

A different method (**Classical Multidimensional Scaling - cMDS**)
suggests that negative critiques do not have a significant impact on the
rating, and that the lack of opportunities of career advancement is a
separate dimension with an importance on its own.

Although this function can be useful in exploratory data analysis, it’s
best to perform the dimension reduction step in a **separate and
dedicated stage**, as this is a very important process in the data
analysis workflow.

# Principal Component Analysis (PCA)

PCA is a widely used procedure that lies in-between dimension reduction
and structural modelling. Indeed, one of the way of reducing the number
of predictors is to extract a new set of uncorrelated variables that
will *represent* variance of your initial dataset. But how the original
variables relate between themselves can also be a question on its own.

We can apply the `principal_components()` function to do the the
predictors of the model:

``` r
pca <- principal_components(insight::get_predictors(model), n = "auto")
pca
```

    > # Loadings from Principal Component Analysis (no rotation)
    > 
    > Variable   |  PC1 | Complexity
    > ------------------------------
    > complaints | 0.78 |       1.00
    > privileges | 0.70 |       1.00
    > learning   | 0.82 |       1.00
    > raises     | 0.88 |       1.00
    > critical   | 0.40 |       1.00
    > advance    | 0.68 |       1.00
    > 
    > The unique principal component accounted for 52.82% of the total variance of the original data.

The `principal_component()` function automatically selected one
component (if the number of components is not specified, this function
uses
[`n_factors()`](https://easystats.github.io/parameters/articles/n_factors.html)
to estimate the optimal number to keep) and returned the **loadings**,
i.e., the relationship with all of the original variables.

As we can see here, it seems that our new component captured the essence
(more than half of the total variance present in the original dataset)
of all our other variables together. We can **extract** the values of
this component for each of our observation using the `predict()` method
and add in the response variable of our initial dataset.

``` r
newdata <- predict(pca)
newdata$rating <- attitude$rating
```

We can know update the model with this new component:

``` r
update(model, rating ~ PC1, data = newdata) %>% 
  parameters()
```

    > Parameter   | Coefficient |   SE |         95% CI |     t | df |      p
    > -----------------------------------------------------------------------
    > (Intercept) |       64.63 | 1.67 | [61.22, 68.05] | 38.78 | 28 | < .001
    > PC1         |        4.62 | 0.95 | [ 2.67,  6.57] |  4.86 | 28 | < .001

## Using the `psych` package for PCA

You can also use different packages for models, such as
[`psych`](https://cran.r-project.org/package=psych) (Revelle 2018) or
[`FactoMineR`](http://factominer.free.fr/) for PCA or Exploratory Factor
Analysis (EFA), as it allows for more flexibility, control and details
when running such procedures. Thus, the functions from this package are
**fully supported** by `parameters` through the `model_parameters()`
function.

As such, the above analysis can be fully reproduced as follows:

``` r
library(psych)

# Fit the PCA
pca <- psych::principal(attitude, nfactors = 1) %>% 
  model_parameters()
pca
```

    > # Rotated loadings from Principal Component Analysis (varimax-rotation)
    > 
    > Variable   |  PC1 | Complexity | Uniqueness
    > -------------------------------------------
    > rating     | 0.80 |       1.00 |       0.37
    > complaints | 0.85 |       1.00 |       0.28
    > privileges | 0.68 |       1.00 |       0.53
    > learning   | 0.83 |       1.00 |       0.32
    > raises     | 0.86 |       1.00 |       0.26
    > critical   | 0.36 |       1.00 |       0.87
    > advance    | 0.58 |       1.00 |       0.66
    > 
    > The unique principal component (varimax rotation) accounted for 53.09% of the total variance of the original data.

*Note:* By default, `psych::principal()` uses a **varimax** rotation to
extract rotated components, possibly leading to discrepancies in the
results.

Finally, refit the model:

``` r
df <- cbind(attitude, predict(pca))

update(model, rating ~ PC1, data = df) %>% 
  model_parameters()
```

    > Parameter   | Coefficient |   SE |         95% CI |     t | df |      p
    > -----------------------------------------------------------------------
    > (Intercept) |       64.63 | 1.37 | [61.83, 67.44] | 47.23 | 28 | < .001
    > PC1         |        9.69 | 1.39 | [ 6.84, 12.54] |  6.96 | 28 | < .001

# References

<div id="refs" class="references">

<div id="ref-revelle2018">

Revelle, William. 2018. *Psych: Procedures for Psychological,
Psychometric, and Personality Research*. Evanston, Illinois:
Northwestern University. <https://CRAN.R-project.org/package=psych>.

</div>

</div>

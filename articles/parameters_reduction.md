# Feature Reduction (PCA, cMDS, ICA, ...)

Also known as [**feature extraction** or **dimension
reduction**](https://en.wikipedia.org/wiki/Feature_extraction) in
machine learning, the goal of variable reduction is to **reduce the
number of predictors** by deriving a new set of variables intended to be
informative and non-redundant from a set of measured data. This method
can be used to **simplify models**, which can benefit model
interpretation, shorten fitting time, and improve generalization (by
reducing overfitting).

### Quick and Exploratory Method

Let’s start by fitting a multiple linear regression model with the
`attitude` dataset, available is base R, to predict the overall
**rating** by employees of their organization with the remaining
variables (handling of employee **complaints**, special **privileges**,
opportunity of **learning**, **raises**, a feedback considered too
**critical** and opportunity of **advancement**).

``` r

data("attitude")
model <- lm(rating ~ ., data = attitude)
parameters(model)
#> Parameter   | Coefficient |    SE |          95% CI | t(23) |      p
#> --------------------------------------------------------------------
#> (Intercept) |       10.79 | 11.59 | [-13.19, 34.76] |  0.93 | 0.362 
#> complaints  |        0.61 |  0.16 | [  0.28,  0.95] |  3.81 | < .001
#> privileges  |       -0.07 |  0.14 | [ -0.35,  0.21] | -0.54 | 0.596 
#> learning    |        0.32 |  0.17 | [ -0.03,  0.67] |  1.90 | 0.070 
#> raises      |        0.08 |  0.22 | [ -0.38,  0.54] |  0.37 | 0.715 
#> critical    |        0.04 |  0.15 | [ -0.27,  0.34] |  0.26 | 0.796 
#> advance     |       -0.22 |  0.18 | [ -0.59,  0.15] | -1.22 | 0.236
```

We can explore a reduction of the number of parameters with the
[`reduce_parameters()`](https://easystats.github.io/parameters/reference/reduce_parameters.md)
function.

``` r

newmodel <- reduce_parameters(model)
parameters(newmodel)
#> Parameter                                                             
#> ----------------------------------------------------------------------
#> (Intercept)                                                           
#> raises 0 88/learning 0 82/complaints 0 78/privileges 0 70/advance 0 68
#> critical -0 80                                                        
#> 
#> Coefficient |   SE |         95% CI | t(27) |      p
#> ----------------------------------------------------
#>       64.63 | 1.57 | [61.41, 67.85] | 41.19 | < .001
#>        4.62 | 0.90 | [ 2.78,  6.46] |  5.16 | < .001
#>        3.41 | 1.59 | [ 0.14,  6.67] |  2.14 | 0.041
```

This output *hints* at the fact that the model could be represented via
**two “latent” dimensions**, one correlated with all the positive things
that a company has to offer, and the other one related to the amount of
negative critiques received by the employees. These two dimensions have
a positive and negative relationship with the company rating,
respectively.

> What does
> [`reduce_parameters()`](https://easystats.github.io/parameters/reference/reduce_parameters.md)
> exactly do?

This function performs a reduction in the parameter space (the number of
variables). It starts by creating a new set of variables, based on the
chosen method (the default method is “**PCA**”, but other are available
via the `method` argument, such as “**cMDS**”, “**DRR**” or “**ICA**”).
Then, it names this new dimensions using the original variables that
*correlate* the most with it. For instance, in the example above a
variable named
`raises_0.88/learning_0.82/complaints_0.78/privileges_0.70/advance_0.68`
means that the respective variables (`raises`, `learning`, `complaints`,
`privileges`, `advance`) correlate maximally (with coefficients of .88,
.82, .78, .70, .68, respectively) with this dimension.

``` r

reduce_parameters(model, method = "cMDS") %>%
  parameters()
#> Parameter                                                 | Coefficient |   SE
#> ------------------------------------------------------------------------------
#> (Intercept)                                               |       64.63 | 1.41
#> raises 0 85/complaints 0 84/learning 0 83/privileges 0 74 |        0.43 | 0.07
#> advance 0 60                                              |       -0.32 | 0.13
#> critical -0 65                                            |       -0.24 | 0.15
#> 
#> Parameter                                                 |         95% CI
#> --------------------------------------------------------------------------
#> (Intercept)                                               | [61.73, 67.53]
#> raises 0 85/complaints 0 84/learning 0 83/privileges 0 74 | [ 0.28,  0.57]
#> advance 0 60                                              | [-0.59, -0.04]
#> critical -0 65                                            | [-0.56,  0.07]
#> 
#> Parameter                                                 | t(26) |      p
#> --------------------------------------------------------------------------
#> (Intercept)                                               | 45.80 | < .001
#> raises 0 85/complaints 0 84/learning 0 83/privileges 0 74 |  6.14 | < .001
#> advance 0 60                                              | -2.36 | 0.026 
#> critical -0 65                                            | -1.61 | 0.120
```

A different method (**Classical Multidimensional Scaling - cMDS**)
suggests that negative critiques do not have a significant impact on the
rating, and that the lack of opportunities of career advancement is a
separate dimension with an importance on its own.

Although
[`reduce_parameters()`](https://easystats.github.io/parameters/reference/reduce_parameters.md)
function can be useful in exploratory data analysis, it’s best to
perform the dimension reduction step in a **separate and dedicated
stage**, as this is a very important process in the data analysis
workflow.

### Principal Component Analysis (PCA)

PCA is a widely used procedure that lies in-between dimension reduction
and structural modeling. Indeed, one of the ways of reducing the number
of predictors is to extract a new set of uncorrelated variables that
will *represent* variance of your initial dataset. But how the original
variables relate between themselves can also be a question on its own.

We can apply the
[`principal_components()`](https://easystats.github.io/parameters/reference/principal_components.md)
function to do the the predictors of the model:

``` r

pca <- principal_components(insight::get_predictors(model), n = "auto")
pca
#> # Loadings from Principal Component Analysis (no rotation)
#> 
#> Variable   |  PC1 | Complexity
#> ------------------------------
#> complaints | 0.78 |       1.00
#> privileges | 0.70 |       1.00
#> learning   | 0.82 |       1.00
#> raises     | 0.88 |       1.00
#> critical   | 0.40 |       1.00
#> advance    | 0.68 |       1.00
#> 
#> The unique principal component accounted for 52.82% of the total variance of the original data.
```

The
[`principal_components()`](https://easystats.github.io/parameters/reference/principal_components.md)
function automatically selected one component (if the number of
components is not specified, this function uses
[`n_factors()`](https://easystats.github.io/parameters/reference/n_factors.html)
to estimate the optimal number to keep) and returned the **loadings**,
i.e., the relationship with all of the original variables.

As we can see here, it seems that our new component captured the essence
(more than half of the total variance present in the original dataset)
of all our other variables together. We can **extract** the values of
this component for each of our observation using the
[`predict()`](https://rdrr.io/r/stats/predict.html) method and add in
the response variable of our initial dataset.

``` r

newdata <- predict(pca)
newdata$rating <- attitude$rating
```

We can know update the model with this new component:

``` r

update(model, rating ~ PC1, data = newdata) %>%
  parameters()
#> Parameter   | Coefficient |   SE |         95% CI | t(28) |      p
#> ------------------------------------------------------------------
#> (Intercept) |       64.63 | 1.67 | [61.22, 68.05] | 38.78 | < .001
#> PC1         |        4.62 | 0.95 | [ 2.67,  6.57] |  4.86 | < .001
```

#### Using the `psych` package for PCA

You can also use different packages for models, such as
[`psych`](https://cran.r-project.org/package=psych) (Revelle 2018) or
[`FactoMineR`](http://factominer.free.fr/) for PCA or Exploratory Factor
Analysis (EFA), as it allows for more flexibility and control when
running such procedures.

The functions from this package are **fully supported** by `parameters`
through the
[`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
function. For instance, we can redo the above analysis using the `psych`
package as follows:

``` r

library(psych)

# Fit the PCA
pca <- model_parameters(psych::principal(attitude, nfactors = 1))
pca
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
```

*Note:* By default,
[`psych::principal()`](https://rdrr.io/pkg/psych/man/principal.html)
uses a **varimax** rotation to extract rotated components, possibly
leading to discrepancies in the results.

Finally, refit the model:

``` r

df <- cbind(attitude, predict(pca))

update(model, rating ~ PC1, data = df) %>%
  model_parameters()
```

## References

Revelle, William. 2018. *Psych: Procedures for Psychological,
Psychometric, and Personality Research*. Northwestern University.
<https://CRAN.R-project.org/package=psych>.

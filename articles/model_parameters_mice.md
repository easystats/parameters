# Model Parameters for Multiply Imputed Repeated Analyses

## Model Parameters from `mira` objects

[`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
can be used in combination with the *mice* package to deal with missing
data, in particular to summaries regression models used with multiple
imputed datasets. It computes pooled summaries of multiple imputed
repeated regression analyses, i.e. of objects of class `mira`. Thus,
[`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
for `mira`-objects is comparable to the
[`pool()`](https://amices.org/mice/reference/pool.html)-function from
*mice*, but only focuses on the final summary of parameters and does not
include the diagnostic statistic per estimate.

``` r

library(mice)
library(parameters)

data("nhanes2")
imp <- mice(nhanes2, printFlag = FALSE)
fit <- with(data = imp, exp = lm(bmi ~ age + hyp + chl))

model_parameters(fit)
```

    #> # Fixed Effects
    #> 
    #> Parameter   | Coefficient |   SE |          95% CI | Statistic |    df |      p
    #> -------------------------------------------------------------------------------
    #> (Intercept) |       18.93 | 2.89 | [ 12.82, 25.03] |      6.54 | 16.91 | < .001
    #> age40-59    |       -6.16 | 1.65 | [ -9.74, -2.58] |     -3.73 | 12.59 | 0.003 
    #> age60-99    |       -7.87 | 2.17 | [-12.99, -2.76] |     -3.63 |  7.05 | 0.008 
    #> hypyes      |        2.23 | 1.68 | [ -1.40,  5.86] |      1.33 | 12.84 | 0.208 
    #> chl         |        0.06 | 0.02 | [  0.02,  0.09] |      3.51 | 17.27 | 0.003

Not all packages work with `with.mids()` from package *mice*. Thus, for
some modeling packages, it’s not possible to perform multiply imputed
repeated analyses, i.e. you cannot work with imputed data for such
models. We give an example for the *GLMMadaptive* package here.

First, we generate a dataset with missing values. We take the data
`cbpp` from *lme4* and randomly assign some missing values into one of
the predictors. Then we impute the data, using
[`mice()`](https://amices.org/mice/reference/mice.html) from package
*mice*.

``` r

library(lme4)
library(GLMMadaptive)

data(cbpp)
cbpp$period[sample(seq_len(nrow(cbpp)), size = 10)] <- NA

imputed_data <- mice(cbpp, printFlag = FALSE)
```

Using `with` to compute multiple regression analyses for each imputed
dataset fails.

``` r

fit <- with(data = imputed_data, expr = GLMMadaptive::mixed_model(
  cbind(incidence, size - incidence) ~ period,
  random = ~ 1 | herd,
  family = binomial
))
# > Error in as.data.frame(data) :
# >   argument "data" is missing, with no default
```

However, we can use a workaround by using
[`pool_parameters()`](https://easystats.github.io/parameters/reference/pool_parameters.md),
which works on a list of model objects. So whenever a model-object is
not yet supported by
[`mice::with()`](https://amices.org/mice/reference/with.mids.html), you
can instead fit multiple models to the imputed datasets and pool all
parameters with
[`pool_parameters()`](https://easystats.github.io/parameters/reference/pool_parameters.md):

The steps would be:

1.  Calculate the regression models for each imputed dataset manually
    (either by using
    [`complete()`](https://tidyr.tidyverse.org/reference/complete.html)
    from package *mice* to get the imputed datasets, or by accessing the
    datasets directly from the `mids` object)

2.  Save all model objects in a list.

3.  Pass the list to
    [`pool_parameters()`](https://easystats.github.io/parameters/reference/pool_parameters.md).

``` r

models <- lapply(1:imputed_data$m, function(i) {
  mixed_model(
    cbind(incidence, size - incidence) ~ period,
    random = ~ 1 | herd,
    data = complete(imputed_data, action = i),
    family = binomial
  )
})
pool_parameters(models)
```

    #> # Fixed Effects
    #> 
    #> Parameter   | Log-Odds |   SE |         95% CI | Statistic |      p
    #> -------------------------------------------------------------------
    #> (Intercept) |    -1.45 | 0.24 | [-1.93, -0.97] |     -5.93 | < .001
    #> period [2]  |    -0.88 | 0.34 | [-1.55, -0.20] |     -2.55 | 0.011 
    #> period [3]  |    -1.41 | 0.38 | [-2.16, -0.66] |     -3.70 | < .001
    #> period [4]  |    -1.65 | 0.62 | [-2.87, -0.43] |     -2.66 | 0.008

For comparison and to show that the results from `mice:pool()` and
[`pool_parameters()`](https://easystats.github.io/parameters/reference/pool_parameters.md)
are identical, we take an example that also works with the *mice*
package:

``` r

library(mice)
library(parameters)

data("nhanes2")
imp <- mice(nhanes2, printFlag = FALSE)

# approach when model is supported by "mice"
fit <- with(data = imp, exp = lm(bmi ~ age + hyp + chl))
summary(pool(fit))
```

    #>          term estimate std.error statistic df p.value
    #> 1 (Intercept)   19.667     3.373       5.8 11 0.00013
    #> 2    age40-59   -5.705     1.711      -3.3 14 0.00475
    #> 3    age60-99   -7.007     1.783      -3.9 18 0.00099
    #> 4      hypyes    2.713     1.829       1.5 11 0.16544
    #> 5         chl    0.051     0.017       2.9 12 0.01290

``` r

# approach when model is *not* supported by "mice"
models <- lapply(1:5, function(i) {
  lm(bmi ~ age + hyp + chl, data = complete(imp, action = i))
})
pool_parameters(models)
```

    #> # Fixed Effects
    #> 
    #> Parameter   | Coefficient |   SE |          95% CI | Statistic |    df |      p
    #> -------------------------------------------------------------------------------
    #> (Intercept) |       19.67 | 3.37 | [ 12.21, 27.13] |      5.83 | 10.62 | < .001
    #> age [40-59] |       -5.71 | 1.71 | [ -9.37, -2.04] |     -3.33 | 14.41 | 0.005 
    #> age [60-99] |       -7.01 | 1.78 | [-10.76, -3.26] |     -3.93 | 17.88 | < .001
    #> hyp [yes]   |        2.71 | 1.83 | [ -1.30,  6.73] |      1.48 | 11.24 | 0.165 
    #> chl         |        0.05 | 0.02 | [  0.01,  0.09] |      2.90 | 12.34 | 0.013

## Model Parameters from `mipo` objects

It is also possible to compute summaries of pooled objects of class
`mipo`.

``` r

data("nhanes2")
imp <- mice(nhanes2, printFlag = FALSE)
fit <- with(data = imp, exp = lm(bmi ~ age + hyp + chl))
pooled <- pool(fit)

model_parameters(pooled)
```

    #> # Fixed Effects
    #> 
    #> Parameter   | Coefficient |   SE |          95% CI |     t |    df |      p
    #> ---------------------------------------------------------------------------
    #> (Intercept) |       17.10 | 3.87 | [  8.63, 25.58] |  4.42 | 11.51 | < .001
    #> age [40-59] |       -5.03 | 2.55 | [-11.44,  1.38] | -1.97 |  5.42 | 0.101 
    #> age [60-99] |       -7.39 | 2.70 | [-13.87, -0.92] | -2.73 |  6.59 | 0.031 
    #> hyp [yes]   |        1.36 | 2.44 | [ -4.27,  6.99] |  0.56 |  8.03 | 0.593 
    #> chl         |        0.06 | 0.02 | [  0.01,  0.12] |  2.68 |  8.34 | 0.027

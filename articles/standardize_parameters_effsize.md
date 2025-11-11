# Parameter and Model Standardization

## Introduction

Standardizing parameters (*i.e.*, coefficients) can allow for their
comparison within and between models, variables and studies. Moreover,
as it returns coefficients expressed in terms of **change of variance**
(for instance, coefficients expressed in terms of SD of the response
variable), it can allow for the usage of [effect size interpretation
guidelines](https://easystats.github.io/effectsize/articles/interpret.html),
such as Cohen’s (1988) famous rules of thumb.

However, standardizing a model’s parameters should *not* be
automatically and mindlessly done: for some research fields, particular
variables or types of studies (*e.g.*, replications), it sometimes makes
more sense to keep, use and interpret the original parameters,
especially if they are well known or easily understood.

Critically, **parameters standardization is not a trivial process**.
Different techniques exist, that can lead to drastically different
results. Thus, it is critical that the standardization method is
explicitly documented and detailed.

### Standardizing Parameters of Simple Models

#### Standardized Associations

``` r

library(parameters)
library(effectsize)

m <- lm(rating ~ complaints, data = attitude)

standardize_parameters(m)
```

    > # Standardization method: refit
    > 
    > Parameter   | Std. Coef. |        95% CI
    > ----------------------------------------
    > (Intercept) |  -9.80e-16 | [-0.21, 0.21]
    > complaints  |       0.83 | [ 0.61, 1.04]

Standardizing the coefficient of this *simple* linear regression gives a
value of

``` r

round(standardize_parameters(m)[2, 2], 2)
```

    > [1] 0.83

But did you know that for a simple regression this is actually the
**same as a correlation**? Thus, you can eventually apply some
(*in*)famous interpretation guidelines (e.g., Cohen’s rules of thumb).

``` r

library(correlation)
correlation(attitude, select = c("rating", "complaints"))
```

    > # Correlation Matrix (pearson-method)
    > 
    > Parameter1 | Parameter2 |    r |       95% CI | t(28) |         p
    > -----------------------------------------------------------------
    > rating     | complaints | 0.83 | [0.66, 0.91] |  7.74 | < .001***
    > 
    > p-value adjustment method: Holm (1979)
    > Observations: 30

#### Standardized Differences

How does it work in the case of differences, when **factors** are
entered and differences between a given level and a reference level? You
might have heard that it is similar to a **Cohen’s *d***. Well, let’s
see.

``` r

# Select portion of data containing the two levels of interest
mtcars$am <- factor(mtcars$am, labels = c("Manual", "Automatic"))

m <- lm(mpg ~ am, data = mtcars)
standardize_parameters(m)
```

    > # Standardization method: refit
    > 
    > Parameter      | Std. Coef. |         95% CI
    > --------------------------------------------
    > (Intercept)    |      -0.49 | [-0.87, -0.11]
    > am [Automatic] |       1.20 | [ 0.60,  1.80]

This linear model suggests that the *standardized* difference between
*Manual* (the reference level - the model’s intercept) and *Automatic*
is of 1.20 standard deviation of `mpg` (because the response variable
was standardized, right?). Let’s compute the **Cohen’s *d*** between
these two levels:

``` r

library(effectsize)
cohens_d(mpg ~ am, data = mtcars)
```

    > Cohen's d |         95% CI
    > --------------------------
    > -1.48     | [-2.27, -0.67]
    > 
    > - Estimated using pooled SD.

***It is larger!*** Why? How? Both differences should be expressed in
units of SD! But which SDs? Different SDs!

When looking at the difference between groups as a **slope**, the
standardized parameter is the difference between the means in SD\_{mpg}.
That is, the *slope* between `Manual` and `Automatic` is a change of
1.20 SD\_{mpg}s.

However, when looking a the difference as a **distance between two
populations**, Cohen’s d is the distance between the means in units of
[**pooled
SDs**](https://easystats.github.io/effectsize/reference/sd_pooled.html).
That is, the *distance* between `Manual` and `Automatic` is of 1.48 SDs
of *each of the groups* (here assumed to be equal).

In this simple model, the pooled SD is the residual SD, so we can also
estimate Cohen’s *d* as:

``` r

coef(m)[2] / sigma(m)
```

    > amAutomatic 
    >         1.5

And we can also get an approximation of Cohen’s *d* by converting the
t-statistic from the regression model via
[`t_to_d()`](https://easystats.github.io/effectsize/reference/t_to_r.html):

``` r

model_parameters(m)
```

    > Parameter      | Coefficient |   SE |         95% CI | t(30) |      p
    > ---------------------------------------------------------------------
    > (Intercept)    |       17.15 | 1.12 | [14.85, 19.44] | 15.25 | < .001
    > am [Automatic] |        7.24 | 1.76 | [ 3.64, 10.85] |  4.11 | < .001

``` r

t_to_d(4.11, df_error = 30)
```

    > d    |       95% CI
    > -------------------
    > 1.50 | [0.68, 2.30]

It is also interesting to note that using the `smart` method (explained
in detail below) when standardizing parameters will give you indices
equivalent to **Glass’ *delta***, which is a standardized difference
expressed in terms of SD of the reference group.

``` r

m <- lm(mpg ~ am, data = mtcars)

standardize_parameters(m, method = "smart")
```

    > # Standardization method: smart
    > 
    > Parameter      | Std. Coef. |       95% CI
    > ------------------------------------------
    > (Intercept)    |       0.00 | [0.00, 0.00]
    > am [Automatic] |       1.17 | [0.59, 1.76]

``` r

glass_delta(mpg ~ am, data = mtcars)
```

    > Glass' delta (adj.) |         95% CI
    > ------------------------------------
    > -1.10               | [-1.80, -0.37]

***… So note that some standardized differences are different than
others! :)***

### Standardizing Parameters of Linear Models

As mentioned above, standardization of parameters can also be used to
compare among parameters within the same model. Essentially, what
prevents us from normally being able to compare among different
parameters is that their underlying variables are on different
scales.\[^But also as noted above, this is not always an issue. For
example, when the variables scale is important for the interpretation of
results, standardization might in fact hinder interpretation!\]

For example, in the following example, we use a liner regression model
to predict a worker’s salary (in Shmekels) from their age (years),
seniority (years), overtime (`xtra_hours`) and how many compliments they
give their boss (`n_comps`).

Let us explore the different parameter standardization methods provided
by *parameters*.

#### Standardized Slopes are Not (Always) Correlations

We saw that in simple linear models, the standardized slope is equal to
the correlation between the outcome and predictor - does this hold for
**multiple regression** as well? As in each effect in a regression model
is “adjusted” for the other ones, we might expect coefficients to be
somewhat alike to **partial correlations**. Let’s first start by
computing the partial correlation between numeric predictors and the
outcome.

``` r

data("hardlyworking", package = "effectsize")
head(hardlyworking)
```

    >   salary xtra_hours n_comps age seniority is_senior
    > 1  19745        4.2       1  32         3     FALSE
    > 2  11302        1.6       0  34         3     FALSE
    > 3  20636        1.2       3  33         5      TRUE
    > 4  23047        7.2       1  35         3     FALSE
    > 5  27342       11.3       0  33         4     FALSE
    > 6  25657        3.6       2  30         5      TRUE

``` r

correlation(
  hardlyworking,
  select = "salary",
  select2 = c("xtra_hours", "n_comps", "age", "seniority"),
  partial = TRUE # get partial correlations
)
```

    > # Correlation Matrix (pearson-method)
    > 
    > Parameter1 | Parameter2 |    r |       95% CI | t(498) |         p
    > ------------------------------------------------------------------
    > salary     | xtra_hours | 0.87 | [0.85, 0.89] |  39.80 | < .001***
    > salary     |    n_comps | 0.71 | [0.66, 0.75] |  22.40 | < .001***
    > salary     |        age | 0.09 | [0.01, 0.18] |   2.10 | 0.037*   
    > salary     |  seniority | 0.19 | [0.10, 0.27] |   4.30 | < .001***
    > 
    > p-value adjustment method: Holm (1979)
    > Observations: 500

Let’s compare these to the standardized slopes:

``` r

mod <- lm(salary ~ xtra_hours + n_comps + age + seniority,
  data = hardlyworking
)

standardize_parameters(mod)
```

    > # Standardization method: refit
    > 
    > Parameter   | Std. Coef. |        95% CI
    > ----------------------------------------
    > (Intercept) |   2.77e-16 | [-0.03, 0.03]
    > xtra hours  |       0.77 | [ 0.73, 0.81]
    > n comps     |       0.39 | [ 0.36, 0.42]
    > age         |       0.04 | [ 0.00, 0.07]
    > seniority   |       0.08 | [ 0.04, 0.12]

They are quite different! It seems then that ***standardized slopes in
multiple linear regressions are not the same a correlations or partial
correlations*** :(

However, not all hope is lost yet - we can still try and recover the
partial correlations from our model, in another way: by converting the
*t*-statistics (and their degrees of freedom, *df*) into a partial
correlation coefficient *r*.

``` r

params <- model_parameters(mod)

t_to_r(params$t[-1], df_error = params$df_error[-1])
```

    > r    |       95% CI
    > -------------------
    > 0.87 | [0.85, 0.89]
    > 0.71 | [0.67, 0.74]
    > 0.09 | [0.01, 0.18]
    > 0.19 | [0.10, 0.27]

Wow, the retrieved correlations coefficients from the regression model
are **exactly** the same as the partial correlations we estimated above!
So these “*r*” effect sizes can also be used.

#### Methods of Standardizing Parameters

Let’s convert `age` into a 3-level factor:

``` r

hardlyworking$age_g <- cut(hardlyworking$age,
  breaks = c(25, 30, 35, 45)
)

mod <- lm(salary ~ xtra_hours + n_comps + age_g + seniority,
  data = hardlyworking
)

model_parameters(mod)
```

    > Parameter      | Coefficient |     SE |              95% CI | t(494) |      p
    > -----------------------------------------------------------------------------
    > (Intercept)    |     9806.10 | 446.71 | [8928.41, 10683.79] |  21.95 | < .001
    > xtra hours     |     1221.35 |  30.72 | [1161.00,  1281.71] |  39.76 | < .001
    > n comps        |     2944.87 | 131.12 | [2687.25,  3202.48] |  22.46 | < .001
    > age g [>30-35] |      393.36 | 241.01 | [ -80.18,   866.90] |   1.63 | 0.103 
    > age g [>35-45] |      597.61 | 427.72 | [-242.77,  1437.99] |   1.40 | 0.163 
    > seniority      |      443.76 | 102.38 | [ 242.62,   644.91] |   4.33 | < .001

It seems like the best or most important predictor is `n_comps` as it
has the coefficient. However, it is hard to compare among predictors, as
they are on different scales. To address this issue, we must have all
the predictors on the same scale - usually in the arbitrary unit of
*standard deviations*.

##### **`"refit"`**: Re-fitting the model with standardized data

**This method is based on a complete model re-fit with a standardized
version of data**. Hence, this method is equal to standardizing the
variables *before* fitting the model. It is the “purest” and the most
accurate (Neter et al. 1989), but it is also the most computationally
costly and long (especially for heavy models such as Bayesian models, or
complex mixed models). This method is particularly recommended for
models that include interactions or transformations (e.g.,
exponentiation, log, polynomial or spline terms).

``` r

standardize_parameters(mod, method = "refit")
```

    > # Standardization method: refit
    > 
    > Parameter      | Std. Coef. |        95% CI
    > -------------------------------------------
    > (Intercept)    |      -0.05 | [-0.11, 0.02]
    > xtra hours     |       0.77 | [ 0.73, 0.81]
    > n comps        |       0.39 | [ 0.36, 0.43]
    > age g [>30-35] |       0.06 | [-0.01, 0.14]
    > age g [>35-45] |       0.10 | [-0.04, 0.23]
    > seniority      |       0.08 | [ 0.04, 0.12]

`standardize_parameters` also has a `robust` argument (default to
`FALSE`), which enables a **robust standardization of the data**,
*i.e.*, based on the **median** and **MAD** instead of the **mean** and
**SD**:

``` r

standardize_parameters(mod, method = "refit", robust = TRUE)
```

    > # Standardization method: refit
    > 
    > Parameter      | Std. Coef. |         95% CI
    > --------------------------------------------
    > (Intercept)    |      -0.20 | [-0.27, -0.13]
    > xtra hours     |       0.65 | [ 0.62,  0.68]
    > n comps        |       0.82 | [ 0.74,  0.89]
    > age g [>30-35] |       0.07 | [-0.01,  0.16]
    > age g [>35-45] |       0.11 | [-0.05,  0.27]
    > seniority      |       0.12 | [ 0.07,  0.18]
    > 
    > - Scaled by one MAD from the median.

Note that since `age_g` is a factor, it is not numerically standardized,
and so it standardized parameter is still not directly comparable to
those of numeric variables. To address this, we can set `two_sd = TRUE`,
thereby scaling parameters on 2 SDs (or MADs) of the predictors (Gelman
2008).

``` r

standardize_parameters(mod, method = "refit", two_sd = TRUE)
```

    > # Standardization method: refit
    > 
    > Parameter      | Std. Coef. |        95% CI
    > -------------------------------------------
    > (Intercept)    |      -0.05 | [-0.11, 0.02]
    > xtra hours     |       1.54 | [ 1.46, 1.61]
    > n comps        |       0.78 | [ 0.72, 0.85]
    > age g [>30-35] |       0.06 | [-0.01, 0.14]
    > age g [>35-45] |       0.10 | [-0.04, 0.23]
    > seniority      |       0.16 | [ 0.09, 0.24]
    > 
    > - Scaled by two SDs from the mean.

*parameters* also comes with a helper function that returns the re-fit
model, without summarizing it, which can then be used as the original
model would:

``` r

mod_z <- standardize(mod, two_sd = FALSE, robust = FALSE)
mod_z
```

    > 
    > Call:
    > lm(formula = salary ~ xtra_hours + n_comps + age_g + seniority, 
    >     data = data_std)
    > 
    > Coefficients:
    >  (Intercept)    xtra_hours       n_comps  age_g(30,35]  age_g(35,45]  
    >      -0.0458        0.7692        0.3921        0.0635        0.0964  
    >    seniority  
    >       0.0821

``` r

model_parameters(mod_z)
```

    > Parameter      | Coefficient |   SE |        95% CI | t(494) |      p
    > ---------------------------------------------------------------------
    > (Intercept)    |       -0.05 | 0.03 | [-0.11, 0.02] |  -1.47 | 0.142 
    > xtra hours     |        0.77 | 0.02 | [ 0.73, 0.81] |  39.76 | < .001
    > n comps        |        0.39 | 0.02 | [ 0.36, 0.43] |  22.46 | < .001
    > age g [>30-35] |        0.06 | 0.04 | [-0.01, 0.14] |   1.63 | 0.103 
    > age g [>35-45] |        0.10 | 0.07 | [-0.04, 0.23] |   1.40 | 0.163 
    > seniority      |        0.08 | 0.02 | [ 0.04, 0.12] |   4.33 | < .001

##### **`"posthoc"`**: Refit without refitting

Post-hoc standardization of the parameters aims at emulating the results
obtained by `"refit"` without refitting the model. The coefficients are
divided by the standard deviation (or MAD if `robust`) of the outcome
(which becomes their expression ‘unit’). Then, the coefficients related
to numeric variables are additionally multiplied by the standard
deviation (or MAD if `robust`) of the related terms, so that they
correspond to changes of 1 SD of the predictor (e.g., “A change in 1 SD
of *x* is related to a change of 0.24 of the SD of *y*). This does not
apply to binary variables or factors, so the coefficients are still
related to changes in levels. This method is not accurate and tend to
give aberrant results when interactions are specified.

But why is interaction for posthoc-standardization problematic? A
regression model estimates coefficient between two variables when the
other predictors are at 0 (are *fixed* at 0, that people interpret as
*“adjusted for”*). When a standardized data is passed (in the *refit*
method), the effects and interactions are estimated at the *means* of
the other predictors (because 0 is the mean for a standardized
variable). Whereas in posthoc-standardization, this coefficient
corresponds to something different (because the 0 has different meanings
in standardized and non-standardized data).

``` r

standardize_parameters(mod, method = "posthoc")
```

    > # Standardization method: posthoc
    > 
    > Parameter      | Std. Coef. |        95% CI
    > -------------------------------------------
    > (Intercept)    |       0.00 | [ 0.00, 0.00]
    > xtra hours     |       0.77 | [ 0.73, 0.81]
    > n comps        |       0.39 | [ 0.36, 0.43]
    > age g [>30-35] |       0.06 | [-0.01, 0.14]
    > age g [>35-45] |       0.10 | [-0.04, 0.23]
    > seniority      |       0.08 | [ 0.04, 0.12]

##### **`"smart"`**: Standardization of Model’s parameters with Adjustment, Reconnaissance and Transformation

> Experimental

Similar to `method = "posthoc"` in that it does not involve model
refitting. The difference is that the SD of the response is computed on
the relevant section of the data. For instance, if a factor with 3
levels A (the intercept), B and C is entered as a predictor, the effect
corresponding to B vs. A will be scaled by the variance of the response
at the intercept only. As a results, the coefficients for effects of
factors are similar to a Glass’ *delta*.

``` r

standardize_parameters(mod, method = "smart")
```

    > # Standardization method: smart
    > 
    > Parameter      | Std. Coef. |        95% CI
    > -------------------------------------------
    > (Intercept)    |       0.00 | [ 0.00, 0.00]
    > xtra hours     |       0.77 | [ 0.73, 0.81]
    > n comps        |       0.39 | [ 0.36, 0.43]
    > age g [>30-35] |       0.06 | [-0.01, 0.14]
    > age g [>35-45] |       0.10 | [-0.04, 0.23]
    > seniority      |       0.08 | [ 0.04, 0.12]

##### **`"basic"`**: Raw scaling of the model frame

This method is similar to `method = "posthoc"`, but treats all variables
as continuous: it scales the coefficient by the standard deviation of
model’s matrix’ parameter of factors levels (transformed to integers) or
binary predictors. Although it can be argued that this might be
inappropriate for these cases, this method allows for easier importance
judgment across all predictor type (numeric, factor, interactions…). It
is also the type of standardization implemented by default in other
software packages (also
[`lm.beta::lm.beta()`](https://rdrr.io/pkg/lm.beta/man/lm.beta.html)),
and, such as can be used for reproducibility and replication purposes.

``` r

standardize_parameters(mod, method = "basic")
```

    > # Standardization method: basic
    > 
    > Parameter      | Std. Coef. |        95% CI
    > -------------------------------------------
    > (Intercept)    |       0.00 | [ 0.00, 0.00]
    > xtra hours     |       0.77 | [ 0.73, 0.81]
    > n comps        |       0.39 | [ 0.36, 0.43]
    > age g [>30-35] |       0.03 | [-0.01, 0.07]
    > age g [>35-45] |       0.03 | [-0.01, 0.06]
    > seniority      |       0.08 | [ 0.04, 0.12]

#### Standardizing Parameters In Mixed Models

Linear mixed models (LMM/HLM/MLM) offer an additional conundrum to
standardization - how does one even calculate the SDs of the various
predictors? Or of the response - is it the deviations within each group?
Or perhaps between them?

The solution: standardize according to level of the predictor (Hoffman
2015, 342)! Level 1 parameters are standardized according to variance
*within* groups, while level 2 parameters are standardized according to
variance *between* groups. The resulting standardized coefficient are
also called *pseudo*-standardized coefficients.\[^Note that like method
`"basic"`, these are based on the model matrix.\]

``` r

m <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)

standardize_parameters(m, method = "pseudo", ci_method = "satterthwaite")
```

    > # Standardization method: pseudo
    > 
    > Parameter   | Std. Coef. |       95% CI
    > ---------------------------------------
    > (Intercept) |       0.00 | [0.00, 0.00]
    > Days        |       0.68 | [0.47, 0.89]

``` r

# compare to:
standardize_parameters(m, method = "basic", ci_method = "satterthwaite")
```

    > # Standardization method: basic
    > 
    > Parameter   | Std. Coef. |       95% CI
    > ---------------------------------------
    > (Intercept) |       0.00 | [0.00, 0.00]
    > Days        |       0.54 | [0.37, 0.70]

#### Standardizing Parameters In Generalized Linear Models

Unlike linear (/mixed) models, in generalized linear (/mixed) models
(GLMs) there is *less* of a need for standardization. Why? Because in
many GLMs the estimated coefficients are themselves measures of effect
size, such as *odds-ratios* (OR) in logistic regression, or *incidence
rate ratios* (IRR) in Poisson regressions. This is because in such model
the outcome is **not** on an arbitrary scale - that is, the meaning of
rates and probabilities are changed by arbitrary linear transformations.

But still, some standardization is sometimes needed, for the predictors.
Luckily,
[`standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.md)
(and
[`standardize()`](https://easystats.github.io/datawizard/reference/standardize.html))
are smart enough to know when GLMs are passed so as to only standardize
according to the predictors:

``` r

mod_b <- glm(am ~ mpg + factor(cyl),
  data = mtcars,
  family = binomial()
)

standardize_parameters(mod_b, method = "refit", two_sd = TRUE)
```

    > # Standardization method: refit
    > 
    > Parameter                 | Std. Coef. |         95% CI
    > -------------------------------------------------------
    > (Intercept)               |      -0.91 | [-3.32,  1.33]
    > mpg                       |       4.46 | [ 0.30, 10.54]
    > cyl [-0.0524939042876197] |       0.73 | [-2.04,  3.66]
    > cyl [0.507441074780324]   |       0.70 | [-3.13,  4.78]
    > 
    > - Scaled by two SDs from the mean.
    > - Response is unstandardized.

``` r

# standardize_parameters(mod_b, method = "posthoc", two_sd = TRUE)
# standardize_parameters(mod_b, method = "basic")
```

These can then be converted to OR (with
[`exp()`](https://rdrr.io/r/base/Log.html)) and discussed as the
“*change in Odds as a function of a change in one SD of x*”.

``` r

std <- standardize_parameters(mod_b, method = "refit", two_sd = TRUE)
exp(std$Std_Coefficient)
```

    > [1]  0.4 86.4  2.1  2.0

Or we can directly ask for the coefficients to be exponentiated:

``` r

standardize_parameters(mod_b, method = "refit", two_sd = TRUE, exponentiate = TRUE)
```

    > # Standardization method: refit
    > 
    > Parameter                 | Std_Odds_Ratio |           95% CI
    > -------------------------------------------------------------
    > (Intercept)               |           0.40 | [0.04,     3.76]
    > mpg                       |          86.40 | [1.36, 37955.41]
    > cyl [-0.0524939042876197] |           2.08 | [0.13,    39.04]
    > cyl [0.507441074780324]   |           2.02 | [0.04,   119.12]
    > 
    > - Scaled by two SDs from the mean.
    > - Response is unstandardized.

## References

Gelman, Andrew. 2008. “Scaling Regression Inputs by Dividing by Two
Standard Deviations.” *Statistics in Medicine* 27 (15): 2865–73.

Hoffman, Lesa. 2015. *Longitudinal Analysis: Modeling Within-Person
Fluctuation and Change*. Routledge.

Neter, John, William Wasserman, and Michael H Kutner. 1989. *Applied
Linear Regression Models*.

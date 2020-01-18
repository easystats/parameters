Parameters Selection
================

  - [Simple linear regression](#simple-linear-regression)
      - [Fit a powerful model](#fit-a-powerful-model)
      - [Too many parameters?](#too-many-parameters)
      - [Parameters selection](#parameters-selection)
  - [Mixed and Bayesian models](#mixed-and-bayesian-models)
      - [Mixed model](#mixed-model)

<img src="https://raw.githubusercontent.com/easystats/parameters/master/man/figures/figure2.png" style="display: block; margin: auto;" />

Also known as [**feature
selection**](https://en.wikipedia.org/wiki/Feature_selection) in machine
learning, the goal of variable selection is to **identify a subset of
predictors** to **simplify models**. This can benefit model
interpretation, shorten fitting time, and improve generalization (by
reducing overfitting).

There are many different methods. The one that is appropriate for a
given problem depends on the model type, the data, the objective and the
theorethical rationale.

The `parameters` package implements a helper that will **automatically
pick a method deemed appropriate for the provided model**, run the
variables selection and return the **optimal formula**, which you can
then re-use to update the model.

## Simple linear regression

### Fit a powerful model

If you are familiar with R and the formula interface, you know of the
possibility of including a dot (`.`) in the formula, signifying **“all
the remaining variables”**. Curiously, few are aware of the possibility
of additionally easily adding **all the interaction terms**. This can be
achieved using the `*.*` notation.

Let’s try that with the linear regression predicting **Sepal.Length**
with the [`iris`](https://en.wikipedia.org/wiki/Iris_flower_data_set)
dataset, included by default in R.

``` r
model <- lm(Sepal.Length ~ .*., data=iris)
summary(model)
```

    > 
    > Call:
    > lm(formula = Sepal.Length ~ . * ., data = iris)
    > 
    > Residuals:
    >    Min     1Q Median     3Q    Max 
    > -0.726 -0.210  0.014  0.213  0.713 
    > 
    > Coefficients:
    >                                Estimate Std. Error t value Pr(>|t|)   
    > (Intercept)                      1.6998     1.0576    1.61   0.1103   
    > Sepal.Width                      0.8301     0.3047    2.72   0.0073 **
    > Petal.Length                     0.3178     0.7852    0.40   0.6863   
    > Petal.Width                      2.5827     1.5874    1.63   0.1061   
    > Speciesversicolor               -2.7193     1.6201   -1.68   0.0956 . 
    > Speciesvirginica                -6.1704     3.2045   -1.93   0.0563 . 
    > Sepal.Width:Petal.Length        -0.0149     0.2185   -0.07   0.9458   
    > Sepal.Width:Petal.Width         -0.6112     0.4536   -1.35   0.1801   
    > Sepal.Width:Speciesversicolor    0.4300     0.6666    0.65   0.5200   
    > Sepal.Width:Speciesvirginica     0.8288     1.0031    0.83   0.4101   
    > Petal.Length:Petal.Width        -0.1195     0.3300   -0.36   0.7178   
    > Petal.Length:Speciesversicolor   0.7398     0.5166    1.43   0.1544   
    > Petal.Length:Speciesvirginica    0.9034     0.6938    1.30   0.1951   
    > Petal.Width:Speciesversicolor   -1.0070     1.2454   -0.81   0.4202   
    > Petal.Width:Speciesvirginica    -0.2864     1.5065   -0.19   0.8495   
    > ---
    > Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    > 
    > Residual standard error: 0.3 on 135 degrees of freedom
    > Multiple R-squared:  0.882,   Adjusted R-squared:  0.87 
    > F-statistic: 72.1 on 14 and 135 DF,  p-value: <2e-16

***Wow, that’s a lot of parameters\! And almost none of them is
significant…***

Which is ***weird***, considering that **gorgeous R2\! 0.882\!** *I wish
I had that in my research…*

### Too many parameters?

As you might know, having a **model that is too performant is not always
a good thing**. For instance, it can be a marker of
[**overfitting**](https://en.wikipedia.org/wiki/Overfitting): the model
corresponds too closely to a particular set of data, and may therefore
fail to predict future observations reliably. In multiple regressions,
in can also fall under the [**Freedman’s
paradox**](https://en.wikipedia.org/wiki/Freedman%27s_paradox): some
predictors that have actually no relation to the dependent variable
being predicted will be **spuriously found to be statistically
significant**.

Let’s run a few checks using the
[**performance**](https://github.com/easystats/performance) package:

``` r
library(performance)

check_normality(model)
```

    > OK: Residuals appear as normally distributed (p = 0.612).

``` r
check_heteroscedasticity(model)
```

    > OK: Error variance appears to be homoscedastic (p = 0.118).

``` r
check_autocorrelation(model)
```

    > OK: Residuals appear to be independent and not autocorrelated (p = 0.574).

``` r
check_collinearity(model)
```

    > # Check for Multicollinearity
    > 
    > High Correlation
    > 
    >                 Parameter        VIF Increased SE
    >               Sepal.Width      29.42         5.42
    >              Petal.Length    3204.78        56.61
    >               Petal.Width    2442.10        49.42
    >                   Species  398125.98       630.97
    >  Sepal.Width:Petal.Length    2183.98        46.73
    >   Sepal.Width:Petal.Width    1866.48        43.20
    >       Sepal.Width:Species  349436.10       591.13
    >  Petal.Length:Petal.Width    4032.80        63.50
    >      Petal.Length:Species 1230070.70      1109.09
    >       Petal.Width:Species  377458.32       614.38

The main issue of the model seems to be the high
[multicollinearity](https://en.wikipedia.org/wiki/Multicollinearity).
This suggests that our model might not be able to give valid results
about any individual predictor, nor tell which predictors are redundant
with respect to others.

### Parameters selection

Time to do some variables selection. This can be easily done using the
`select_parameters()` function, that will **automatically select the
best variables** and update the model accordingly. One way of using that
is in a tidy pipeline (using
[`%>%`](https://cran.r-project.org/web/packages/magrittr/README.html)),
using this output to update a new model.

``` r
library(dplyr)
library(parameters)

lm(Sepal.Length ~ .*., data=iris) %>% 
  select_parameters() %>% 
  summary()
```

    > 
    > Call:
    > lm(formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + 
    >     Species + Sepal.Width:Petal.Width + Petal.Length:Species + 
    >     Petal.Width:Species, data = iris)
    > 
    > Residuals:
    >     Min      1Q  Median      3Q     Max 
    > -0.7261 -0.2165  0.0021  0.2191  0.7439 
    > 
    > Coefficients:
    >                                Estimate Std. Error t value Pr(>|t|)    
    > (Intercept)                       2.090      0.528    3.95  0.00012 ***
    > Sepal.Width                       0.734      0.130    5.66  8.3e-08 ***
    > Petal.Length                      0.232      0.260    0.89  0.37310    
    > Petal.Width                       1.051      0.532    1.98  0.04993 *  
    > Speciesversicolor                -1.047      0.547   -1.92  0.05754 .  
    > Speciesvirginica                 -2.682      0.638   -4.21  4.6e-05 ***
    > Sepal.Width:Petal.Width          -0.232      0.103   -2.24  0.02667 *  
    > Petal.Length:Speciesversicolor    0.660      0.298    2.22  0.02837 *  
    > Petal.Length:Speciesvirginica     0.720      0.273    2.63  0.00941 ** 
    > Petal.Width:Speciesversicolor    -1.112      0.550   -2.02  0.04528 *  
    > Petal.Width:Speciesvirginica     -0.499      0.460   -1.09  0.27934    
    > ---
    > Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    > 
    > Residual standard error: 0.3 on 139 degrees of freedom
    > Multiple R-squared:  0.88,    Adjusted R-squared:  0.872 
    > F-statistic:  102 on 10 and 139 DF,  p-value: <2e-16

That’s still a lot of parameters, but as you can see, but almost all of
them are now significant, and the R2 did not change much.

Although appealing, please note that these automated selection methods
are [**quite
criticized**](https://towardsdatascience.com/stopping-stepwise-why-stepwise-selection-is-bad-and-what-you-should-use-instead-90818b3f52df),
and should not be used in place of **theoretical** or **hypothetical**
reasons (*i.e.*, you should have justified hypotheses about the
parameters of your model).

## Mixed and Bayesian models

For simple linear regressions as above, the selection is made using the
`step()` function (available in base R). This performs a
[**stepwise**](https://en.wikipedia.org/wiki/Stepwise_regression)
selection. However, this procedures is not available for other types of
models, such as **mixed** or **Bayesian** models.

### Mixed model

``` r
library(lme4)

lmer(Sepal.Length ~ Sepal.Width * Petal.Length * Petal.Width + (1|Species), data=iris) %>%
  select_parameters() %>%
  summary()
```

    > Linear mixed model fit by REML ['lmerMod']
    > Formula: Sepal.Length ~ Sepal.Width * Petal.Length * Petal.Width + (1 |  
    >     Species)
    >    Data: iris
    > 
    > REML criterion at convergence: 95
    > 
    > Scaled residuals: 
    >     Min      1Q  Median      3Q     Max 
    > -2.4935 -0.8045  0.0176  0.6593  2.3356 
    > 
    > Random effects:
    >  Groups   Name        Variance Std.Dev.
    >  Species  (Intercept) 0.0352   0.188   
    >  Residual             0.0932   0.305   
    > Number of obs: 150, groups:  Species, 3
    > 
    > Fixed effects:
    >                                      Estimate Std. Error t value
    > (Intercept)                            1.7768     0.9128    1.95
    > Sepal.Width                            0.7501     0.2681    2.80
    > Petal.Length                           0.8793     0.4971    1.77
    > Petal.Width                           -2.0380     1.5196   -1.34
    > Sepal.Width:Petal.Length              -0.1012     0.1611   -0.63
    > Sepal.Width:Petal.Width                0.3435     0.4894    0.70
    > Petal.Length:Petal.Width               0.2805     0.2428    1.16
    > Sepal.Width:Petal.Length:Petal.Width  -0.0467     0.0769   -0.61
    > 
    > Correlation of Fixed Effects:
    >             (Intr) Spl.Wd Ptl.Ln Ptl.Wd Sp.W:P.L S.W:P.W P.L:P.
    > Sepal.Width -0.948                                             
    > Petal.Lngth -0.746  0.757                                      
    > Petal.Width -0.289  0.222 -0.306                               
    > Spl.Wdt:P.L  0.705 -0.762 -0.983  0.338                        
    > Spl.Wdt:P.W  0.250 -0.214  0.336 -0.986 -0.356                 
    > Ptl.Lng:P.W  0.754 -0.705 -0.459 -0.679  0.416    0.642        
    > S.W:P.L:P.W -0.732  0.730  0.442  0.675 -0.428   -0.670  -0.976

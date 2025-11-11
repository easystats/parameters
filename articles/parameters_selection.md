# Selection of Model Parameters

Also known as [**feature
selection**](https://en.wikipedia.org/wiki/Feature_selection) in machine
learning, the goal of variable selection is to **identify a subset of
predictors** to **simplify models**. This can benefit model
interpretation, shorten fitting time, and improve generalization (by
reducing overfitting).

There are many different methods. The appropriate method for a given
problem will depend on the model type, the data, the objective, and the
theoretical rationale.

The `parameters` package implements a helper that will **automatically**
pick a method deemed appropriate for the provided model, run the
variables selection and return the **optimal formula**, which you can
then re-use to update the model.

## Simple linear regression

### Fit a powerful model

If you are familiar with R and the formula interface, you know of the
possibility of including a dot (`.`) in the formula, signifying “all the
remaining variables”. Curiously, few are aware of the possibility of
additionally easily adding “all the interaction terms”. This can be
achieved using the `.*.` notation.

Let’s try that with the linear regression predicting **Sepal.Length**
with the [`iris`](https://en.wikipedia.org/wiki/Iris_flower_data_set)
dataset, included by default in R.

``` r

model <- lm(Sepal.Length ~ . * ., data = iris)
summary(model)
#> 
#> Call:
#> lm(formula = Sepal.Length ~ . * ., data = iris)
#> 
#> Residuals:
#>    Min     1Q Median     3Q    Max 
#> -0.726 -0.210  0.014  0.213  0.713 
#> 
#> Coefficients:
#>                                Estimate Std. Error t value Pr(>|t|)   
#> (Intercept)                      1.6998     1.0576    1.61   0.1103   
#> Sepal.Width                      0.8301     0.3047    2.72   0.0073 **
#> Petal.Length                     0.3178     0.7852    0.40   0.6863   
#> Petal.Width                      2.5827     1.5874    1.63   0.1061   
#> Speciesversicolor               -2.7193     1.6201   -1.68   0.0956 . 
#> Speciesvirginica                -6.1704     3.2045   -1.93   0.0563 . 
#> Sepal.Width:Petal.Length        -0.0149     0.2185   -0.07   0.9458   
#> Sepal.Width:Petal.Width         -0.6112     0.4536   -1.35   0.1801   
#> Sepal.Width:Speciesversicolor    0.4300     0.6666    0.65   0.5200   
#> Sepal.Width:Speciesvirginica     0.8288     1.0031    0.83   0.4101   
#> Petal.Length:Petal.Width        -0.1195     0.3300   -0.36   0.7178   
#> Petal.Length:Speciesversicolor   0.7398     0.5166    1.43   0.1544   
#> Petal.Length:Speciesvirginica    0.9034     0.6938    1.30   0.1951   
#> Petal.Width:Speciesversicolor   -1.0070     1.2454   -0.81   0.4202   
#> Petal.Width:Speciesvirginica    -0.2864     1.5065   -0.19   0.8495   
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.3 on 135 degrees of freedom
#> Multiple R-squared:  0.882,  Adjusted R-squared:  0.87 
#> F-statistic: 72.1 on 14 and 135 DF,  p-value: <2e-16
```

***Wow, that’s a lot of parameters! And almost none of them are
significant!***

Which is ***weird***, considering that **gorgeous R^2 of 0.882!**

*I wish I had that in my research!*

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
#> OK: residuals appear as normally distributed (p = 0.612).
check_heteroscedasticity(model)
#> OK: Error variance appears to be homoscedastic (p = 0.118).
check_autocorrelation(model)
#> OK: Residuals appear to be independent and not autocorrelated (p = 0.574).
check_collinearity(model)
#> # Check for Multicollinearity
#> 
#> High Correlation
#> 
#>                      Term      VIF           VIF 95% CI adj. VIF Tolerance
#>               Sepal.Width    29.42 [   22.31,    38.91]     5.42      0.03
#>              Petal.Length  3204.78 [ 2414.74,  4253.41]    56.61  3.12e-04
#>               Petal.Width  2442.10 [ 1840.11,  3241.14]    49.42  4.09e-04
#>                   Species 3.98e+05 [3.00e+05, 5.28e+05]    25.12  2.51e-06
#>  Sepal.Width:Petal.Length  2183.98 [ 1645.63,  2898.55]    46.73  4.58e-04
#>   Sepal.Width:Petal.Width  1866.48 [ 1406.42,  2477.15]    43.20  5.36e-04
#>       Sepal.Width:Species 3.49e+05 [2.63e+05, 4.64e+05]   591.13  2.86e-06
#>  Petal.Length:Petal.Width  4032.80 [ 3038.60,  5352.41]    63.50  2.48e-04
#>      Petal.Length:Species 1.23e+06 [9.27e+05, 1.63e+06]  1109.09  8.13e-07
#>       Petal.Width:Species 3.77e+05 [2.84e+05, 5.01e+05]   614.38  2.65e-06
#>  Tolerance 95% CI
#>      [0.03, 0.04]
#>      [0.00, 0.00]
#>      [0.00, 0.00]
#>      [0.00, 0.00]
#>      [0.00, 0.00]
#>      [0.00, 0.00]
#>      [0.00, 0.00]
#>      [0.00, 0.00]
#>      [0.00, 0.00]
#>      [0.00, 0.00]
```

The main issue of the model seems to be the high
[multicollinearity](https://en.wikipedia.org/wiki/Multicollinearity).
This suggests that our model might not be able to give valid results
about any individual predictor, nor tell which predictors are redundant
with respect to others.

### Parameters selection

Time to do some variables selection! This can be easily done using the
[`select_parameters()`](https://easystats.github.io/parameters/reference/select_parameters.md)
function in `parameters`. It will **automatically** select the best
variables and update the model accordingly. One way of using that is in
a tidy pipeline (using
[`%>%`](https://cran.r-project.org/package=magrittr/)), using this
output to update a new model.

``` r

library(parameters)
lm(Sepal.Length ~ . * ., data = iris) |>
  select_parameters() |>
  summary()
#> 
#> Call:
#> lm(formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + 
#>     Species + Sepal.Width:Petal.Width + Petal.Length:Species + 
#>     Petal.Width:Species, data = iris)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -0.7261 -0.2165  0.0021  0.2191  0.7439 
#> 
#> Coefficients:
#>                                Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)                       2.090      0.528    3.95  0.00012 ***
#> Sepal.Width                       0.734      0.130    5.66  8.3e-08 ***
#> Petal.Length                      0.232      0.260    0.89  0.37310    
#> Petal.Width                       1.051      0.532    1.98  0.04993 *  
#> Speciesversicolor                -1.047      0.547   -1.92  0.05754 .  
#> Speciesvirginica                 -2.682      0.638   -4.21  4.6e-05 ***
#> Sepal.Width:Petal.Width          -0.232      0.103   -2.24  0.02667 *  
#> Petal.Length:Speciesversicolor    0.660      0.298    2.22  0.02837 *  
#> Petal.Length:Speciesvirginica     0.720      0.273    2.63  0.00941 ** 
#> Petal.Width:Speciesversicolor    -1.112      0.550   -2.02  0.04528 *  
#> Petal.Width:Speciesvirginica     -0.499      0.460   -1.09  0.27934    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.3 on 139 degrees of freedom
#> Multiple R-squared:  0.88,   Adjusted R-squared:  0.872 
#> F-statistic:  102 on 10 and 139 DF,  p-value: <2e-16
```

That’s still a lot of parameters, but as you can see, almost all of them
are now significant, and the R^2 did not change much.

Although appealing, please note that these automated selection methods
are [**quite
criticized**](https://www.lexjansen.com/nesug/nesug09/sa/SA01.pdf), and
should not be used in place of **theoretical** or **hypothetical**
reasons (*i.e.*, you should have *a priori* hypotheses about which
parameters of your model you want to focus on).

## Mixed models

For simple linear regressions as above, the selection is made using the
[`step()`](https://rdrr.io/r/stats/step.html) function (available in
base R). This performs a
[**stepwise**](https://en.wikipedia.org/wiki/Stepwise_regression)
selection. However, this procedures is not available for other types of
models, such as **mixed** models.

### Mixed models

For mixed models (of class `merMod`), stepwise selection is based on
[`cAIC4::stepcAIC()`](https://rdrr.io/pkg/cAIC4/man/stepcAIC.html). This
step function only searches the “best” model based on the *random
effects structure*,
i.e. [`select_parameters()`](https://easystats.github.io/parameters/reference/select_parameters.md)
adds or excludes random effects until the `cAIC` can’t be improved
further.

This is what our initial model looks like.

``` r

library(lme4)
data("qol_cancer")

# initial model
lmer(
  QoL ~ time + phq4 + age + (1 + time | hospital / ID),
  data = qol_cancer
) |>
  summary()
#> Linear mixed model fit by REML ['lmerMod']
#> Formula: QoL ~ time + phq4 + age + (1 + time | hospital/ID)
#>    Data: qol_cancer
#> 
#> REML criterion at convergence: 4647
#> 
#> Scaled residuals: 
#>    Min     1Q Median     3Q    Max 
#> -3.581 -0.393  0.082  0.507  2.505 
#> 
#> Random effects:
#>  Groups      Name        Variance Std.Dev. Corr 
#>  ID:hospital (Intercept) 202.49   14.23         
#>              time         12.54    3.54    -0.72
#>  hospital    (Intercept)   8.47    2.91         
#>              time          1.27    1.13    -1.00
#>  Residual                143.11   11.96         
#> Number of obs: 564, groups:  ID:hospital, 188; hospital, 2
#> 
#> Fixed effects:
#>             Estimate Std. Error t value
#> (Intercept)  70.6514     3.1279   22.59
#> time          1.4896     1.2194    1.22
#> phq4         -4.7687     0.3235  -14.74
#> age          -0.0173     0.1889   -0.09
#> 
#> Correlation of Fixed Effects:
#>      (Intr) time   phq4  
#> time -0.954              
#> phq4  0.014 -0.011       
#> age  -0.017  0.003  0.107
#> optimizer (nloptwrap) convergence code: 0 (OK)
#> boundary (singular) fit: see help('isSingular')
```

This is the model selected by
[`select_parameters()`](https://easystats.github.io/parameters/reference/select_parameters.md).
Please notice the differences in the random effects structure between
the initial and the selected models:

``` r

## TODO: this is currently broken due to an issue in package cAIC4

# multiple models are checked, however, initial models
# already seems to be the best one...
lmer(
  QoL ~ time + phq4 + age + (1 + time | hospital / ID),
  data = qol_cancer
) |>
  select_parameters() |>
  summary()
```

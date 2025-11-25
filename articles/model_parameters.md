# Summary of Model Parameters

The
[`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
function (also accessible via the shortcut
[`parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md))
allows you to extract the parameters and their characteristics from
various models in a consistent way. It can be considered as a
lightweight alternative to
[`broom::tidy()`](https://github.com/tidymodels/broom), with some
notable differences:

- The names of the returned data frame are **specific** to their
  content. For instance, the column containing the statistic is named
  following the statistic name, i.e., *t*, *z*, etc., instead of a
  generic name such as *statistic* (**however**, you can get
  standardized (generic) column names using
  [`standardize_names()`](https://easystats.github.io/insight/reference/standardize_names.html)).

- It is able to compute or extract indices not available by default,
  such as \*\*p\*-values**,** CIs\*\*, etc.

- It includes **feature engineering** capabilities, including parameters
  [**bootstrapping**](https://easystats.github.io/parameters/reference/bootstrap_model.html).

## Correlations and *t*-tests

### Frequentist

``` r

library(parameters)
cor.test(iris$Sepal.Length, iris$Sepal.Width) |>
  parameters()
#> Pearson's product-moment correlation
#> 
#> Parameter1        |       Parameter2 |     r |        95% CI | t(148) |     p
#> -----------------------------------------------------------------------------
#> iris$Sepal.Length | iris$Sepal.Width | -0.12 | [-0.27, 0.04] |  -1.44 | 0.152
#> 
#> Alternative hypothesis: true correlation is not equal to 0
```

``` r

t.test(mpg ~ vs, data = mtcars) |>
  parameters()
#> Welch Two Sample t-test
#> 
#> Parameter | Group | vs = 0 | vs = 1 | Difference |          95% CI | t(22.72) |      p
#> --------------------------------------------------------------------------------------
#> mpg       |    vs |  16.62 |  24.56 |      -7.94 | [-11.46, -4.42] |    -4.67 | < .001
#> 
#> Alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
```

### Bayesian

``` r

BayesFactor::correlationBF(iris$Sepal.Length, iris$Sepal.Width) |>
  parameters()
#> Bayesian correlation analysis
#> 
#> Parameter | Median |        95% CI |     pd |         Prior |    BF
#> -------------------------------------------------------------------
#> rho       |  -0.11 | [-0.27, 0.04] | 92.25% | Beta (3 +- 3) | 0.509
```

``` r

BayesFactor::ttestBF(formula = mpg ~ vs, data = mtcars) |>
  parameters()
#> Bayesian t-test
#> 
#> Parameter  | Median |          95% CI |     pd |              Prior |     BF
#> ----------------------------------------------------------------------------
#> Difference |  -7.31 | [-10.81, -3.79] | 99.98% | Cauchy (0 +- 0.71) | 529.27
```

## ANOVAs

Indices of effect size for ANOVAs, such as partial and non-partial
versions of `eta_squared()`, `epsilon_sqared()` or `omega_squared()` are
powered by the
[**effectsize**-package](https://easystats.github.io/effectsize/).
However, **parameters** uses these function to compute such indices for
parameters summaries, including confidence intervals

### Simple

``` r

aov(Sepal.Length ~ Species, data = iris) |>
  parameters(es_type = c("omega", "eta", "epsilon"))
#> Parameter | Sum_Squares |  df | Mean_Square |      F |      p | Omega2 | Eta2 | Epsilon2
#> ----------------------------------------------------------------------------------------
#> Species   |       63.21 |   2 |       31.61 | 119.26 | < .001 |   0.61 | 0.62 |     0.61
#> Residuals |       38.96 | 147 |        0.27 |        |        |        |      |         
#> 
#> Anova Table (Type 1 tests)
```

Let’s complicate things further with an interaction term:

``` r

aov(Sepal.Length ~ Species * Sepal.Width, data = iris) |>
  parameters(
    es_type = c("omega", "eta"),
    ci = 0.8
  )
#> Parameter           | Sum_Squares |  df | Mean_Square |      F |      p
#> -----------------------------------------------------------------------
#> Species             |       63.21 |   2 |       31.61 | 163.44 | < .001
#> Sepal.Width         |       10.95 |   1 |       10.95 |  56.64 | < .001
#> Species:Sepal.Width |        0.16 |   2 |        0.08 |   0.41 | 0.667 
#> Residuals           |       27.85 | 144 |        0.19 |        |       
#> 
#> Parameter           | Omega2 (partial) | Omega2 80% CI | Eta2 (partial) |  Eta2 80% CI
#> --------------------------------------------------------------------------------------
#> Species             |             0.68 |  [0.65, 1.00] |           0.69 | [0.66, 1.00]
#> Sepal.Width         |             0.27 |  [0.22, 1.00] |           0.28 | [0.23, 1.00]
#> Species:Sepal.Width |             0.00 |  [0.00, 1.00] |       5.61e-03 | [0.00, 1.00]
#> Residuals           |                  |               |                |             
#> 
#> Anova Table (Type 1 tests)
```

### Repeated measures

[`parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
(resp. its alias
[`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md))
also works on repeated measures ANOVAs, whether computed from
[`aov()`](https://rdrr.io/r/stats/aov.html) or from a mixed model.

``` r

aov(mpg ~ am + Error(gear), data = mtcars) |>
  parameters()
#> # gear
#> 
#> Parameter | Sum_Squares | df | Mean_Square
#> ------------------------------------------
#> am        |      259.75 |  1 |      259.75
#> 
#> # Within
#> 
#> Parameter | Sum_Squares | df | Mean_Square |    F |     p
#> ---------------------------------------------------------
#> am        |      145.45 |  1 |      145.45 | 5.85 | 0.022
#> Residuals |      720.85 | 29 |       24.86 |      |      
#> 
#> Anova Table (Type 1 tests)
```

## Regressions (GLMs, Mixed Models, GAMs, …)

[`parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
(resp. its alias
[`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md))
was mainly built with regression models in mind. It works for many types
of models and packages, including mixed models and Bayesian models.

### GLMs

``` r

glm(vs ~ poly(mpg, 2) + cyl, data = mtcars, family = binomial()) |>
  parameters()
#> Parameter        | Log-Odds |   SE |          95% CI |     z |     p
#> --------------------------------------------------------------------
#> (Intercept)      |    13.51 | 7.20 | [  2.56, 33.42] |  1.88 | 0.060
#> mpg [1st degree] |    -6.64 | 8.99 | [-27.81, 11.13] | -0.74 | 0.461
#> mpg [2nd degree] |     1.16 | 3.59 | [ -7.91,  8.56] |  0.32 | 0.746
#> cyl              |    -2.28 | 1.18 | [ -5.58, -0.51] | -1.92 | 0.055
```

``` r

# show Odds Ratios and Wald-method for degrees of freedom
glm(vs ~ poly(mpg, 2) + cyl, data = mtcars, family = binomial()) |>
  parameters(exponentiate = TRUE, ci_method = "wald")
#> Parameter        | Odds Ratio |       SE |           95% CI |     z |     p
#> ---------------------------------------------------------------------------
#> (Intercept)      |   7.38e+05 | 5.31e+06 | [0.55, 9.87e+11] |  1.88 | 0.060
#> mpg [1st degree] |   1.31e-03 |     0.01 | [0.00, 59497.56] | -0.74 | 0.461
#> mpg [2nd degree] |       3.20 |    11.49 | [0.00,  3637.30] |  0.32 | 0.746
#> cyl              |       0.10 |     0.12 | [0.01,     1.05] | -1.92 | 0.055
```

``` r

# show Odds Ratios and include model info
glm(vs ~ poly(mpg, 2) + cyl, data = mtcars, family = binomial()) |>
  parameters(exponentiate = TRUE, include_info = TRUE)
#> Parameter        | Odds Ratio |       SE |            95% CI |     z |     p
#> ----------------------------------------------------------------------------
#> (Intercept)      |   7.38e+05 | 5.31e+06 | [12.95, 3.26e+14] |  1.88 | 0.060
#> mpg [1st degree] |   1.31e-03 |     0.01 | [ 0.00, 68459.83] | -0.74 | 0.461
#> mpg [2nd degree] |       3.20 |    11.49 | [ 0.00,  5212.21] |  0.32 | 0.746
#> cyl              |       0.10 |     0.12 | [ 0.00,     0.60] | -1.92 | 0.055
#> 
#> Model: vs ~ poly(mpg, 2) + cyl (32 Observations)
#> Sigma: 1.000 (df = 28)
#> RMSE : 0.283
#> Tjur's R2: 0.670
```

### Mixed Models

``` r

library(lme4)

lmer(Sepal.Width ~ Petal.Length + (1 | Species), data = iris) |>
  parameters()
#> # Fixed Effects
#> 
#> Parameter    | Coefficient |   SE |       95% CI | t(146) |      p
#> ------------------------------------------------------------------
#> (Intercept)  |        2.00 | 0.56 | [0.89, 3.11] |   3.56 | < .001
#> Petal Length |        0.28 | 0.06 | [0.16, 0.40] |   4.75 | < .001
#> 
#> # Random Effects
#> 
#> Parameter               | Coefficient |   SE |       95% CI
#> -----------------------------------------------------------
#> SD (Intercept: Species) |        0.89 | 0.46 | [0.33, 2.43]
#> SD (Residual)           |        0.32 | 0.02 | [0.28, 0.35]
```

### Mixed Models, without Random Effects Variances

``` r

lmer(Sepal.Width ~ Petal.Length + (1 | Species), data = iris) |>
  parameters(effects = "fixed")
#> # Fixed Effects
#> 
#> Parameter    | Coefficient |   SE |       95% CI | t(146) |      p
#> ------------------------------------------------------------------
#> (Intercept)  |        2.00 | 0.56 | [0.89, 3.11] |   3.56 | < .001
#> Petal Length |        0.28 | 0.06 | [0.16, 0.40] |   4.75 | < .001
```

### Mixed Model with Zero-Inflation Model

``` r

library(GLMMadaptive)
library(glmmTMB)
data("Salamanders")
model <- mixed_model(
  count ~ spp + mined,
  random = ~ 1 | site,
  zi_fixed = ~ spp + mined,
  family = zi.negative.binomial(),
  data = Salamanders
)
parameters(model)
#> # Fixed Effects (Count Model)
#> 
#> Parameter   | Log-Mean |   SE |        95% CI |     z |      p
#> --------------------------------------------------------------
#> (Intercept) |    -0.63 | 0.40 | [-1.42, 0.16] | -1.56 | 0.118 
#> spp [PR]    |    -0.99 | 0.70 | [-2.35, 0.38] | -1.41 | 0.157 
#> spp [DM]    |     0.17 | 0.24 | [-0.29, 0.63] |  0.72 | 0.469 
#> spp [EC-A]  |    -0.39 | 0.35 | [-1.07, 0.29] | -1.13 | 0.258 
#> spp [EC-L]  |     0.49 | 0.24 | [ 0.02, 0.96] |  2.03 | 0.043 
#> spp [DES-L] |     0.59 | 0.23 | [ 0.14, 1.04] |  2.57 | 0.010 
#> spp [DF]    |    -0.11 | 0.24 | [-0.59, 0.37] | -0.46 | 0.642 
#> mined [no]  |     1.45 | 0.37 | [ 0.73, 2.17] |  3.95 | < .001
#> 
#> # Fixed Effects (Zero-Inflation Component)
#> 
#> Parameter   | Log-Odds |   SE |         95% CI |     z |      p
#> ---------------------------------------------------------------
#> (Intercept) |     0.90 | 0.64 | [-0.35,  2.15] |  1.41 | 0.159 
#> spp [PR]    |     1.12 | 1.50 | [-1.82,  4.06] |  0.74 | 0.456 
#> spp [DM]    |    -0.95 | 0.82 | [-2.56,  0.65] | -1.17 | 0.244 
#> spp [EC-A]  |     1.04 | 0.72 | [-0.38,  2.46] |  1.44 | 0.150 
#> spp [EC-L]  |    -0.58 | 0.74 | [-2.03,  0.88] | -0.77 | 0.439 
#> spp [DES-L] |    -0.91 | 0.78 | [-2.43,  0.61] | -1.18 | 0.239 
#> spp [DF]    |    -2.63 | 2.37 | [-7.27,  2.02] | -1.11 | 0.268 
#> mined [no]  |    -2.56 | 0.63 | [-3.80, -1.32] | -4.06 | < .001
#> 
#> # Random Effects Variances
#> 
#> Parameter            | Coefficient
#> ----------------------------------
#> SD (Intercept: site) |        0.39
#> SD (Residual)        |        1.62
```

### Mixed Models with Dispersion Model

``` r

library(glmmTMB)

sim1 <- function(nfac = 40, nt = 100, facsd = 0.1, tsd = 0.15, mu = 0, residsd = 1) {
  dat <- expand.grid(fac = factor(letters[1:nfac]), t = 1:nt)
  n <- nrow(dat)
  dat$REfac <- rnorm(nfac, sd = facsd)[dat$fac]
  dat$REt <- rnorm(nt, sd = tsd)[dat$t]
  dat$x <- rnorm(n, mean = mu, sd = residsd) + dat$REfac + dat$REt
  dat
}

set.seed(101)
d1 <- sim1(mu = 100, residsd = 10)
d2 <- sim1(mu = 200, residsd = 5)
d1$sd <- "ten"
d2$sd <- "five"
dat <- rbind(d1, d2)
model <- glmmTMB(x ~ sd + (1 | t), dispformula = ~sd, data = dat)

parameters(model)
#> # Fixed Effects
#> 
#> Parameter   | Coefficient |   SE |            95% CI |       z |      p
#> -----------------------------------------------------------------------
#> (Intercept) |      200.03 | 0.10 | [ 199.84, 200.22] | 2056.35 | < .001
#> sd [ten]    |      -99.71 | 0.22 | [-100.14, -99.29] | -458.39 | < .001
#> 
#> # Dispersion
#> 
#> Parameter   | Coefficient |   SE |       95% CI |      z |      p
#> -----------------------------------------------------------------
#> (Intercept) |        1.60 | 0.01 | [1.57, 1.63] | 115.48 | < .001
#> sd [ten]    |        0.69 | 0.02 | [0.65, 0.73] |  35.35 | < .001
#> 
#> # Random Effects Variances
#> 
#> Parameter         | Coefficient
#> -------------------------------
#> SD (Intercept: t) |    4.60e-04
#> SD (Residual)     |
```

### Bayesian Models

[`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
also works with Bayesian models from the **rstanarm** package:

``` r

library(rstanarm)

# if you are unfamiliar with the `refresh` argument here, it just avoids
# printing few messages to the console
stan_glm(mpg ~ wt * cyl, data = mtcars, refresh = 0) |>
  parameters()
#> Parameter   | Median |          95% CI |     pd |  Rhat |  ESS |                   Prior
#> ----------------------------------------------------------------------------------------
#> (Intercept) |  52.86 | [ 40.98, 63.96] |   100% | 1.003 | 1048 | Normal (20.09 +- 15.07)
#> wt          |  -8.09 | [-12.44, -3.72] | 99.88% | 1.002 | 1256 |  Normal (0.00 +- 15.40)
#> cyl         |  -3.58 | [ -5.47, -1.63] | 99.98% | 1.002 | 1120 |   Normal (0.00 +- 8.44)
#> wt:cyl      |   0.73 | [  0.11,  1.33] | 98.58% | 1.003 | 1068 |   Normal (0.00 +- 1.36)
```

Additionally, it also works for models from the **brms** package.

For more complex models, specific model components can be printed using
the arguments `effects` and `component` arguments.

``` r

library(brms)
data(fish)
set.seed(123)

# fitting a model using `brms`
model <- suppressWarnings(brm(
  bf(
    count ~ persons + child + camper + (1 | persons),
    zi ~ child + camper + (1 | persons)
  ),
  data = fish,
  family = zero_inflated_poisson(),
  refresh = 0
))
#> Running /opt/R/4.5.2/lib/R/bin/R CMD SHLIB foo.c
#> using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
#> gcc -std=gnu2x -I"/opt/R/4.5.2/lib/R/include" -DNDEBUG   -I"/home/runner/work/_temp/Library/Rcpp/include/"  -I"/home/runner/work/_temp/Library/RcppEigen/include/"  -I"/home/runner/work/_temp/Library/RcppEigen/include/unsupported"  -I"/home/runner/work/_temp/Library/BH/include" -I"/home/runner/work/_temp/Library/StanHeaders/include/src/"  -I"/home/runner/work/_temp/Library/StanHeaders/include/"  -I"/home/runner/work/_temp/Library/RcppParallel/include/"  -I"/home/runner/work/_temp/Library/rstan/include" -DEIGEN_NO_DEBUG  -DBOOST_DISABLE_ASSERTS  -DBOOST_PENDING_INTEGER_LOG2_HPP  -DSTAN_THREADS  -DUSE_STANC3 -DSTRICT_R_HEADERS  -DBOOST_PHOENIX_NO_VARIADIC_EXPRESSION  -D_HAS_AUTO_PTR_ETC=0  -include '/home/runner/work/_temp/Library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp'  -D_REENTRANT -DRCPP_PARALLEL_USE_TBB=1   -I/usr/local/include    -fpic  -g -O2  -c foo.c -o foo.o
#> In file included from /home/runner/work/_temp/Library/RcppEigen/include/Eigen/Core:19,
#>                  from /home/runner/work/_temp/Library/RcppEigen/include/Eigen/Dense:1,
#>                  from /home/runner/work/_temp/Library/StanHeaders/include/stan/math/prim/fun/Eigen.hpp:22,
#>                  from <command-line>:
#> /home/runner/work/_temp/Library/RcppEigen/include/Eigen/src/Core/util/Macros.h:679:10: fatal error: cmath: No such file or directory
#>   679 | #include <cmath>
#>       |          ^~~~~~~
#> compilation terminated.
#> make: *** [/opt/R/4.5.2/lib/R/etc/Makeconf:202: foo.o] Error 1

parameters(model, component = "conditional", verbose = FALSE)
#> # Fixed Effects
#> 
#> Parameter   | Median |         95% CI |     pd |  Rhat |  ESS
#> -------------------------------------------------------------
#> (Intercept) |  -0.85 | [-1.68,  0.24] | 96.30% | 1.036 |  188
#> persons     |   0.85 | [ 0.49,  1.12] |   100% | 1.076 |   78
#> child       |  -1.15 | [-1.34, -0.97] |   100% | 1.000 | 2078
#> camper1     |   0.74 | [ 0.55,  0.97] |   100% | 1.061 |   62

parameters(model, effects = "all", component = "all", verbose = FALSE)
#> # Fixed Effects
#> 
#> Parameter   | Median |         95% CI |     pd |  Rhat |  ESS
#> -------------------------------------------------------------
#> (Intercept) |  -0.85 | [-1.68,  0.24] | 96.30% | 1.036 |  188
#> persons     |   0.85 | [ 0.49,  1.12] |   100% | 1.076 |   78
#> child       |  -1.15 | [-1.34, -0.97] |   100% | 1.000 | 2078
#> camper1     |   0.74 | [ 0.55,  0.97] |   100% | 1.061 |   62
#> 
#> # Zero-Inflation Parameters
#> 
#> Parameter   | Median |         95% CI |     pd |  Rhat | ESS
#> ------------------------------------------------------------
#> (Intercept) |  -0.71 | [-2.11,  0.74] | 84.92% | 1.006 | 612
#> child       |   1.89 | [ 1.26,  2.48] |   100% | 1.031 | 134
#> camper1     |  -0.78 | [-1.52, -0.10] | 98.40% | 1.031 | 130
#> 
#> # Random Effects Variances
#> 
#> Parameter               | Median |       95% CI |   pd |  Rhat | ESS
#> --------------------------------------------------------------------
#> SD (Intercept: persons) |   0.15 | [0.01, 0.90] | 100% | 1.152 |  31
#> 
#> # Zero-Inflation Random Effects
#> 
#> Parameter                  | Median |       95% CI |   pd |  Rhat | ESS
#> -----------------------------------------------------------------------
#> SD (zi_Intercept: persons) |   1.25 | [0.52, 3.46] | 100% | 1.017 | 727
```

To include information about the random effect parameters (group
levels), set `group_level = TRUE`:

``` r

parameters(
  model,
  effects = "all",
  component = "conditional",
  group_level = TRUE,
  verbose = FALSE
)
#> # Fixed Effects
#> 
#> Parameter   | Median |         95% CI |     pd |  Rhat |  ESS
#> -------------------------------------------------------------
#> (Intercept) |  -0.85 | [-1.68,  0.24] | 96.30% | 1.036 |  188
#> persons     |   0.85 | [ 0.49,  1.12] |   100% | 1.076 |   78
#> child       |  -1.15 | [-1.34, -0.97] |   100% | 1.000 | 2078
#> camper1     |   0.74 | [ 0.55,  0.97] |   100% | 1.061 |   62
#> 
#> # Random Effects: persons
#> 
#> Parameter   | Median |       95% CI |   pd |  Rhat | ESS
#> --------------------------------------------------------
#> (Intercept) |   0.15 | [0.01, 0.90] | 100% | 1.152 |  31
```

## Structural Models (PCA, EFA, CFA, SEM…)

The **parameters** package extends the support to structural models.

### Principal Component Analysis (PCA) and Exploratory Factor Analysis (EFA)

``` r

psych::pca(mtcars, nfactors = 3) |>
  parameters()
#> # Rotated loadings from Principal Component Analysis (varimax-rotation)
#> 
#> Variable |   RC2 |   RC3 |   RC1 | Complexity | Uniqueness
#> ----------------------------------------------------------
#> mpg      |  0.66 | -0.41 | -0.54 |       2.63 |       0.10
#> cyl      | -0.62 |  0.67 |  0.34 |       2.49 |       0.05
#> disp     | -0.72 |  0.52 |  0.35 |       2.33 |       0.10
#> hp       | -0.30 |  0.64 |  0.63 |       2.40 |       0.10
#> drat     |  0.85 | -0.26 | -0.05 |       1.19 |       0.21
#> wt       | -0.78 |  0.21 |  0.51 |       1.90 |       0.08
#> qsec     | -0.18 | -0.91 | -0.28 |       1.28 |       0.06
#> vs       |  0.28 | -0.86 | -0.23 |       1.36 |       0.12
#> am       |  0.92 |  0.14 | -0.11 |       1.08 |       0.12
#> gear     |  0.91 | -0.02 |  0.26 |       1.16 |       0.10
#> carb     |  0.11 |  0.44 |  0.85 |       1.53 |       0.07
#> 
#> The 3 principal components (varimax rotation) accounted for 89.87% of the total variance of the original data (RC2 = 41.43%, RC3 = 29.06%, RC1 = 19.39%).
```

We will avoid displaying a graph while carrying out factor analysis:

``` r

FactoMineR::FAMD(iris, ncp = 3, graph = FALSE) |>
  parameters()
#> # Loadings from Factor Analysis (no rotation)
#> 
#> Variable     | Dim.1 |    Dim.2 |    Dim.3 | Complexity
#> -------------------------------------------------------
#> Sepal.Length |  0.75 |     0.07 |     0.10 |       1.05
#> Sepal.Width  |  0.23 |     0.51 |     0.23 |       1.86
#> Petal.Length |  0.98 | 1.32e-03 | 1.99e-03 |       1.00
#> Petal.Width  |  0.94 |     0.01 | 2.82e-05 |       1.00
#> Species      |  0.96 |     0.75 |     0.26 |       2.05
#> 
#> The 3 latent factors accounted for 96.73% of the total variance of the original data (Dim.1 = 64.50%, Dim.2 = 22.37%, Dim.3 = 9.86%).
```

### Confirmatory Factor Analysis (CFA) and Structural Equation Models (SEM)

#### Frequentist

``` r

library(lavaan)

model <- lavaan::cfa(" visual  =~ x1 + x2 + x3
                       textual =~ x4 + x5 + x6
                       speed   =~ x7 + x8 + x9 ",
  data = HolzingerSwineford1939
)

model_parameters(model)
#> # Loading
#> 
#> Link          | Coefficient |   SE |       95% CI |     z |      p
#> ------------------------------------------------------------------
#> visual =~ x1  |        1.00 | 0.00 | [1.00, 1.00] |       | < .001
#> visual =~ x2  |        0.55 | 0.10 | [0.36, 0.75] |  5.55 | < .001
#> visual =~ x3  |        0.73 | 0.11 | [0.52, 0.94] |  6.68 | < .001
#> textual =~ x4 |        1.00 | 0.00 | [1.00, 1.00] |       | < .001
#> textual =~ x5 |        1.11 | 0.07 | [0.98, 1.24] | 17.01 | < .001
#> textual =~ x6 |        0.93 | 0.06 | [0.82, 1.03] | 16.70 | < .001
#> speed =~ x7   |        1.00 | 0.00 | [1.00, 1.00] |       | < .001
#> speed =~ x8   |        1.18 | 0.16 | [0.86, 1.50] |  7.15 | < .001
#> speed =~ x9   |        1.08 | 0.15 | [0.79, 1.38] |  7.15 | < .001
#> 
#> # Correlation
#> 
#> Link              | Coefficient |   SE |       95% CI |    z |      p
#> ---------------------------------------------------------------------
#> visual ~~ textual |        0.41 | 0.07 | [0.26, 0.55] | 5.55 | < .001
#> visual ~~ speed   |        0.26 | 0.06 | [0.15, 0.37] | 4.66 | < .001
#> textual ~~ speed  |        0.17 | 0.05 | [0.08, 0.27] | 3.52 | < .001
```

#### Bayesian

`blavaan` to be done.

## Meta-Analysis

[`parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
also works for `rma`-objects from the **metafor** package.

``` r

library(metafor)

mydat <<- data.frame(
  effectsize = c(-0.393, 0.675, 0.282, -1.398),
  standarderror = c(0.317, 0.317, 0.13, 0.36)
)

rma(yi = effectsize, sei = standarderror, method = "REML", data = mydat) |>
  model_parameters()
#> Meta-analysis using 'metafor'
#> 
#> Parameter | Coefficient |   SE |         95% CI |     z |      p | Weight
#> -------------------------------------------------------------------------
#> Study 1   |       -0.39 | 0.32 | [-1.01,  0.23] | -1.24 | 0.215  |   9.95
#> Study 2   |        0.68 | 0.32 | [ 0.05,  1.30] |  2.13 | 0.033  |   9.95
#> Study 3   |        0.28 | 0.13 | [ 0.03,  0.54] |  2.17 | 0.030  |  59.17
#> Study 4   |       -1.40 | 0.36 | [-2.10, -0.69] | -3.88 | < .001 |   7.72
#> Overall   |       -0.18 | 0.44 | [-1.05,  0.68] | -0.42 | 0.676  |
```

## Plotting Model Parameters

There is a
[`plot()`](https://rdrr.io/r/graphics/plot.default.html)-method
implemented in the [**see**-package](https://easystats.github.io/see/).
Several examples are shown [in this
vignette](https://easystats.github.io/see/articles/parameters.html).

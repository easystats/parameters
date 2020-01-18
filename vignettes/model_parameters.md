Parameters Description
================

  - [Correlations and *t*-tests](#correlations-and-t-tests)
      - [Frequentist](#frequentist)
      - [Bayesian](#bayesian)
  - [ANOVAs](#anovas)
      - [Simple](#simple)
      - [Repeated measures](#repeated-measures)
  - [Regressions (GLMs, Mixed Models, GAMs,
    …)](#regressions-glms-mixed-models-gams)
      - [GLMs](#glms)
      - [Mixed Models](#mixed-models)
      - [Bayesian Models](#bayesian-models)
  - [Structural Models (PCA, EFA, CFA,
    SEM…)](#structural-models-pca-efa-cfa-sem)
      - [Principal Component Analysis (PCA) and Exploratory Factor
        Analysis
        (EFA)](#principal-component-analysis-pca-and-exploratory-factor-analysis-efa)
      - [Confirmatory Factor Analysis (CFA) and Structural Equation
        Models
        (SEM)](#confirmatory-factor-analysis-cfa-and-structural-equation-models-sem)
  - [Meta-Analysis](#meta-analysis)

<img src="https://raw.githubusercontent.com/easystats/parameters/master/man/figures/figure1.png" style="display: block; margin: auto;" />

The `model_parameters()` function (also accessible via the shortcut
`parameters()`) allows you to extract the parameters and their
characteristics from various models in a consistent way. It can be
considered as a lightweight alternative to
[`broom::tidy()`](https://github.com/tidymodels/broom), with some
notable differences:

  - The names of the returned dataframe are **specific** to their
    content. For instance, the column containing the statistic is named
    following the statistic name, i.e., *t*, *z*, etc., instead of a
    generic name such as *statistic* (**however**, you can get
    standardized (generic) column names using
    [`standardize_names()`](https://easystats.github.io/parameters/reference/standardize_names.html)).
  - It is able to compute or extract indices not available by default,
    such as ***p*-values**, **CIs**, etc.
  - It includes **feature engineering** capabilities, including
    parameters
    [**bootstrapping**](https://easystats.github.io/parameters/articles/bootstrapping.html).

## Correlations and *t*-tests

### Frequentist

``` r
cor.test(iris$Sepal.Length, iris$Sepal.Width) %>% 
  parameters()
```

    > Parameter1        |       Parameter2 |     r |     t |  df |     p |        95% CI |  Method
    > --------------------------------------------------------------------------------------------
    > iris$Sepal.Length | iris$Sepal.Width | -0.12 | -1.44 | 148 | 0.152 | [-0.27, 0.04] | Pearson

``` r
t.test(mpg ~ vs, data = mtcars) %>% 
  parameters()
```

    > Parameter | Group | Mean_Group1 | Mean_Group2 | Difference |     t |    df |      p |          95% CI |                  Method
    > -------------------------------------------------------------------------------------------------------------------------------
    > mpg       |    vs |       16.62 |       24.56 |       7.94 | -4.67 | 22.72 | < .001 | [-11.46, -4.42] | Welch Two Sample t-test

### Bayesian

``` r
library(BayesFactor)

BayesFactor::correlationBF(iris$Sepal.Length, iris$Sepal.Width) %>% 
  parameters()
```

    > Parameter | Median |        89% CI |     pd | % in ROPE |              Prior | Effects |   Component |   BF
    > -----------------------------------------------------------------------------------------------------------
    > rho       |  -0.11 | [-0.23, 0.02] | 92.90% |    43.13% | Cauchy (0 +- 0.33) |   fixed | conditional | 0.51

``` r
BayesFactor::ttestBF(formula = mpg ~ vs, data = mtcars) %>% 
  parameters()
```

    > Parameter  | Median |          89% CI |     pd | % in ROPE |              Prior | Effects |   Component |     BF
    > ----------------------------------------------------------------------------------------------------------------
    > Difference |  -7.30 | [-10.15, -4.58] | 99.98% |        0% | Cauchy (0 +- 0.71) |   fixed | conditional | 529.27

## ANOVAs

Indices of effect size for ANOVAs, such as partial and non-partial
versions of `eta_squared()`, `epsilon_sqared()` or `omega_squared()`,
were moved to the
[**effectsize**-package](https://easystats.github.io/effectsize/).
However, **parameters** uses these function to compute such indices for
parameters summaries.

### Simple

``` r
aov(Sepal.Length ~ Species, data = iris) %>% 
  parameters(omega_squared = "partial", eta_squared = "partial", epsilon_squared = TRUE)
```

    > Parameter | Sum_Squares |  df | Mean_Square |      F |      p | Omega_Sq (partial) | Eta_Sq (partial) | Epsilon_sq
    > ------------------------------------------------------------------------------------------------------------------
    > Species   |       63.21 |   2 |       31.61 | 119.26 | < .001 |               0.61 |             0.62 |       0.61
    > Residuals |       38.96 | 147 |        0.27 |        |        |                    |                  |

### Repeated measures

`parameters()` (resp. its alias `model_parameters()`) also works on
repeated measures ANOVAs, whether computed from `aov()` or from a mixed
model.

``` r
aov(mpg ~ am + Error(gear), data = mtcars) %>% 
  parameters()
```

    > Group  | Parameter | Sum_Squares | df | Mean_Square |    F |     p
    > ------------------------------------------------------------------
    > Within |        am |      145.45 |  1 |      145.45 | 5.85 | 0.022
    > Within | Residuals |      720.85 | 29 |       24.86 |      |      
    > gear   |        am |      259.75 |  1 |      259.75 |      |

## Regressions (GLMs, Mixed Models, GAMs, …)

`parameters()` (resp. its alias `model_parameters()`) was mainly built
with regression models in mind. It works for many types of models and
packages, including mixed models and Bayesian models.

### GLMs

``` r
glm(vs ~ poly(mpg, 2) + cyl, data = mtcars) %>% 
  parameters()
```

    > Parameter        | Coefficient |   SE |         95% CI |     t | df |      p
    > ----------------------------------------------------------------------------
    > (Intercept)      |        2.04 | 0.39 | [ 1.27,  2.80] |  5.22 | 28 | < .001
    > mpg [1st degree] |       -0.33 | 0.61 | [-1.53,  0.87] | -0.53 | 28 | 0.599 
    > mpg [2nd degree] |        0.10 | 0.32 | [-0.54,  0.74] |  0.31 | 28 | 0.762 
    > cyl              |       -0.26 | 0.06 | [-0.38, -0.14] | -4.14 | 28 | < .001

### Mixed Models

``` r
library(lme4)

lmer(Sepal.Width ~ Petal.Length + (1|Species), data = iris) %>% 
  parameters()
```

    > Parameter    | Coefficient |   SE |       95% CI |    t |  df |      p
    > ----------------------------------------------------------------------
    > (Intercept)  |        2.00 | 0.56 | [0.90, 3.10] | 3.56 | 146 | < .001
    > Petal.Length |        0.28 | 0.06 | [0.17, 0.40] | 4.75 | 146 | < .001

### Bayesian Models

`model_parameters()` works fine with Bayesian models from the
**rstanarm** package…

``` r
library(rstanarm)

stan_glm(mpg ~ wt * cyl, data = mtcars) %>% 
  parameters()
```

    > Parameter   | Median |          89% CI |     pd | % in ROPE |  Rhat | ESS |               Prior
    > -----------------------------------------------------------------------------------------------
    > (Intercept) |  53.16 | [ 42.36, 61.52] |   100% |        0% | 1.002 | 188 | Normal (0 +- 60.27)
    > wt          |  -8.19 | [-11.59, -4.40] |   100% |     0.20% | 1.006 | 184 | Normal (0 +- 15.40)
    > cyl         |  -3.71 | [ -4.88, -1.77] |   100% |     0.20% | 1.000 | 206 |  Normal (0 +- 8.44)
    > wt * cyl    |   0.76 | [  0.19,  1.25] | 98.40% |    32.00% | 1.004 | 179 |  Normal (0 +- 1.36)

… as well as for (more complex) models from the **brms** package. For
more complex models, other model components can be printed using the
arguments `effects` and `component` arguments.

``` r
# We download the model to save computation time. Here is the code
# to refit the exact model used below...

# zinb <- read.csv("http://stats.idre.ucla.edu/stat/data/fish.csv")
# set.seed(123)
# model <- brm(bf(
#     count ~ persons + child + camper + (1 | persons),
#     zi ~ child + camper + (1 | persons)
#   ),
#   data = zinb,
#   family = zero_inflated_poisson()
# )
model <- insight::download_model("brms_zi_2")
parameters(model, component = "conditional")
```

    > Parameter   | Median |         89% CI |     pd | % in ROPE |  ESS |  Rhat
    > -------------------------------------------------------------------------
    > b_Intercept |  -0.84 | [-1.44, -0.29] | 96.43% |     2.77% |  562 | 1.009
    > b_persons   |   0.84 | [ 0.66,  1.06] |   100% |        0% |  382 | 1.010
    > b_child     |  -1.15 | [-1.29, -0.98] |   100% |        0% | 1089 | 1.002
    > b_camper    |   0.73 | [ 0.58,  0.89] |   100% |        0% | 2724 | 1.000

``` r
parameters(model, effects = "all", component = "all")
```

    > # Fixed Effects (Count Model) 
    > 
    > Parameter   | Median |         89% CI |     pd | % in ROPE |  ESS |  Rhat
    > -------------------------------------------------------------------------
    > (Intercept) |  -0.84 | [-1.44, -0.29] | 96.43% |     2.77% |  562 | 1.009
    > persons     |   0.84 | [ 0.66,  1.06] |   100% |        0% |  382 | 1.010
    > child       |  -1.15 | [-1.29, -0.98] |   100% |        0% | 1089 | 1.002
    > camper      |   0.73 | [ 0.58,  0.89] |   100% |        0% | 2724 | 1.000
    > 
    > # Fixed Effects (Zero-Inflated Model) 
    > 
    > Parameter   | Median |         89% CI |     pd | % in ROPE |  ESS |  Rhat
    > -------------------------------------------------------------------------
    > (Intercept) |  -0.64 | [-1.93,  0.52] | 83.15% |     6.95% |  845 | 1.001
    > child       |   1.88 | [ 1.40,  2.43] |   100% |        0% | 2322 | 1.001
    > camper      |  -0.83 | [-1.41, -0.24] | 98.95% |     1.70% | 2277 | 1.001
    > 
    > # Random Effects (Count Model) 
    > 
    > Parameter | Median |        89% CI |     pd | % in ROPE | ESS |  Rhat
    > ---------------------------------------------------------------------
    > persons.1 |  -0.01 | [-0.38, 0.28] | 55.33% |    60.50% | 572 | 1.009
    > persons.2 |   0.02 | [-0.17, 0.30] | 61.88% |    65.62% | 691 | 1.008
    > persons.3 |  -0.02 | [-0.26, 0.18] | 61.27% |    67.90% | 340 | 1.011
    > persons.4 |   0.00 | [-0.32, 0.33] | 51.38% |    62.12% | 287 | 1.011
    > 
    > # Random Effects (Zero-Inflated Model) 
    > 
    > Parameter | Median |         89% CI |     pd | % in ROPE | ESS |  Rhat
    > ----------------------------------------------------------------------
    > persons.1 |   1.28 | [ 0.08,  2.70] | 95.73% |     2.15% | 811 | 1.001
    > persons.2 |   0.25 | [-0.90,  1.57] | 66.45% |    12.72% | 759 | 1.001
    > persons.3 |  -0.18 | [-1.51,  1.01] | 59.67% |    11.28% | 871 | 1.001
    > persons.4 |  -1.29 | [-2.62, -0.01] | 94.85% |     1.85% | 912 | 1.000

## Structural Models (PCA, EFA, CFA, SEM…)

The **parameters** package extends the support to structural models.

### Principal Component Analysis (PCA) and Exploratory Factor Analysis (EFA)

``` r
library(psych)

psych::pca(mtcars, nfactors = 3) %>% 
  parameters()
```

    > # Rotated loadings from Principal Component Analysis (varimax-rotation)
    > 
    > Variable |   RC2 |   RC3 |   RC1 | Complexity | Uniqueness
    > ----------------------------------------------------------
    > mpg      |  0.66 | -0.41 | -0.54 |       2.63 |       0.10
    > cyl      | -0.62 |  0.67 |  0.34 |       2.49 |       0.05
    > disp     | -0.72 |  0.52 |  0.35 |       2.33 |       0.10
    > hp       | -0.30 |  0.64 |  0.63 |       2.40 |       0.10
    > drat     |  0.85 | -0.26 | -0.05 |       1.19 |       0.21
    > wt       | -0.78 |  0.21 |  0.51 |       1.90 |       0.08
    > qsec     | -0.18 | -0.91 | -0.28 |       1.28 |       0.06
    > vs       |  0.28 | -0.86 | -0.23 |       1.36 |       0.12
    > am       |  0.92 |  0.14 | -0.11 |       1.08 |       0.12
    > gear     |  0.91 | -0.02 |  0.26 |       1.16 |       0.10
    > carb     |  0.11 |  0.44 |  0.85 |       1.53 |       0.07
    > 
    > The 3 principal components (varimax rotation) accounted for 89.87% of the total variance of the original data (RC2 = 41.43%, RC3 = 29.06%, RC1 = 19.39%).

``` r
library(FactoMineR)

FactoMineR::FAMD(iris, ncp = 3) %>% 
  parameters()
```

    > # Loadings from Principal Component Analysis (no rotation)
    > 
    > Variable     | Dim.1 | Dim.2 | Dim.3 | Complexity
    > -------------------------------------------------
    > Sepal.Length |  0.75 |  0.07 |  0.10 |       1.05
    > Sepal.Width  |  0.23 |  0.51 |  0.23 |       1.86
    > Petal.Length |  0.98 |  0.00 |  0.00 |       1.00
    > Petal.Width  |  0.94 |  0.01 |  0.00 |       1.00
    > Species      |  0.96 |  0.75 |  0.26 |       2.05
    > 
    > The 3 latent factors accounted for 96.73% of the total variance of the original data (Dim.1 = 64.50%, Dim.2 = 22.37%, Dim.3 = 9.86%).

### Confirmatory Factor Analysis (CFA) and Structural Equation Models (SEM)

#### Frequentist

``` r
library(lavaan)

model <- lavaan::cfa(' visual  =~ x1 + x2 + x3
                       textual =~ x4 + x5 + x6
                       speed   =~ x7 + x8 + x9 ', 
                       data = HolzingerSwineford1939)

model_parameters(model)
```

    > # Correlation type
    > 
    > Link              | Coefficient |   SE |       95% CI |      p
    > --------------------------------------------------------------
    > visual ~~ textual |        0.41 | 0.07 | [0.26, 0.55] | < .001
    > visual ~~ speed   |        0.26 | 0.06 | [0.15, 0.37] | < .001
    > textual ~~ speed  |        0.17 | 0.05 | [0.08, 0.27] | < .001
    > 
    > # Loading type
    > 
    > Link          | Coefficient |   SE |       95% CI |      p
    > ----------------------------------------------------------
    > visual =~ x1  |        1.00 | 0.00 | [1.00, 1.00] | < .001
    > visual =~ x2  |        0.55 | 0.10 | [0.36, 0.75] | < .001
    > visual =~ x3  |        0.73 | 0.11 | [0.52, 0.94] | < .001
    > textual =~ x4 |        1.00 | 0.00 | [1.00, 1.00] | < .001
    > textual =~ x5 |        1.11 | 0.07 | [0.98, 1.24] | < .001
    > textual =~ x6 |        0.93 | 0.06 | [0.82, 1.03] | < .001
    > speed =~ x7   |        1.00 | 0.00 | [1.00, 1.00] | < .001
    > speed =~ x8   |        1.18 | 0.16 | [0.86, 1.50] | < .001
    > speed =~ x9   |        1.08 | 0.15 | [0.79, 1.38] | < .001

#### Bayesian

`blavaan` to be done.

## Meta-Analysis

`parameters()` also works for `rma`-objects from the **metafor**
package.

``` r
library(metafor)

mydat <- data.frame(
  effectsize = c(-0.393, 0.675, 0.282, -1.398),
  standarderror = c(0.317, 0.317, 0.13, 0.36)
)

rma(yi = effectsize, sei = standarderror, method = "REML", data = mydat) %>% 
  model_parameters()
```

    > Parameter   | Coefficient |   SE |         95% CI |     z |      p | Weight
    > ---------------------------------------------------------------------------
    > Study 1     |       -0.39 | 0.32 | [-1.01,  0.23] | -1.24 | 0.215  |   9.95
    > Study 2     |        0.68 | 0.32 | [ 0.05,  1.30] |  2.13 | 0.033  |   9.95
    > Study 3     |        0.28 | 0.13 | [ 0.03,  0.54] |  2.17 | 0.030  |  59.17
    > Study 4     |       -1.40 | 0.36 | [-2.10, -0.69] | -3.88 | < .001 |   7.72
    > (Intercept) |       -0.18 | 0.44 | [-1.05,  0.68] | -0.42 | 0.676  |

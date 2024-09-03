
# parameters <img src="man/figures/logo.png" align="right" height="139" />

[![DOI](https://joss.theoj.org/papers/10.21105/joss.02445/status.svg)](https://doi.org/10.21105/joss.02445)
[![downloads](https://cranlogs.r-pkg.org/badges/parameters)](https://cran.r-project.org/package=parameters)
[![total](https://cranlogs.r-pkg.org/badges/grand-total/parameters)](https://cranlogs.r-pkg.org/)

***Describe and understand your model’s parameters!***

**parameters**’ primary goal is to provide utilities for processing the
parameters of various statistical models (see
[here](https://easystats.github.io/insight/) for a list of supported
models). Beyond computing *p-values*, *CIs*, *Bayesian indices* and
other measures for a wide variety of models, this package implements
features like *bootstrapping* of parameters and models, *feature
reduction* (feature extraction and variable selection), or tools for
data reduction like functions to perform cluster, factor or principal
component analysis.

Another important goal of the **parameters** package is to facilitate
and streamline the process of reporting results of statistical models,
which includes the easy and intuitive calculation of standardized
estimates or robust standard errors and p-values. **parameters**
therefor offers a simple and unified syntax to process a large variety
of (model) objects from many different packages.

## Installation

[![CRAN](https://www.r-pkg.org/badges/version/parameters)](https://cran.r-project.org/package=parameters)
[![parameters status
badge](https://easystats.r-universe.dev/badges/parameters)](https://easystats.r-universe.dev)
[![R-CMD-check](https://github.com/easystats/parameters/workflows/R-CMD-check/badge.svg?branch=main)](https://github.com/easystats/parameters/actions)

| Type | Source | Command |
|----|----|----|
| Release | CRAN | `install.packages("parameters")` |
| Development | r - universe | `install.packages("parameters", repos = "https://easystats.r-universe.dev")` |
| Development | GitHub | `remotes::install_github("easystats/parameters")` |

> **Tip**
>
> Instead of `library(parameters)`, use `library(easystats)`. This will
> make all features of the easystats-ecosystem available.
>
> To stay updated, use `easystats::install_latest()`.

## Documentation

[![Documentation](https://img.shields.io/badge/documentation-parameters-orange.svg?colorB=E91E63)](https://easystats.github.io/parameters/)
[![Blog](https://img.shields.io/badge/blog-easystats-orange.svg?colorB=FF9800)](https://easystats.github.io/blog/posts/)
[![Features](https://img.shields.io/badge/features-parameters-orange.svg?colorB=2196F3)](https://easystats.github.io/parameters/reference/index.html)

Click on the buttons above to access the package
[documentation](https://easystats.github.io/parameters/) and the
[easystats blog](https://easystats.github.io/blog/posts/), and check-out
these vignettes:

- [Summary of Model
  Parameters](https://easystats.github.io/parameters/articles/model_parameters.html)
- [Parameter and Model
  Standardization](https://easystats.github.io/parameters/articles/standardize_parameters_effsize.html)
- [Robust Estimation of Standard Errors, Confidence Intervals and
  p-values](https://easystats.github.io/parameters/articles/model_parameters_robust.html)
- [Model Parameters and Missing
  Data](https://easystats.github.io/parameters/articles/model_parameters_mice.html)
- [Feature reduction (PCA, cMDS,
  ICA…)](https://easystats.github.io/parameters/articles/parameters_reduction.html)
- [Structural models (EFA, CFA,
  SEM…)](https://easystats.github.io/parameters/articles/efa_cfa.html)
- [Parameters
  selection](https://easystats.github.io/parameters/articles/parameters_selection.html)
- [A Practical Guide for Panel Data
  Analysis](https://easystats.github.io/parameters/articles/demean.html)
- [Plotting
  functions](https://easystats.github.io/see/articles/parameters.html)

## Contributing and Support

In case you want to file an issue or contribute in another way to the
package, please follow [this
guide](https://github.com/easystats/parameters/blob/main/.github/CONTRIBUTING.md).
For questions about the functionality, you may either contact us via
email or also file an issue.

# Features

## Model’s parameters description

<img src="man/figures/figure1.png" width="100%" style="display: block; margin: auto;" />

The
[`model_parameters()`](https://easystats.github.io/parameters/articles/model_parameters.html)
function (that can be accessed via the `parameters()` shortcut) allows
you to extract the parameters and their characteristics from various
models in a consistent way. It can be considered as a lightweight
alternative to [`broom::tidy()`](https://github.com/tidymodels/broom),
with some notable differences:

- The column names of the returned data frame are *specific* to their
  content. For instance, the column containing the statistic is named
  following the statistic name, i.e., *t*, *z*, etc., instead of a
  generic name such as *statistic* (however, you can get standardized
  (generic) column names using
  [`standardize_names()`](https://easystats.github.io/insight/reference/standardize_names.html)).
- It is able to compute or extract indices not available by default,
  such as *p-values*, *CIs*, etc.
- It includes *feature engineering* capabilities, including parameters
  [bootstrapping](https://easystats.github.io/parameters/reference/bootstrap_parameters.html).

### Classical Regression Models

``` r
model <- lm(Sepal.Width ~ Petal.Length * Species + Petal.Width, data = iris)

# regular model parameters
model_parameters(model)
#> Parameter                           | Coefficient |   SE |         95% CI | t(143) |      p
#> -------------------------------------------------------------------------------------------
#> (Intercept)                         |        2.89 | 0.36 | [ 2.18,  3.60] |   8.01 | < .001
#> Petal Length                        |        0.26 | 0.25 | [-0.22,  0.75] |   1.07 | 0.287 
#> Species [versicolor]                |       -1.66 | 0.53 | [-2.71, -0.62] |  -3.14 | 0.002 
#> Species [virginica]                 |       -1.92 | 0.59 | [-3.08, -0.76] |  -3.28 | 0.001 
#> Petal Width                         |        0.62 | 0.14 | [ 0.34,  0.89] |   4.41 | < .001
#> Petal Length × Species [versicolor] |       -0.09 | 0.26 | [-0.61,  0.42] |  -0.36 | 0.721 
#> Petal Length × Species [virginica]  |       -0.13 | 0.26 | [-0.64,  0.38] |  -0.50 | 0.618

# standardized parameters
model_parameters(model, standardize = "refit")
#> Parameter                           | Coefficient |   SE |         95% CI | t(143) |      p
#> -------------------------------------------------------------------------------------------
#> (Intercept)                         |        3.59 | 1.30 | [ 1.01,  6.17] |   2.75 | 0.007 
#> Petal Length                        |        1.07 | 1.00 | [-0.91,  3.04] |   1.07 | 0.287 
#> Species [versicolor]                |       -4.62 | 1.31 | [-7.21, -2.03] |  -3.53 | < .001
#> Species [virginica]                 |       -5.51 | 1.38 | [-8.23, -2.79] |  -4.00 | < .001
#> Petal Width                         |        1.08 | 0.24 | [ 0.59,  1.56] |   4.41 | < .001
#> Petal Length × Species [versicolor] |       -0.38 | 1.06 | [-2.48,  1.72] |  -0.36 | 0.721 
#> Petal Length × Species [virginica]  |       -0.52 | 1.04 | [-2.58,  1.54] |  -0.50 | 0.618

# heteroscedasticity-consitent SE and CI
model_parameters(model, vcov = "HC3")
#> Parameter                           | Coefficient |   SE |         95% CI | t(143) |      p
#> -------------------------------------------------------------------------------------------
#> (Intercept)                         |        2.89 | 0.43 | [ 2.03,  3.75] |   6.66 | < .001
#> Petal Length                        |        0.26 | 0.29 | [-0.30,  0.83] |   0.92 | 0.357 
#> Species [versicolor]                |       -1.66 | 0.53 | [-2.70, -0.62] |  -3.16 | 0.002 
#> Species [virginica]                 |       -1.92 | 0.77 | [-3.43, -0.41] |  -2.51 | 0.013 
#> Petal Width                         |        0.62 | 0.12 | [ 0.38,  0.85] |   5.23 | < .001
#> Petal Length × Species [versicolor] |       -0.09 | 0.29 | [-0.67,  0.48] |  -0.32 | 0.748 
#> Petal Length × Species [virginica]  |       -0.13 | 0.31 | [-0.73,  0.48] |  -0.42 | 0.675
```

### Mixed Models

``` r
library(lme4)
model <- lmer(Sepal.Width ~ Petal.Length + (1 | Species), data = iris)

# model parameters with CI, df and p-values based on Wald approximation
model_parameters(model)
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

# model parameters with CI, df and p-values based on Kenward-Roger approximation
model_parameters(model, ci_method = "kenward", effects = "fixed")
#> # Fixed Effects
#> 
#> Parameter    | Coefficient |   SE |       95% CI |    t |     df |      p
#> -------------------------------------------------------------------------
#> (Intercept)  |        2.00 | 0.57 | [0.07, 3.93] | 3.53 |   2.67 | 0.046 
#> Petal Length |        0.28 | 0.06 | [0.16, 0.40] | 4.58 | 140.98 | < .001
```

### Structural Models

Besides many types of regression models and packages, it also works for
other types of models, such as [**structural
models**](https://easystats.github.io/parameters/articles/efa_cfa.html)
(EFA, CFA, SEM…).

``` r
library(psych)

model <- psych::fa(attitude, nfactors = 3)
model_parameters(model)
#> # Rotated loadings from Factor Analysis (oblimin-rotation)
#> 
#> Variable   |   MR1 |   MR2 |   MR3 | Complexity | Uniqueness
#> ------------------------------------------------------------
#> rating     |  0.90 | -0.07 | -0.05 |       1.02 |       0.23
#> complaints |  0.97 | -0.06 |  0.04 |       1.01 |       0.10
#> privileges |  0.44 |  0.25 | -0.05 |       1.64 |       0.65
#> learning   |  0.47 |  0.54 | -0.28 |       2.51 |       0.24
#> raises     |  0.55 |  0.43 |  0.25 |       2.35 |       0.23
#> critical   |  0.16 |  0.17 |  0.48 |       1.46 |       0.67
#> advance    | -0.11 |  0.91 |  0.07 |       1.04 |       0.22
#> 
#> The 3 latent factors (oblimin rotation) accounted for 66.60% of the total variance of the original data (MR1 = 38.19%, MR2 = 22.69%, MR3 = 5.72%).
```

## Variable and parameters selection

<img src="man/figures/figure2.png" width="100%" style="display: block; margin: auto;" />

[`select_parameters()`](https://easystats.github.io/parameters/articles/parameters_selection.html)
can help you quickly select and retain the most relevant predictors
using methods tailored for the model type.

``` r
lm(disp ~ ., data = mtcars) |>
  select_parameters() |>
  model_parameters()
#> Parameter   | Coefficient |     SE |            95% CI | t(26) |      p
#> -----------------------------------------------------------------------
#> (Intercept) |      141.70 | 125.67 | [-116.62, 400.02] |  1.13 | 0.270 
#> cyl         |       13.14 |   7.90 | [  -3.10,  29.38] |  1.66 | 0.108 
#> hp          |        0.63 |   0.20 | [   0.22,   1.03] |  3.18 | 0.004 
#> wt          |       80.45 |  12.22 | [  55.33, 105.57] |  6.58 | < .001
#> qsec        |      -14.68 |   6.14 | [ -27.31,  -2.05] | -2.39 | 0.024 
#> carb        |      -28.75 |   5.60 | [ -40.28, -17.23] | -5.13 | < .001
```

## Statistical inference - how to quantify evidence

There is no standardized approach to drawing conclusions based on the
available data and statistical models. A frequently chosen but also much
criticized approach is to evaluate results based on their statistical
significance ((Amrhein, Korner-Nievergelt, & Roth, 2017)).

A more sophisticated way would be to test whether estimated effects
exceed the “smallest effect size of interest”, to avoid even the
smallest effects being considered relevant simply because they are
statistically significant, but clinically or practically irrelevant
(Lakens, 2024; Lakens, Scheel, & Isager, 2018). A rather unconventional
approach, which is nevertheless advocated by various authors, is to
interpret results from classical regression models in terms of
probabilities, similar to the usual approach in Bayesian statistics
((Greenland, Rafi, Matthews, & Higgs, 2022; Rafi & Greenland, 2020;
Schweder, 2018; Schweder & Hjort, 2003; Vos & Holbert, 2022)).

The *parameters* package provides several options or functions to aid
statistical inference. These are, for example:

- [`equivalence_test()`](https://easystats.github.io/parameters/reference/equivalence_test.lm.html),
  to compute the (conditional) equivalence test for frequentist models
- [`p_significance()`](https://easystats.github.io/parameters/reference/p_significance.lm.html),
  to compute the probability of *practical significance*, which can be
  conceptualized as a unidirectional equivalence test
- [`p_function()`](https://easystats.github.io/parameters/reference/p_function.html),
  or *consonance function*, to compute p-values and compatibility
  (confidence) intervals for statistical models
- the `pd` argument (setting `pd = TRUE`) in `model_parameters()`
  includes a column with the *probability of direction*, i.e. the
  probability that a parameter is strictly positive or negative. See
  [`bayestestR::p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.html)
  for details.
- the `s_value` argument (setting `s_value = TRUE`) in
  `model_parameters()` replaces the p-values with their related
  *S*-values (*Rafi and Greenland 2020*)
- finally, it is possible to generate distributions of model
  coefficients by generating bootstrap-samples (setting
  `bootstrap = TRUE`) or simulating draws from model coefficients using
  [`simulate_model()`](https://easystats.github.io/parameters/reference/simulate_model.html).
  These samples can then be treated as “posterior samples” and used in
  many functions from the **bayestestR** package.

Most of the above shown options or functions derive from methods
originally implemented for Bayesian models ((Makowski, Ben-Shachar, &
Lüdecke, 2019)). However, assuming that model assumptions are met (which
means, the model fits well to the data, the correct model is chosen that
reflects the data generating process (distributional model family)
etc.), it seems appropriate to interpret results from classical
frequentist models in a “Bayesian way” (more details: documentation in
[`p_function()`](https://easystats.github.io/parameters/reference/p_function.html)).

## Citation

In order to cite this package, please use the following command:

``` r
citation("parameters")
To cite package 'parameters' in publications use:

  Lüdecke D, Ben-Shachar M, Patil I, Makowski D (2020). "Extracting,
  Computing and Exploring the Parameters of Statistical Models using
  R." _Journal of Open Source Software_, *5*(53), 2445.
  doi:10.21105/joss.02445 <https://doi.org/10.21105/joss.02445>.

A BibTeX entry for LaTeX users is

  @Article{,
    title = {Extracting, Computing and Exploring the Parameters of Statistical Models using {R}.},
    volume = {5},
    doi = {10.21105/joss.02445},
    number = {53},
    journal = {Journal of Open Source Software},
    author = {Daniel Lüdecke and Mattan S. Ben-Shachar and Indrajeet Patil and Dominique Makowski},
    year = {2020},
    pages = {2445},
  }
```

## Code of Conduct

Please note that the parameters project is released with a [Contributor
Code of
Conduct](https://www.contributor-covenant.org/version/2/1/code_of_conduct/).
By contributing to this project, you agree to abide by its terms.

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0" line-spacing="2">

<div id="ref-amrhein_earth_2017" class="csl-entry">

Amrhein, V., Korner-Nievergelt, F., & Roth, T. (2017). The earth is flat
( *p* \> 0.05): Significance thresholds and the crisis of unreplicable
research. *PeerJ*, *5*, e3544. <https://doi.org/10.7717/peerj.3544>

</div>

<div id="ref-greenland_aid_2022" class="csl-entry">

Greenland, S., Rafi, Z., Matthews, R., & Higgs, M. (2022). *To Aid
Scientific Inference, Emphasize Unconditional Compatibility Descriptions
of Statistics*. Retrieved from <http://arxiv.org/abs/1909.08583>

</div>

<div id="ref-lakens_improving_2022" class="csl-entry">

Lakens, D. (2024). *Improving Your Statistical Inferences*.
<https://doi.org/10.5281/ZENODO.6409077>

</div>

<div id="ref-lakens2020equivalence" class="csl-entry">

Lakens, D., Scheel, A. M., & Isager, P. M. (2018). Equivalence testing
for psychological research: A tutorial. *Advances in Methods and
Practices in Psychological Science*, *1*(2), 259–269.
<https://doi.org/10.1177/2515245918770963>

</div>

<div id="ref-makowski2019bayetestR" class="csl-entry">

Makowski, D., Ben-Shachar, M., & Lüdecke, D. (2019).
<span class="nocase">bayestestR</span>: Describing effects and their
uncertainty, existence and significance within the bayesian framework.
*Journal of Open Source Software*, *4*(40), 1541.
<https://doi.org/10.21105/joss.01541>

</div>

<div id="ref-rafi_semantic_2020" class="csl-entry">

Rafi, Z., & Greenland, S. (2020). Semantic and cognitive tools to aid
statistical science: Replace confidence and significance by
compatibility and surprise. *BMC Medical Research Methodology*, *20*(1),
244. <https://doi.org/10.1186/s12874-020-01105-9>

</div>

<div id="ref-schweder_confidence_2018" class="csl-entry">

Schweder, T. (2018). Confidence is epistemic probability for empirical
science. *Journal of Statistical Planning and Inference*, *195*,
116–125. <https://doi.org/10.1016/j.jspi.2017.09.016>

</div>

<div id="ref-schweder_frequentist_2003" class="csl-entry">

Schweder, T., & Hjort, N. L. (2003). Frequentist Analogues of Priors and
Posteriors. In B. Stigum (Ed.), *Econometrics and the Philosophy of
Economics: Theory-Data Confrontations in Economics* (pp. 285–217).
Retrieved from <https://www.duo.uio.no/handle/10852/10425>

</div>

<div id="ref-vos_frequentist_2022" class="csl-entry">

Vos, P., & Holbert, D. (2022). Frequentist statistical inference without
repeated sampling. *Synthese*, *200*(2), 89.
<https://doi.org/10.1007/s11229-022-03560-x>

</div>

</div>

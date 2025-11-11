# Model Parameters

Compute and extract model parameters. The available options and
arguments depend on the modeling **package** and model `class`. Follow
one of these links to read the model-specific documentation:

- [Default
  method](https://easystats.github.io/parameters/reference/model_parameters.default.md):
  `lm`, `glm`, **stats**, **censReg**, **MASS**, **survey**, ...

- [Additive
  models](https://easystats.github.io/parameters/reference/model_parameters.cgam.md):
  **bamlss**, **gamlss**, **mgcv**, **scam**, **VGAM**, `Gam` (although
  the output of `Gam` is more Anova-alike), `gamm`, ...

- [ANOVA](https://easystats.github.io/parameters/reference/model_parameters.aov.md):
  **afex**, `aov`, `anova`, `Gam`, ...

- [Bayesian](https://easystats.github.io/parameters/reference/model_parameters.brmsfit.md):
  **BayesFactor**, **blavaan**, **brms**, **MCMCglmm**, **posterior**,
  **rstanarm**, `bayesQR`, `bcplm`, `BGGM`, `blmrm`, `blrm`,
  `mcmc.list`, `MCMCglmm`, ...

- [Clustering](https://easystats.github.io/parameters/reference/model_parameters.hclust.md):
  **hclust**, **kmeans**, **mclust**, **pam**, ...

- [Correlations, t-tests,
  etc.](https://easystats.github.io/parameters/reference/model_parameters.htest.md):
  **lmtest**, `htest`, `pairwise.htest`, ...

- [Meta-Analysis](https://easystats.github.io/parameters/reference/model_parameters.rma.md):
  **metaBMA**, **metafor**, **metaplus**, ...

- [Mixed
  models](https://easystats.github.io/parameters/reference/model_parameters.glmmTMB.md):
  **cplm**, **glmmTMB**, **lme4**, **lmerTest**, **nlme**, **ordinal**,
  **robustlmm**, **spaMM**, `mixed`, `MixMod`, ...

- [Multinomial, ordinal and cumulative
  link](https://easystats.github.io/parameters/reference/model_parameters.mlm.md):
  **brglm2**, **DirichletReg**, **nnet**, **ordinal**, `mlm`, ...

- [Multiple
  imputation](https://easystats.github.io/parameters/reference/model_parameters.mira.md):
  **mice**

- [PCA, FA, CFA,
  SEM](https://easystats.github.io/parameters/reference/model_parameters.principal.md):
  **FactoMineR**, **lavaan**, **psych**, `sem`, ...

- [Zero-inflated and
  hurdle](https://easystats.github.io/parameters/reference/model_parameters.zcpglm.md):
  **cplm**, **mhurdle**, **pscl**, ...

- [Other
  models](https://easystats.github.io/parameters/reference/model_parameters.glimML.md):
  **aod**, **bbmle**, **betareg**, **emmeans**, **epiR**, **glmx**,
  **ivfixed**, **ivprobit**, **JRM**, **lmodel2**, **logitsf**,
  **marginaleffects**, **margins**, **maxLik**, **mediation**, **mfx**,
  **multcomp**, **mvord**, **plm**, **PMCMRplus**, **quantreg**,
  **selection**, **systemfit**, **tidymodels**, **varEST**, **WRS2**,
  `bfsl`, `deltaMethod`, `fitdistr`, `mjoint`, `mle`, `model.avg`, ...

## Usage

``` r
model_parameters(model, ...)

parameters(model, ...)
```

## Arguments

- model:

  Statistical Model.

- ...:

  Arguments passed to or from other methods. Non-documented arguments
  are

  - `digits`, `p_digits`, `ci_digits` and `footer_digits` to set the
    number of digits for the output. `groups` can be used to group
    coefficients. These arguments will be passed to the print-method, or
    can directly be used in
    [`print()`](https://rdrr.io/r/base/print.html), see documentation in
    [`print.parameters_model()`](https://easystats.github.io/parameters/reference/print.parameters_model.md).

  - If `s_value = TRUE`, the p-value will be replaced by the S-value in
    the output (cf. *Rafi and Greenland 2020*).

  - `pd` adds an additional column with the *probability of direction*
    (see
    [`bayestestR::p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.html)
    for details). Furthermore, see 'Examples' in
    [`model_parameters.default()`](https://easystats.github.io/parameters/reference/model_parameters.default.md).

  - For developers, whose interest mainly is to get a "tidy" data frame
    of model summaries, it is recommended to set `pretty_names = FALSE`
    to speed up computation of the summary table.

## Value

A data frame of indices related to the model's parameters.

## Details

A full overview can be found here:
https://easystats.github.io/parameters/reference/

## Note

The
[`print()`](https://easystats.github.io/parameters/reference/print.parameters_model.md)
method has several arguments to tweak the output. There is also a
[[`plot()`](https://rdrr.io/r/graphics/plot.default.html)-method](https://easystats.github.io/see/articles/parameters.html)
implemented in the [**see**-package](https://easystats.github.io/see/),
and a dedicated method for use inside rmarkdown files,
[`print_md()`](https://easystats.github.io/parameters/reference/print.parameters_model.md).  
  
**For developers**, if speed performance is an issue, you can use the
(undocumented) `pretty_names` argument, e.g.
`model_parameters(..., pretty_names = FALSE)`. This will skip the
formatting of the coefficient names and makes `model_parameters()`
faster.

## Standardization of model coefficients

Standardization is based on
[`standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.md).
In case of `standardize = "refit"`, the data used to fit the model will
be standardized and the model is completely refitted. In such cases,
standard errors and confidence intervals refer to the standardized
coefficient. The default, `standardize = "refit"`, never standardizes
categorical predictors (i.e. factors), which may be a different
behaviour compared to other R packages or other software packages (like
SPSS). To mimic behaviour of SPSS or packages such as **lm.beta**, use
`standardize = "basic"`.

## Standardization Methods

- **refit**: This method is based on a complete model re-fit with a
  standardized version of the data. Hence, this method is equal to
  standardizing the variables before fitting the model. It is the
  "purest" and the most accurate (Neter et al., 1989), but it is also
  the most computationally costly and long (especially for heavy models
  such as Bayesian models). This method is particularly recommended for
  complex models that include interactions or transformations (e.g.,
  polynomial or spline terms). The `robust` (default to `FALSE`)
  argument enables a robust standardization of data, i.e., based on the
  `median` and `MAD` instead of the `mean` and `SD`. **See
  [`datawizard::standardize()`](https://easystats.github.io/datawizard/reference/standardize.html)
  for more details.** **Note** that
  `standardize_parameters(method = "refit")` may not return the same
  results as fitting a model on data that has been standardized with
  `standardize()`;
  [`standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.md)
  used the data used by the model fitting function, which might not be
  same data if there are missing values. see the `remove_na` argument in
  `standardize()`.

- **posthoc**: Post-hoc standardization of the parameters, aiming at
  emulating the results obtained by "refit" without refitting the model.
  The coefficients are divided by the standard deviation (or MAD if
  `robust`) of the outcome (which becomes their expression 'unit').
  Then, the coefficients related to numeric variables are additionally
  multiplied by the standard deviation (or MAD if `robust`) of the
  related terms, so that they correspond to changes of 1 SD of the
  predictor (e.g., "A change in 1 SD of `x` is related to a change of
  0.24 of the SD of `y`). This does not apply to binary variables or
  factors, so the coefficients are still related to changes in levels.
  This method is not accurate and tend to give aberrant results when
  interactions are specified.

- **basic**: This method is similar to `method = "posthoc"`, but treats
  all variables as continuous: it also scales the coefficient by the
  standard deviation of model's matrix' parameter of factors levels
  (transformed to integers) or binary predictors. Although being
  inappropriate for these cases, this method is the one implemented by
  default in other software packages, such as
  [`lm.beta::lm.beta()`](https://rdrr.io/pkg/lm.beta/man/lm.beta.html).

- **smart** (Standardization of Model's parameters with Adjustment,
  Reconnaissance and Transformation - *experimental*): Similar to
  `method = "posthoc"` in that it does not involve model refitting. The
  difference is that the SD (or MAD if `robust`) of the response is
  computed on the relevant section of the data. For instance, if a
  factor with 3 levels A (the intercept), B and C is entered as a
  predictor, the effect corresponding to B vs. A will be scaled by the
  variance of the response at the intercept only. As a results, the
  coefficients for effects of factors are similar to a Glass' delta.

- **pseudo** (*for 2-level (G)LMMs only*): In this (post-hoc) method,
  the response and the predictor are standardized based on the level of
  prediction (levels are detected with
  [`performance::check_group_variation()`](https://easystats.github.io/performance/reference/check_group_variation.html)):
  Predictors are standardized based on their SD at level of prediction
  (see also
  [`datawizard::demean()`](https://easystats.github.io/datawizard/reference/demean.html));
  The outcome (in linear LMMs) is standardized based on a fitted
  random-intercept-model, where `sqrt(random-intercept-variance)` is
  used for level 2 predictors, and `sqrt(residual-variance)` is used for
  level 1 predictors (Hoffman 2015, page 342). A warning is given when a
  within-group variable is found to have access between-group variance.

See also [package
vignette](https://easystats.github.io/parameters/articles/standardize_parameters_effsize.html).

## Labeling the Degrees of Freedom

Throughout the **parameters** package, we decided to label the residual
degrees of freedom *df_error*. The reason for this is that these degrees
of freedom not always refer to the residuals. For certain models, they
refer to the estimate error - in a linear model these are the same, but
in - for instance - any mixed effects model, this isn't strictly true.
Hence, we think that `df_error` is the most generic label for these
degrees of freedom.

## Confidence intervals and approximation of degrees of freedom

There are different ways of approximating the degrees of freedom
depending on different assumptions about the nature of the model and its
sampling distribution. The `ci_method` argument modulates the method for
computing degrees of freedom (df) that are used to calculate confidence
intervals (CI) and the related p-values. Following options are allowed,
depending on the model class:

**Classical methods:**

Classical inference is generally based on the **Wald method**. The Wald
approach to inference computes a test statistic by dividing the
parameter estimate by its standard error (Coefficient / SE), then
comparing this statistic against a t- or normal distribution. This
approach can be used to compute CIs and p-values.

`"wald"`:

- Applies to *non-Bayesian models*. For *linear models*, CIs computed
  using the Wald method (SE and a *t-distribution with residual df*);
  p-values computed using the Wald method with a *t-distribution with
  residual df*. For other models, CIs computed using the Wald method (SE
  and a *normal distribution*); p-values computed using the Wald method
  with a *normal distribution*.

`"normal"`

- Applies to *non-Bayesian models*. Compute Wald CIs and p-values, but
  always use a normal distribution.

`"residual"`

- Applies to *non-Bayesian models*. Compute Wald CIs and p-values, but
  always use a *t-distribution with residual df* when possible. If the
  residual df for a model cannot be determined, a normal distribution is
  used instead.

**Methods for mixed models:**

Compared to fixed effects (or single-level) models, determining
appropriate df for Wald-based inference in mixed models is more
difficult. See [the R GLMM
FAQ](https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#what-are-the-p-values-listed-by-summaryglmerfit-etc.-are-they-reliable)
for a discussion.

Several approximate methods for computing df are available, but you
should also consider instead using profile likelihood (`"profile"`) or
bootstrap ("`boot"`) CIs and p-values instead.

`"satterthwaite"`

- Applies to *linear mixed models*. CIs computed using the Wald method
  (SE and a *t-distribution with Satterthwaite df*); p-values computed
  using the Wald method with a *t-distribution with Satterthwaite df*.

`"kenward"`

- Applies to *linear mixed models*. CIs computed using the Wald method
  (*Kenward-Roger SE* and a *t-distribution with Kenward-Roger df*);
  p-values computed using the Wald method with *Kenward-Roger SE and
  t-distribution with Kenward-Roger df*.

`"ml1"`

- Applies to *linear mixed models*. CIs computed using the Wald method
  (SE and a *t-distribution with m-l-1 approximated df*); p-values
  computed using the Wald method with a *t-distribution with m-l-1
  approximated df*. See
  [`ci_ml1()`](https://easystats.github.io/parameters/reference/p_value_ml1.md).

`"betwithin"`

- Applies to *linear mixed models* and *generalized linear mixed
  models*. CIs computed using the Wald method (SE and a *t-distribution
  with between-within df*); p-values computed using the Wald method with
  a *t-distribution with between-within df*. See
  [`ci_betwithin()`](https://easystats.github.io/parameters/reference/p_value_betwithin.md).

**Likelihood-based methods:**

Likelihood-based inference is based on comparing the likelihood for the
maximum-likelihood estimate to the the likelihood for models with one or
more parameter values changed (e.g., set to zero or a range of
alternative values). Likelihood ratios for the maximum-likelihood and
alternative models are compared to a \\\chi\\-squared distribution to
compute CIs and p-values.

`"profile"`

- Applies to *non-Bayesian models* of class `glm`, `polr`, `merMod` or
  `glmmTMB`. CIs computed by *profiling the likelihood curve for a
  parameter*, using linear interpolation to find where likelihood ratio
  equals a critical value; p-values computed using the Wald method with
  a *normal-distribution* (note: this might change in a future update!)

`"uniroot"`

- Applies to *non-Bayesian models* of class `glmmTMB`. CIs computed by
  *profiling the likelihood curve for a parameter*, using root finding
  to find where likelihood ratio equals a critical value; p-values
  computed using the Wald method with a *normal-distribution* (note:
  this might change in a future update!)

**Methods for bootstrapped or Bayesian models:**

Bootstrap-based inference is based on **resampling** and refitting the
model to the resampled datasets. The distribution of parameter estimates
across resampled datasets is used to approximate the parameter's
sampling distribution. Depending on the type of model, several different
methods for bootstrapping and constructing CIs and p-values from the
bootstrap distribution are available.

For Bayesian models, inference is based on drawing samples from the
model posterior distribution.

`"quantile"` (or `"eti"`)

- Applies to *all models (including Bayesian models)*. For non-Bayesian
  models, only applies if `bootstrap = TRUE`. CIs computed as *equal
  tailed intervals* using the quantiles of the bootstrap or posterior
  samples; p-values are based on the *probability of direction*. See
  [`bayestestR::eti()`](https://easystats.github.io/bayestestR/reference/eti.html).

`"hdi"`

- Applies to *all models (including Bayesian models)*. For non-Bayesian
  models, only applies if `bootstrap = TRUE`. CIs computed as *highest
  density intervals* for the bootstrap or posterior samples; p-values
  are based on the *probability of direction*. See
  [`bayestestR::hdi()`](https://easystats.github.io/bayestestR/reference/hdi.html).

`"bci"` (or `"bcai"`)

- Applies to *all models (including Bayesian models)*. For non-Bayesian
  models, only applies if `bootstrap = TRUE`. CIs computed as *bias
  corrected and accelerated intervals* for the bootstrap or posterior
  samples; p-values are based on the *probability of direction*. See
  [`bayestestR::bci()`](https://easystats.github.io/bayestestR/reference/bci.html).

`"si"`

- Applies to *Bayesian models* with proper priors. CIs computed as
  *support intervals* comparing the posterior samples against the prior
  samples; p-values are based on the *probability of direction*. See
  [`bayestestR::si()`](https://easystats.github.io/bayestestR/reference/si.html).

`"boot"`

- Applies to *non-Bayesian models* of class `merMod`. CIs computed using
  *parametric bootstrapping* (simulating data from the fitted model);
  p-values computed using the Wald method with a *normal-distribution)*
  (note: this might change in a future update!).

For all iteration-based methods other than `"boot"` (`"hdi"`,
`"quantile"`, `"ci"`, `"eti"`, `"si"`, `"bci"`, `"bcai"`), p-values are
based on the probability of direction
([`bayestestR::p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.html)),
which is converted into a p-value using
[`bayestestR::pd_to_p()`](https://easystats.github.io/bayestestR/reference/pd_to_p.html).

## Statistical inference - how to quantify evidence

There is no standardized approach to drawing conclusions based on the
available data and statistical models. A frequently chosen but also much
criticized approach is to evaluate results based on their statistical
significance (*Amrhein et al. 2017*).

A more sophisticated way would be to test whether estimated effects
exceed the "smallest effect size of interest", to avoid even the
smallest effects being considered relevant simply because they are
statistically significant, but clinically or practically irrelevant
(*Lakens et al. 2018, Lakens 2024*).

A rather unconventional approach, which is nevertheless advocated by
various authors, is to interpret results from classical regression
models either in terms of probabilities, similar to the usual approach
in Bayesian statistics (*Schweder 2018; Schweder and Hjort 2003; Vos
2022*) or in terms of relative measure of "evidence" or "compatibility"
with the data (*Greenland et al. 2022; Rafi and Greenland 2020*), which
nevertheless comes close to a probabilistic interpretation.

A more detailed discussion of this topic is found in the documentation
of
[`p_function()`](https://easystats.github.io/parameters/reference/p_function.md).

The **parameters** package provides several options or functions to aid
statistical inference. These are, for example:

- [`equivalence_test()`](https://easystats.github.io/parameters/reference/equivalence_test.lm.md),
  to compute the (conditional) equivalence test for frequentist models

- [`p_significance()`](https://easystats.github.io/parameters/reference/p_significance.lm.md),
  to compute the probability of *practical significance*, which can be
  conceptualized as a unidirectional equivalence test

- [`p_function()`](https://easystats.github.io/parameters/reference/p_function.md),
  or *consonance function*, to compute p-values and compatibility
  (confidence) intervals for statistical models

- the `pd` argument (setting `pd = TRUE`) in `model_parameters()`
  includes a column with the *probability of direction*, i.e. the
  probability that a parameter is strictly positive or negative. See
  [`bayestestR::p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.html)
  for details. If plotting is desired, the
  [`p_direction()`](https://easystats.github.io/parameters/reference/p_direction.lm.md)
  function can be used, together with
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

- the `s_value` argument (setting `s_value = TRUE`) in
  `model_parameters()` replaces the p-values with their related
  *S*-values (*Rafi and Greenland 2020*)

- finally, it is possible to generate distributions of model
  coefficients by generating bootstrap-samples (setting
  `bootstrap = TRUE`) or simulating draws from model coefficients using
  [`simulate_model()`](https://easystats.github.io/parameters/reference/simulate_model.md).
  These samples can then be treated as "posterior samples" and used in
  many functions from the **bayestestR** package.

Most of the above shown options or functions derive from methods
originally implemented for Bayesian models (*Makowski et al. 2019*).
However, assuming that model assumptions are met (which means, the model
fits well to the data, the correct model is chosen that reflects the
data generating process (distributional model family) etc.), it seems
appropriate to interpret results from classical frequentist models in a
"Bayesian way" (more details: documentation in
[`p_function()`](https://easystats.github.io/parameters/reference/p_function.md)).

## Interpretation of Interaction Terms

Note that the *interpretation* of interaction terms depends on many
characteristics of the model. The number of parameters, and overall
performance of the model, can differ *or not* between `a * b`, `a : b`,
and `a / b`, suggesting that sometimes interaction terms give different
parameterizations of the same model, but other times it gives completely
different models (depending on `a` or `b` being factors of covariates,
included as main effects or not, etc.). Their interpretation depends of
the full context of the model, which should not be inferred from the
parameters table alone - rather, we recommend to use packages that
calculate estimated marginal means or marginal effects, such as
[modelbased](https://CRAN.R-project.org/package=modelbased),
[emmeans](https://CRAN.R-project.org/package=emmeans),
[ggeffects](https://CRAN.R-project.org/package=ggeffects), or
[marginaleffects](https://CRAN.R-project.org/package=marginaleffects).
To raise awareness for this issue, you may use
`print(...,show_formula=TRUE)` to add the model-specification to the
output of the
[`print()`](https://easystats.github.io/parameters/reference/print.parameters_model.md)
method for `model_parameters()`.

## Global Options to Customize Messages and Tables when Printing

The `verbose` argument can be used to display or silence messages and
warnings for the different functions in the **parameters** package.
However, some messages providing additional information can be displayed
or suppressed using [`options()`](https://rdrr.io/r/base/options.html):

- `parameters_info`: `options(parameters_info = TRUE)` will override the
  `include_info` argument in `model_parameters()` and always show the
  model summary for non-mixed models.

- `parameters_mixed_info`: `options(parameters_mixed_info = TRUE)` will
  override the `include_info` argument in `model_parameters()` for mixed
  models, and will then always show the model summary.

- `parameters_cimethod`: `options(parameters_cimethod = TRUE)` will show
  the additional information about the approximation method used to
  calculate confidence intervals and p-values. Set to `FALSE` to hide
  this message when printing `model_parameters()` objects.

- `parameters_exponentiate`: `options(parameters_exponentiate = TRUE)`
  will show the additional information on how to interpret coefficients
  of models with log-transformed response variables or with
  log-/logit-links when the `exponentiate` argument in
  `model_parameters()` is not `TRUE`. Set this option to `FALSE` to hide
  this message when printing `model_parameters()` objects.

There are further options that can be used to modify the default
behaviour for printed outputs:

- `parameters_labels`: `options(parameters_labels = TRUE)` will use
  variable and value labels for pretty names, if data is labelled. If no
  labels available, default pretty names are used.

- `parameters_interaction`:
  `options(parameters_interaction = <character>)` will replace the
  interaction mark (by default, `*`) with the related character.

- `parameters_select`: `options(parameters_select = <value>)` will set
  the default for the `select` argument. See argument's documentation
  for available options.

- `easystats_table_width`: `options(easystats_table_width = <value>)`
  will set the default width for tables in text-format, i.e. for most of
  the outputs printed to console. If not specified, tables will be
  adjusted to the current available width, e.g. of the of the console
  (or any other source for textual output, like markdown files). The
  argument `table_width` can also be used in most
  [`print()`](https://rdrr.io/r/base/print.html) methods to specify the
  table width as desired.

- `insight_use_symbols`: `options(insight_use_symbols = TRUE)` will try
  to print unicode-chars for symbols as column names, wherever possible
  (e.g., ω instead of `Omega`).

## References

- Amrhein, V., Korner-Nievergelt, F., and Roth, T. (2017). The earth is
  flat (p \> 0.05): Significance thresholds and the crisis of
  unreplicable research. PeerJ, 5, e3544.
  [doi:10.7717/peerj.3544](https://doi.org/10.7717/peerj.3544)

- Greenland S, Rafi Z, Matthews R, Higgs M. To Aid Scientific Inference,
  Emphasize Unconditional Compatibility Descriptions of
  Statistics. (2022) https://arxiv.org/abs/1909.08583v7 (Accessed
  November 10, 2022)

- Hoffman, L. (2015). Longitudinal analysis: Modeling within-person
  fluctuation and change. Routledge.

- Lakens, D. (2024). Improving Your Statistical Inferences (Version
  v1.5.1). Retrieved from
  https://lakens.github.io/statistical_inferences/.
  [doi:10.5281/ZENODO.6409077](https://doi.org/10.5281/ZENODO.6409077)

- Lakens, D., Scheel, A. M., and Isager, P. M. (2018). Equivalence
  Testing for Psychological Research: A Tutorial. Advances in Methods
  and Practices in Psychological Science, 1(2), 259–269.

- Makowski, D., Ben-Shachar, M. S., Chen, S. H. A., and Lüdecke, D.
  (2019). Indices of Effect Existence and Significance in the Bayesian
  Framework. Frontiers in Psychology, 10, 2767.
  [doi:10.3389/fpsyg.2019.02767](https://doi.org/10.3389/fpsyg.2019.02767)

- Montiel Olea, J. L., and Plagborg-Møller, M. (2019). Simultaneous
  confidence bands: Theory, implementation, and an application to SVARs.
  Journal of Applied Econometrics, 34(1), 1–17.
  [doi:10.1002/jae.2656](https://doi.org/10.1002/jae.2656)

- Neter, J., Wasserman, W., and Kutner, M. H. (1989). Applied linear
  regression models.

- Rafi Z, Greenland S. Semantic and cognitive tools to aid statistical
  science: replace confidence and significance by compatibility and
  surprise. BMC Medical Research Methodology (2020) 20:244.

- Schweder T. Confidence is epistemic probability for empirical science.
  Journal of Statistical Planning and Inference (2018) 195:116–125.
  [doi:10.1016/j.jspi.2017.09.016](https://doi.org/10.1016/j.jspi.2017.09.016)

- Schweder T, Hjort NL. Frequentist analogues of priors and posteriors.
  In Stigum, B. (ed.), Econometrics and the Philosophy of Economics:
  Theory Data Confrontation in Economics, pp. 285-217. Princeton
  University Press, Princeton, NJ, 2003

- Vos P, Holbert D. Frequentist statistical inference without repeated
  sampling. Synthese 200, 89 (2022).
  [doi:10.1007/s11229-022-03560-x](https://doi.org/10.1007/s11229-022-03560-x)

## See also

[`insight::standardize_names()`](https://easystats.github.io/insight/reference/standardize_names.html)
to rename columns into a consistent, standardized naming scheme.

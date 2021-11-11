# parameters 0.15.1

## General

* Improved speed performance for `model_parameters()`, in particular for glm's.

* Added more options for printing `model_parameters()`. See also revised vignette:
  https://easystats.github.io/parameters/articles/model_parameters_print.html

## Changes to functions

### `model_parameters()`

* `model_parameters()` for mixed models gains an `include_sigma` argument. If
  `TRUE`, adds the residual variance, computed from the random effects variances,
  as an attribute to the returned data frame. Including sigma was the default 
  behaviour, but now defaults to `FALSE` and is only included when 
  `include_sigma = TRUE`, because the calculation was very time consuming.

* `model_parameters()` for `merMod` models now also computes CIs for the random
  SD parameters when `ci_method="boot"` (previously, this was only possible when
  `ci_method` was `"profile"`).

* `model_parameters()` for `glmmTMB` models now computes CIs for the random SD
  parameters. Note that these are based on a Wald-z-distribution.

* Similar to `model_parameters.htest()`, the `model_parameters.BFBayesFactor()`
  method gains `cohens_d` and `cramers_v` arguments to control if you need to
  add frequentist effect size estimates to the returned summary data frame.
  Previously, this was done by default.

* Column name for coefficients from *emmeans* objects are now more specific.

* `model_prameters()` for `MixMod` objects (package *GLMMadaptive*) gains a
  `robust` argument, to compute robust standard errors.

## Bug fixes 

* Fixed bug with `ci()` for class `merMod` when `method="boot"`.

* Fixed issue with correct association of components for ordinal models of 
  classes `clm` and `clm2`.

* Fixed issues in `random_parameters()` and  `model_parameters()` for mixed 
  models without random intercept.

* Confidence intervals for random parameters in `model_parameters()` failed for
  (some?) `glmer` models.

* Fix issue with default `ci_type` in `compare_parameters()` for Bayesian models.

# parameters 0.15.0

## Breaking changes

* Following functions were moved to the new *datawizard* package and are now
  re-exported from *parameters* package:

  - `center()`

  - `convert_data_to_numeric()`

  - `data_partition()`

  - `demean()` (and its aliases `degroup()` and `detrend()`)

  - `kurtosis()`

  - `rescale_weights()`

  - `skewness()`

  - `smoothness()`

Note that these functions will be removed in the next release of *parameters*
package and they are currently being re-exported only as a convenience for the
package developers. This release should provide them with time to make the
necessary changes before this breaking change is implemented.

* Following functions were moved to the *performance* package:

  - `check_heterogeneity()`

  - `check_multimodal()`

## General

* The handling to approximate the degrees of freedom in `model_parameters()`,
  `ci()` and `p_value()` was revised and should now be more consistent. Some
  bugs related to the previous computation of confidence intervals and p-values
  have been fixed. Now it is possible to change the method to approximate
  degrees of freedom for CIs and p-values using the `ci_method`, resp. `method`
  argument. This change has been documented in detail in `?model_parameters`,
  and online here:
  https://easystats.github.io/parameters/reference/model_parameters.html

* Minor changes to `print()` for *glmmTMB* with dispersion parameter.

* Added vignette on printing options for model parameters.

## Changes to functions

### `model_parameters()`

* The `df_method` argument in `model_parameters()` is deprecated. Please use
  `ci_method` now.

* `model_parameters()` with `standardize = "refit"` now returns random effects
  from the standardized model.

* `model_parameters()` and `ci()` for `lmerMod` models gain a `"residuals"`
  option for the `ci_method` (resp. `method`) argument, to explicitly calculate
  confidence intervals based on the residual degrees of freedom, when present.

* `model_parameters()` supports following new objects: `trimcibt`, `wmcpAKP`,
  `dep.effect` (in *WRS2* package), `systemfit`

* `model_parameters()` gains a new argument `table_wide` for ANOVA tables. This
  can be helpful for users who may wish to report ANOVA table in wide format
  (i.e., with numerator and denominator degrees of freedom on the same row).

* `model_parameters()` gains two new arguments, `keep` and `drop`. `keep` is the
  new names for the former `parameters` argument and can be used to filter
  parameters. While `keep` selects those parameters whose names match the
  regular expression pattern defined in `keep`, `drop` is the counterpart and
  excludes matching parameter names.

* When `model_parameters()` is called with `verbose = TRUE`, and `ci_method` is
  not the default value, the printed output includes a message indicating which
  approximation-method for degrees of freedom was used.

* `model_parameters()` for mixed models with `ci_method = "profile` computes
  (profiled) confidence intervals for both fixed and random effects. Thus,
  `ci_method = "profile` allows to add confidence intervals to the random effect
  variances.

* `model_parameters()` should longer fail for supported model classes when
  robust standard errors are not available.

### Other functions

* `n_factors()` the methods based on fit indices have been fixed and can be
  included separately (`package = "fit"`). Also added a `n_max` argument to crop
  the output.

* `compare_parameters()` now also accepts a list of model objects.

* `describe_distribution()` gets `verbose` argument to toggle warnings and
  messages.

* `format_parameters()` removes dots and underscores from parameter names, to
  make these more "human readable".

* The experimental calculation of p-values in `equivalence_test()` was replaced
  by a proper calculation p-values. The argument `p_value` was removed and
  p-values are now always included.

* Minor improvements to `print()`, `print_html()` and `print_md()`.

## Bug fixes

* The random effects returned by `model_parameters()` mistakenly displayed the
  residuals standard deviation as square-root of the residual SD.

* Fixed issue with `model_parameters()` for *brmsfit* objects that model
  standard errors (i.e. for meta-analysis).

* Fixed issue in `model_parameters` for `lmerMod` models that, by default,
  returned residual degrees of freedom in the statistic column, but confidence
  intervals were based on `Inf` degrees of freedom instead.

* Fixed issue in `ci_satterthwaite()`, which used `Inf` degrees of freedom
  instead of the Satterthwaite approximation.

* Fixed issue in `model_parameters.mlm()` when model contained interaction
  terms.

* Fixed issue in `model_parameters.rma()` when model contained interaction
  terms.

* Fixed sign error for `model_parameters.htest()` for objects created with
  `t.test.formula()` (issue #552)

* Fixed issue when computing random effect variances in `model_parameters()` for
  mixed models with categorical random slopes.

# parameters 0.14.0

## Breaking changes

* `check_sphericity()` has been renamed into `check_sphericity_bartlett()`.

* Removed deprecated arguments.

* `model_parameters()` for bootstrapped samples used in *emmeans* now treats the
  bootstrap samples as samples from posterior distributions (Bayesian models).

## New supported model classes

* `SemiParBIV` (*GJRM*), `selection` (*sampleSelection*), `htest` from the
  *survey* package, `pgmm` (*plm*).

## General

* Performance improvements for models from package *survey*.

## New functions

* Added a `summary()` method for `model_parameters()`, which is a convenient
  shortcut for `print(..., select = "minimal")`.

## Changes to functions

### `model_parameters()`

* `model_parameters()` gains a `parameters` argument, which takes a regular
  expression as string, to select specific parameters from the returned data
  frame.

* `print()` for `model_parameters()` and `compare_parameters()` gains a `groups`
  argument, to group parameters in the output. Furthermore, `groups` can be used
  directly as argument in `model_parameters()` and `compare_parameters()` and
  will be passed to the `print()` method.

* `model_parameters()` for ANOVAs now saves the type as attribute and prints
  this information as footer in the output as well.

* `model_parameters()` for *htest*-objects now saves the alternative hypothesis
  as attribute and prints this information as footer in the output as well.

* `model_parameters()` passes arguments `type`, `parallel` and `n_cpus` down to
  `bootstrap_model()` when `bootstrap = TRUE`.

### other

* `bootstrap_models()` for *merMod* and *glmmTMB* objects gains further
  arguments to set the type of bootstrapping and to allow parallel computing.

* `bootstrap_parameters()` gains the `ci_method` type `"bci"`, to compute
  bias-corrected and accelerated bootstrapped intervals.

* `ci()` for `svyglm` gains a `method` argument.

## Bug fixes

* Fixed issue in `model_parameters()` for *emmGrid* objects with Bayesian
  models.

* Arguments `digits`, `ci_digits` and `p_digits` were ignored for `print()` and
  only worked when used in the call to `model_parameters()` directly.

# parameters 0.13.0

## General

* Revised and improved the `print()` method for `model_parameters()`.

## New supported model classes

* `blrm` (*rmsb*), `AKP`, `med1way`, `robtab` (*WRS2*), `epi.2by2` (*epiR*),
  `mjoint` (*joineRML*), `mhurdle` (*mhurdle*), `sarlm` (*spatialreg*),
  `model_fit` (*tidymodels*), `BGGM` (*BGGM*), `mvord` (*mvord*)

## Changes to functions

### `model_parameters()`

* `model_parameters()` for `blavaan` models is now fully treated as Bayesian
  model and thus relies on the functions from *bayestestR* (i.e. ROPE, Rhat or
  ESS are reported) .

* The `effects`-argument from `model_parameters()` for mixed models was revised
  and now shows the random effects variances by default (same functionality as
  `random_parameters()`, but mimicking the behaviour from
  `broom.mixed::tidy()`). When the `group_level` argument is set to `TRUE`, the
  conditional modes (BLUPs) of the random effects are shown.

* `model_parameters()` for mixed models now returns an `Effects` column even
  when there is just one type of "effects", to mimic the behaviour from
  `broom.mixed::tidy()`. In conjunction with `standardize_names()` users can get
  the same column names as in `tidy()` for `model_parameters()` objects.

* `model_parameters()` for t-tests now uses the group values as column names.

* `print()` for `model_parameters()` gains a `zap_small` argument, to avoid
  scientific notation for very small numbers. Instead, `zap_small` forces to
  round to the specified number of digits.

* To be internally consistent, the degrees of freedom column for `lqm(m)` and
  `cgam(m)` objects (with *t*-statistic) is called `df_error`.

* `model_parameters()` gains a `summary` argument to add summary information
  about the model to printed outputs.

* Minor improvements for models from *quantreg*.

* `model_parameters` supports rank-biserial, rank epsilon-squared, and Kendall's
  *W* as effect size measures for `wilcox.test()`, `kruskal.test`, and
  `friedman.test`, respectively.

### Other functions

* `describe_distribution()` gets a `quartiles` argument to include 25th and 75th
  quartiles of a variable.

## Bug fixes

* Fixed issue with non-initialized argument `style` in `display()` for
  `compare_parameters()`.

* Make `print()` for `compare_parameters()` work with objects that have "simple"
  column names for confidence intervals with missing CI-level (i.e. when column
  is named `"CI"` instead of, say, `"95% CI"`).

* Fixed issue with `p_adjust` in `model_parameters()`, which did not work for
  adjustment-methods `"BY"` and `"BH"`.

* Fixed issue with `show_sigma` in `print()` for `model_parameters()`.

* Fixed issue in `model_parameters()` with incorrect order of degrees of
  freedom.

# parameters 0.12.0

## General

* Roll-back R dependency to R >= 3.4.

* Bootstrapped estimates (from `bootstrap_model()` or `bootstrap_parameters()`)
  can be passed to `emmeans` to obtain bootstrapped estimates, contrasts, simple
  slopes (etc) and their CIs.

  * These can then be passed to `model_parameters()` and related functions to
    obtain standard errors, p-values, etc.

## Breaking changes

* `model_parameters()` now always returns the confidence level for as additional
  `CI` column.

* The `rule` argument in `equivalenct_test()` defaults to `"classic"`.

## New supported model classes

* `crr` (*cmprsk*), `leveneTest()` (*car*), `varest` (*vars*), `ergm` (*ergm*),
  `btergm` (*btergm*), `Rchoice` (*Rchoice*), `garch` (*tseries*)

## New functions

* `compare_parameters()` (and its alias `compare_models()`) to show / print
  parameters of multiple models in one table.

## Changes to functions

* Estimation of bootstrapped *p*-values has been re-written to be more
  accurate.

* `model_parameters()` for mixed models gains an `effects`-argument, to return
  fixed, random or both fixed and random effects parameters.

* Revised printing for `model_parameters()` for *metafor* models.

* `model_parameters()` for *metafor* models now recognized confidence levels
  specified in the function call (via argument `level`).

* Improved support for effect sizes in `model_parameters()` from *anova*
  objects.

## Bug fixes

* Fixed edge case when formatting parameters from polynomial terms with many
  degrees.

* Fixed issue with random sampling and dropped factor levels in
  `bootstrap_model()`.

# parameters 0.11.0

## New supported model classes

* `coxr` (*coxrobust*), `coeftest` (*lmtest*), `ivfixed` (*ivfixed*), `ivprobit`
  (*ivprobit*), `riskRegression` (*riskRegression*), `fitdistr` (*MASS*),
  `yuen`, `t1way`, `onesampb`, `mcp1` and `mcp2` (*WRS2*), `Anova.mlm` (*car*),
  `rqs` (*quantreg*), `lmodel2` (*lmodel2*), `summary.lm`, `PMCMR`, `osrt` and
  `trendPMCMR` (*PMCMRplus*), `bamlss` (*bamlss*).

## New functions

### Printing and table Formatting

* `print_html()` as an alias for `display(format = "html")`. This allows to
  print tabular outputs from data frames (as returned by most functions in
  _parameters_) into nicely rendered HTML markdown tables.

## Changes to functions

* Added more effect size measures to `model_parameters()` for `htest` objects.

* `model_parameters()` for anova objects gains a `power` argument, to calculate
  the power for each parameter.

* `ci()` for models from *lme4* and *glmmTMB* can now computed profiled
  confidence intervals, using `method = "profile"`. Consequently,
  `model_parameters()` with `df_method = "profile"` also computes profiled
  confidence intervals. For models of class `glmmTMB`, option `"uniroot"` is
  also available.

## Bug fixes

* `model_parameters()` for t-tests when `standardize_d = TRUE`, did not return
  columns for the group-specific means.

* Fixed issue in `p_value()` for `fixest::feols()`.

* Fixed issue in `model_parameters()` for `glmer()` models with p-values that
  were calculated with `df_method = "ml1"` or `df_method = "betwithin"`.

* Fixed issue in `model_parameters()` for multinomial models when response was a
  character vector (and no factor).

* Fixed issue in `print_md()` for model-parameters objects from Bayesian
  models.

* Fixed issues with printing of model parameters for multivariate response
  models from *brms*.

* Fixed issue with paired t-tests and `model_parameters()`.

# parameters 0.10.1

## New functions

* `format_p_adjust()`, to create pretty names for p-adjustment methods.

## Bug fixes

* Fixed breaking code / failing tests due to latest _effectsize_ update.

* Fixed issue with `model_parameters()` for models of class `mlm`.

* Undocumented arguments `digits`, `ci_digits` and `p_digits` worked for
  `print()`, but not when directly called inside `model_parameters()`. Now,
  `model_parameters(model, digits = 5, ci_digits = 8)` works again.

* Fixed some minor printing-issues.

# parameters 0.10.0

## Breaking changes

* The default-method for effect sizes in `model_parameters()` for Anova-models
  (i.e. when arguments `omega_squared`, `eta_squared` or `epsilon_squared` are
  set to `TRUE`) is now `"partial"`, as initially intended.

* Column names for degrees of freedom were revised. `"df_residual"` was replaced
  by the more generic `"df_error"`. Moreover, models of class `htest` now also
  have the column name `"df_error"` and no longer `"df"` (where applicable).

* Some re-exports for functions that were moved to *insight* longer ago, were
  now removed.

## New supported model classes

* `Glm` (*rms*), `mediate` (*mediation*).

* `model_parameters()` supports `Gam` models (*gam*), `ridgelm` (*MASS*),
  `htest` objects from `oneway.test()`, `chisq.test()`, `prop.test()`,
  `mcnemar.test()` and `pairwise.htest` objects, `mcmc.list` (e.g. from
  *bayesGARCH*).

## New functions

### Printing and table Formatting

* `display()`, to format output from package-functions into different formats.

* `print_md()` as an alias for `display(format = "markdown")`. This allows to
  print tabular outputs from data frames (as returned by most functions in
  _parameters_) into nicely rendered markdown tables.

* `format()`, to create a "pretty data frame" with nicer column names and
  formatted values. This is one of the worker-functions behind `print()` or
  `print_md()`.

## Changes to functions

### `model_parameters()`

* `model_parameters()` for Anova-models (of class `aov`, `anova` etc.) gains a
  `ci`-argument, to add confidence intervals to effect size parameters.

* `model_parameters()` for `htest` objects gains a `cramers_v` and `phi`
  argument, to compute effect size parameters for objects from `chisq.test()`,
  and a `standardized_D` argument, to compute effect size parameters for objects
  from `t.test()`.

* `model_parameters()` for `metafor`-models is more stable when called from
  inside functions.

* `model_parameters()` for *metaBMA*-models now includes prior information for
  the meta-parameters.

* `model_parameters()` for meta-analysis-models gains a
  `include_studies`-argument, to include or remove studies from the output.

* `model_parameters()` for gam-models now includes the residual df for smooth
  terms, and no longer the reference df.

* Slightly revised and improved the `print()` method for `model_parameters()`.

### Other functions

* `describe_distribution()` now includes the name of the centrality index in the
  `CI`-column, when `centrality = "all"`.

* `pool_parameters()` gains a `details`-argument. For mixed models, and if
  `details = TRUE`, random effect variances will also be pooled.

## Bug fixes

* Fixed issue in `ci()` for *lme* models with non-positive definite
  variance-covariance.

* Fixed issue in `model_parameters()` for `nnet::multinom()`, `lqmm::lqm()`,
  `mgcv::gam()`, and `margins::margins()` models, and models from package
  *blme*.

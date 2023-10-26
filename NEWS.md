# parameters 0.21.3

## Changes

* `principal_components()` and `factor_analysis()` now also work when argument
  `n = 1`.

* `print_md()` for `compare_parameters()` now gains more arguments, similar to
  the `print()` method.

* `bootstrap_parameters()` and `model_parameters()` now accept bootstrapped
  samples returned by `bootstrap_model()`.

* The `print()` method for `model_parameters()` now also yields a warning for
  models with logit-links when possible issues with (quasi) complete separation
  occur.

## Bug fixes

* Fixed issue in `print_html()` for objects from package _ggeffects_.

* Fixed issues for `nnet::multinom()` with wide-format response variables (using
  `cbind()`).

* Minor fixes for `print_html()` method for `model_parameters()`.

* Robust standard errors (argument `vcov`) now works for `plm` models.

# parameters 0.21.2

## Changes

* Minor improvements to factor analysis functions.

* The `ci_digits` argument of the `print()` method for `model_parameters()` now
  defaults to the same value of `digits`.

* `model_parameters()` for objects from package *marginaleffects*  now also
  accepts the `exponentiate` argument.

* The `print()`, `print_html()`, `print_md()` and `format()` methods for
  `model_parameters()` get an `include_reference` argument, to add the reference
  category of categorical predictors to the parameters table.

## Bug fixes

* Fixed issue with wrong calculation of test-statistic and p-values in
  `model_parameters()` for `fixest` models.

* Fixed issue with wrong column header for `glm` models with
  `family = binomial("identiy")`.

* Minor fixes for `dominance_analysis()`.

# parameters 0.21.1

## General

* Added support for models of class `nestedLogit` (*nestedLogit*).

## Changes to functions

* `model_parameters()` now also prints correct "pretty names" when predictors
  where converted to ordered factors inside formulas, e.g. `y ~ as.ordered(x)`.

* `model_parameters()` now prints a message when the `vcov` argument is provided
  and `ci_method` is explicitly set to `"profile"`. Else, when `vcov` is not
  `NULL` and `ci_method` is `NULL`, it defaults to `"wald"`, to return confidence
  intervals based on robust standard errors.

# parameters 0.21.0

## Breaking Changes

* It is no longer possible to calculate Satterthwaite-approximated degrees of
  freedom for mixed models from package *nlme*. This was based on the
  *lavaSearch2* package, which no longer seems to support models of class `lme`.

## Changes to functions

* Improved support for objects of class `mipo` for models with ordinal or
  categorical outcome.

# parameters 0.20.3

## General

* Added support for models of class `hglm` (*hglm*), `mblogit` (*mclogit*),
  `fixest_multi` (*fixest*), and `phylolm` / `phyloglm` (*phylolm*).

* `as.data.frame` methods for extracting posterior draws via `bootstrap_model()`
  have been retired. Instead, directly using `bootstrap_model()` is recommended.
  
## Changes to functions

* `equivalence_test()` gets a method for `ggeffects` objects from package
  *ggeffects*.

* `equivalence_test()` now prints the `SGPV` column instead of `% in ROPE`.
  This is because the former `% in ROPE` actually was equivalent to the second
  generation p-value (SGPV) and refers to the proportion of the _range_ of the
  confidence interval that is covered by the ROPE. However, `% in ROPE` did
  not refer to the probability mass of the underlying distribution of a confidence
  interval that was covered by the ROPE, hence the old column name was a bit
  misleading.

* Fixed issue in `model_parameters.ggeffects()` to address forthcoming changes
  in the _ggeffects_ package.

## Bug fixes

* When an invalid or not supported value for the `p_adjust` argument in
  `model_parameters()` is provided, the valid options were not shown in correct
  capital letters, where appropriate.

* Fixed bug in `cluster_analysis()` for `include_factors = TRUE`.

* Fixed warning in `model_parameters()` and `ci()` for models from package
  *glmmTMB* when `ci_method` was either `"profile"` or `"uniroot"`.

# parameters 0.20.2

## General

* Reduce unnecessary warnings.

* The deprecated argument `df_method` in `model_parameters()`was removed.

* Output from `model_parameters()` for objects returned by `manova()` and
  `car::Manova()` is now more consistent.

## Bug fix

* Fixed issues in tests for `mmrm` models.

* Fixed issue in `bootstrap_model()` for models of class `glmmTMB` with
  dispersion parameters.

* Fixed failing examples.

# parameters 0.20.1

## General

* Added support for models of class `flic` and `flac` (*logistf*), `mmrm` (*mmrm*).

## Changes

* `model_parameters()` now includes a `Group` column for `stanreg` or `brmsfit`
  models with random effects.

* The `print()` method for `model_parameters()` now uses the same pattern to
  print random effect variances for Bayesian models as for frequentist models.

## Bug fix

* Fixed issue with the `print()` method for `compare_parameters()`, which
  duplicated random effects parameters rows in some edge cases.

* Fixed issue with the `print()` method for `compare_parameters()`, which
  didn't work properly when `ci=NULL`.

# parameters 0.20.0

## Breaking

* The deprecated argument `df_method` in `model_parameters()` is now defunct
  and throws an error when used.

* The deprecated functions `ci_robust()`, `p_robust()` and `standard_error_robust`
  have been removed. These were superseded by the `vcov` argument in `ci()`,
  `p_value()`, and `standard_error()`, respectively.

* The `style` argument in `compare_parameters()` was renamed into `select`.

## New functions

* `p_function()`, to print and plot p-values and compatibility (confidence)
  intervals for statistical models, at different levels. This allows to see
  which estimates are most compatible with the model at various compatibility
  levels.

* `p_calibrate()`, to compute calibrated p-values.

## Changes

* `model_parameters()` and `compare_parameters()` now use the unicode character
  for the multiplication-sign as interaction mark (i.e. `\u00d7`). Use
  `options(parameters_interaction = <value>)` or the argument `interaction_mark`
  to use a different character as interaction mark.

* The `select` argument in `compare_parameters()`, which is used to control the
  table column elements, now supports an experimental glue-like syntax.
  See this vignette _Printing Model Parameters_. Furthermore, the `select`
  argument can also be used in the `print()` method for `model_parameters()`.

* `print_html()` gets a `font_size` and `line_padding` argument to tweak the
  appearance of HTML tables. Furthermore, arguments `select` and `column_labels`
  are new, to customize the column layout of tables. See examples in `?display`.

* Consolidation of vignettes on standardization of model parameters.

* Minor speed improvements.

## Bug fix

* `model_parameters().BFBayesFactor` no longer drops the `BF` column if the
  Bayes factor is `NA`.

* The `print()` and `display()` methods for `model_parameters()` from Bayesian
  models now pass the `...` to `insight::format_table()`, allowing extra
  arguments to be recognized.

* Fixed footer message regarding the approximation method for CU and p-values
  for mixed models.

* Fixed issues in the `print()` method for `compare_parameters()` with mixed
  models, when some models contained within-between components (see
  `wb_component`) and others did not.

# parameters 0.19.0

## Breaking

* Arguments that calculate effectsize in `model_parameters()` for `htest`,
  Anova objects and objects of class `BFBayesFactor` were revised. Instead of
  single arguments for the different effectsizes, there is now one argument,
  `effectsize_type`. The reason behind this change is that meanwhile many
  new type of effectsizes have been added to the _effectsize_ package, and
  the generic argument allows to make use of those effect sizes.

* The attribute name in PCA / EFA has been changed from `data_set` to `dataset`.

* The minimum needed R version has been bumped to `3.6`.

* Removed deprecated argument `parameters` from `model_parameters()`.

* `standard_error_robust()`, `ci_robust()` and `p_value_robust()` are now
  deprecated and superseded by the `vcov` and `vcov_args` arguments in the
  related methods `standard_error()`, `ci()` and `p_value()`, respectively.

* Following functions were moved from package *parameters* to *performance*:
  `check_sphericity_bartlett()`, `check_kmo()`, `check_factorstructure()` and
  `check_clusterstructure()`.
  
## Changes to functions

* Added `sparse` option to `principal_components()` for sparse PCA.

* The `pretty_names` argument from the `print()` method can now also be
  `"labels"`, which will then use variable and value labels (if data is
  labelled) as pretty names. If no labels were found, default pretty names
  are used.

* `bootstrap_model()` for models of class `glmmTMB` and `merMod` gains a
  `cluster` argument to specify optional clusters when the `parallel`
  option is set to `"snow"`.

* P-value adjustment (argument `p_adjust` in `model_parameters()`) is now
  performed after potential parameters were removed (using `keep` or `drop`),
  so adjusted p-values is only applied to the parameters of interest.

* Robust standard errors are now supported for `fixest` models with the `vcov`
  argument.

* `print()` for `model_parameters()` gains a `footer` argument, which can be
  used to suppress the footer in the output. Further more, if `footer = ""`
  or `footer = FALSE` in `print_md()`, no footer is printed.

* `simulate_model()` and `simulate_parameters()` now pass `...` to
  `insight::get_varcov()`, to allow simulated draws to be based on
  heteroscedasticity consistent variance covariance matrices.

* The `print()` method for `compare_parameters()` was improved for models with
  multiple components (e.g., mixed models with fixed and random effects, or
  models with count- and zero-inflation parts). For these models,
  `compare_parameters(effects = "all", component = "all")` prints more nicely.

## Bug fixes

* Fix erroneous warning for *p*-value adjustments when the differences between
  original and adjusted *p*-values were very small.

# parameters 0.18.2

## New functions

* New function `dominance_analysis()`, to compute dominance analysis
  statistics and designations.

## Changes to functions

* Argument `ci_random` in `model_parameters()` defaults to `NULL`. It uses a
  heuristic to determine if random effects confidence intervals are likely to
  take a long time to compute, and automatically includes or excludes those
  confidence intervals. Set `ci_random` to `TRUE` or `FALSE` to explicitly
  calculate or omit confidence intervals for random effects.

## Bug fixes

* Fix issues in `pool_parameters()` for certain models with special components
  (like `MASS::polr()`), that failed when argument `component` was set to
  `"conditional"` (the default).

* Fix issues in `model_parameters()` for multiple imputation models from
  package *Hmisc*.

# parameters 0.18.1

## General

* It is now possible to hide messages about CI method below tables by specifying
  `options("parameters_cimethod" = FALSE)` (#722). By default, these messages
  are displayed.

* `model_parameters()` now supports objects from package _marginaleffects_ and
  objects returned by `car::linearHypothesis()`.

* Added `predict()` method to `cluster_meta` objects.

* Reorganization of docs for `model_parameters()`.

## Changes to functions

* `model_parameters()` now also includes standard errors and confidence
  intervals for slope-slope-correlations of random effects variances.

* `model_parameters()` for mixed models gains a `ci_random` argument, to toggle
  whether confidence intervals for random effects parameters should also be
  computed. Set to `FALSE` if calculation of confidence intervals for random
  effects parameters takes too long.

* `ci()` for *glmmTMB* models with `method = "profile"` is now more robust.

## Bug fixes

* Fixed issue with *glmmTMB* models when calculating confidence
  intervals for random effects failed due to singular fits.

* `display()` now correctly includes custom text and additional information
  in the footer (#722).

* Fixed issue with argument `column_names` in `compare_parameters()` when
  strings contained characters that needed to be escaped for regular expressions.

* Fixed issues with unknown arguments in `model_parameters()` for *lavaan* models
  when `standardize = TRUE`.

# parameters 0.18.0

## Breaking Changes

* `model_parameters()` now no longer treats data frame inputs as posterior samples.
  Rather, for data frames, now `NULL` is returned. If you want to treat a data
  frame as posterior samples, set the new argument `as_draws = TRUE`.

## New functions

* `sort_parameters()` to sort model parameters by coefficient values.

* `standardize_parameters()`, `standardize_info()` and `standardise_posteriors()`
  to standardize model parameters.

## Changes to functions

### `model_parameters()`

* `model_parameters()` for mixed models from package *lme4* now also reports
  confidence intervals for random effect variances by default. Formerly, CIs
  were only included when `ci_method` was `"profile"` or `"boot"`. The
  *merDeriv* package is required for this feature.

* `model_parameters()` for `htest` objects now also supports models from
  `var.test()`.

* Improved support for `anova.rms` models in `model_parameters()`.

* `model_parameters()` now supports `draws` objects from package *posterior*
  and `deltaMethods` objects from package *car*.

* `model_parameters()` now checks arguments and informs the user if specific
  given arguments are not supported for that model class (e.g., `"vcov"` is
  currently not supported for models of class *glmmTMB*).

## Bug fixes

* The `vcov` argument, used for computing robust standard errors, did not
  calculate the correct p-values and confidence intervals for models of class
  `lme`.

* `pool_parameters()` did not save all relevant model information as attributes.

* `model_parameters()` for models from package *glmmTMB* did not work when
  `exponentiate = TRUE` and model contained a dispersion parameter that was
  different than sigma. Furthermore, exponentiating falsely exponentiated the
  dispersion parameter.

# parameters 0.17.0

## General

* Added options to set defaults for different arguments. Currently supported:
  - `options("parameters_summary" = TRUE/FALSE)`, which sets the default value
    for the `summary` argument in `model_parameters()` for non-mixed models.
  - `options("parameters_mixed_summary" = TRUE/FALSE)`, which sets the default
    value for the `summary` argument in `model_parameters()` for mixed models.

* Minor improvements for `print()` methods.

* Robust uncertainty estimates:
  - The `vcov_estimation`, `vcov_type`, and `robust` arguments are deprecated in
    these functions: `model_parameters()`, `parameters()`, `standard_error()`,
    `p_value()`, and `ci()`. They are replaced by the `vcov` and `vcov_args`
    arguments.
  - The `standard_error_robust()` and `p_value_robust()` functions are superseded
    by the `vcov` and `vcov_args` arguments of the `standard_error()` and
    `p_value()` functions.
  - Vignette: https://easystats.github.io/parameters/articles/model_parameters_robust.html

## Bug fixes

* Fixed minor issues and edge cases in `n_clusters()` and related cluster
  functions.

* Fixed issue in `p_value()` that returned wrong p-values for `fixest::feols()`.

# parameters 0.16.0

## General

* Improved speed performance for `model_parameters()`, in particular for glm's
  and mixed models where random effect variances were calculated.

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

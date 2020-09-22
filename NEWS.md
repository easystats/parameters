# parameters 0.8.7

## Changes to functions

- `print()` for `model_parameters()` now names the coefficients column depending on the model type (i.e. `"Odds Ratios"` for logistic regression when `exponentiate = TRUE` etc.)

# parameters 0.8.6

## Bug fixes

* Fixed issues with *glmmTMB* models with dispersion-parameter.
* Fixed issue where `model_parameters()` for *glmmTMB* models falsely removed the `Component` column.
* Fixed issue with missing CI columns in `model_parameters()` when `standardize` was one of the options except `"refit"`.
* `parameters_type()` did not correctly detect interaction terms for specific patterns like `scale()` included in the interaction.

# parameters 0.8.5

## General

* Added vignette on model parameters and missing data.
* Update citation.

## New supported model classes

* Support for `mipo` (*mice*), `lqm` and `lqmm` (*lqmm*). Preliminary support for `semLME` (*smicd*), `mle2` (*bbmle*), `mle` (*stats4*)
* `model_parameters()` for objects of class `mira` (*mice*).

## Changes to functions

* `model_parameters()` gets a specific behaviour for brms-meta-analysis models.
* `model_parameters()` for *lavaan* and *blavaan* now also prints self-defined parameters.
* `model_parameters()` for *lavaan* and *blavaan* gains more option for standardized parameters.

## Bug fixes

* Fix issue in `model_parameters()` for `coxph.penal` models.
* Fix issue in `model_parameters.metaplus()` with random effects.
* Fix issue in `check_heterogeneity()` when `x` was a mixed model.
* Fix issue in `check_heterogeneity()` for data with missing values.
* Fix issue in `dof_ml1()` when random-effect terms where character vectors.
* Fix issue in `print()` method for `model_parameters()` that printed empty lines for rows with complete missing values. Empty lines are now removed.
* Fix issue in `parameters_type()` when `exp()` was used in a model formula.

# parameters 0.8.2

## New supported models

* `metaplus` (*metaplus*), `glht` (*multcomp*), `glmm`  (*glmm*), `manova` (*stats*), `crq` and `crqs` (*quantreg*)
* Improved support for models from the *rms*  package.

## Changes to functions

* Improved parameters formatting for ordered factors in `model_parameters()` (and `format_parameters()`).
* Argument `df_method` can now also be applied to GLMs, to allow calculation of confidence intervals based on Wald-approximation, not profiled confidence intervals. This speeds up computation of CIs for models fit to large data sets.
* Improved `select_parameters()` for mixed models, and revised docs and associated vignette.

## Bug fixes

* Allow `threshold` to be passed to `efa_to_cfa()` when the model is from `factor_analysis()`.
* Allow correlation matrix to be passed to `factor_analysis()`.
* Fix CRAN check issues.
* Fix issue in `model_parameters()` for models with non-estimable parameters or statistics.
* Fix issue in `model_parameters()` for *plm* models with only one parameter.
* Fix issue in `check_heterogeneity()` in case no predictor would cause heterogeneity bias.
* Make sure *clubSandwich* is used conditionally in all places, to properly pass CRAN checks.

# parameters 0.8.0

## New supported models

* `robmixglm` (*robmixglm*), `betaor`, `betamfx`, `logitor`, `poissonirr`, `negbinirr`, `logitmfx`, `probitmfx`, `poissonmfx`, `negbinmfx` (*mfx*), partial support `emmGrid` (*emmeans*)

## Changes to functions

### `simulate_parameters()` and `simulate_model()`

* has a nicer `print()` method.
* now also simulate parameters from the dispersion model for *glmmTMB* objects.
* gets a `verbose` argument, to show or hide warnings and messages.

## Bug fixes

* fix issue with rank deficient models.

# parameters 0.7.0

## General

* We changed the computation of confidence intervals or standard errors, so these are now based on a t-distribution with degrees of freedom and not normal distribution assuming infinite degrees of freedom. This was implemented for most functions before and only affects few functions (like `equivalence_test()` or CIs for standardized parameters from `model_parameters()` when standardization method was `"posthoc"`).

## New supported models

* `averaging` (*MuMIn*), `bayesx` (*R2BayesX*), `afex_aov` (*afex*)

## New functions

* `check_heterogeneity()` as a small helper to find variables that have a within- and between-effect related to a grouping variable (and thus, may result in heterogeneity bias, see [this vignette](https://easystats.github.io/parameters/articles/demean.html)).

## Changes to functions

### `equivalence_test()`

* gains a `rule` argument, so equivalence testing can be based on different approaches.
* for mixed models gains an `effect` argument, to perform equivalence testing on random effects.
* gains a `p_values` argument, to calculate p-values for the equivalence test.
* now supports more frequentist model objects.

### `describe_distribution()`

* now works on grouped data frames.
* gains `ci` and `iterations` arguments, to compute confidence intervals based on bootstrapping.
* gains a `iqr` argument, to compute the interquartile range.
* `SE` column was removed.

### `model_parameters()`

* `model_parameters()` for Stan-models (*brms*, *rstanarm*) gains a `group_level` argument to show or hide parameters for group levels of random effects.
* Improved accuracy of confidence intervals in `model_parameters()` with `standardize = "basic"` or `standardize = "posthoc"`.
* `model_parameters.merMod()` no longer passes `...` down to bootstrap-functions (i.e. when `bootstrap = TRUE`), as this might conflict with `lme4::bootMer()`.
* For ordinal models (like `MASS::polr()` or `ordinal::clm()`), a `Component` column is added, indicating intercept categories (`"alpha"`) and estimates (`"beta"`).
* The `select`-argument from `print.parameters_model()` now gets a `"minimal"`-option as shortcut to print coefficients, confidence intervals and p-values only.

### Other changes

* `parameters_table()` and `print.parameters_model()` now explicitly get arguments to define the digits for decimal places used in output.
* `ci()`, `standard_error()`, `p_value()` and `model_parameters()` for *glmmTMB* models now also works for dispersion models.

## Bug fixes

* Fixed issue in `equivalence_test()` for mixed models.
* Fixed bug for `model_parameters.anova(..., eta_squared = "partial")` when called with non-mixed models.
* Fixed issue with wrong degrees of freedom in `model_parameters()` for *gam* models.
* Fixed issue with unused arguments in `model_parameters()`.

# parameters 0.6.1

## General

* Remove 'Zelig' from suggested packages, as it was removed from CRAN.

## Changes to functions

### model_parameters()

* `model_parameters()` now also transforms standard errors when `exponentiate = TRUE`.
* `model_parameters()` for `anova()` from mixed models can now also compute effect sizes like eta squared.
* `model_parameters()` for `aov()` gains a `type`-argument to compute type-1, type-2 or type-3 sums of squares.
* `model_parameters()` for Bayesian models gains a `standardize` argument, to return standardized parameters from the posterior distribution.
* Improved `print()` method for `model_parameters()` for nested `aov()` (repeated measurements).
* You can now control whether `demean()` should add attributes to indicate within- and between-effects. This is only relevant for the `print()`-method of `model_parameters()`.

## Bug fixes

* Fixed `model_parameters()` for `anova()` from *lmerTest* models.

# parameters 0.6.0

## Breaking changes

- Alias `model_bootstrap()` was removed, please use `bootstrap_model()`.
- Alias `parameters_bootstrap()` was removed, please use `bootstrap_parameters()`.
- Alias `model_simulate()` was removed, please use `simulate_model()`.
- Alias `parameters_simulate()` was removed, please use `simulate_parameters()`.
- Alias `parameters_selection()` was removed, please use `select_parameters()`.
- Alias `parameters_reduction()` was removed, please use `reduce_parameters()`.
- Functions `DDR()`, `ICA()` and `cmds()` are no longer exported, as these were intended to be used internally by `reduce_parameters()` only.
- `skewness()` and `kurtosis()` always return a data frame.

## New supported models

- Added support for `arima` (*stats*), `bife` (*bife*), `bcplm` and `zcpglm` (*cplm*)

## Changes to functions

### model_parameters()

- Improved print-method for `model_parameters.brmsfit()`.
- Improved print-method for `model_parameters.merMod()` when fitting REWB-Models (see `demean()`).
- Improved efficiency for `model_parameters()` (for linear mixed models) when `df_method = "kenward"`.
- `model_parameters()` gets a `p_adjust`-argument, to adjust p-values for multiple comparisons.
- Minor improvements for `cluster_analysis()` when `method = "kmeans"` and `force = TRUE` (factors now also work for kmeans-clustering).

### p_value(), ci() and standard_error()

- `p_value_kenward()`, `se_kenward()` etc. now give a warning when model was not fitted by REML.
- Added `ci()`, `standard_error()` and `p_value()` for *lavaan* and *blavaan* objects.
- Added `standard_error()` for *brmsfit* and *stanreg* objects.

### Other changes

- Run certain tests only locally, to reduce duration of CRAN checks.
- `skewness()`, `kurtosis()` and `smoothness()` get an `iteration` argument, to set the numbers of bootstrap replicates for computing standard errors.
- Improved print-method for `factor_analysis()`.
- `demean()` now additionally converts factors with more than 2 levels to dummy-variables (binary), to mimic *panelr*-behaviour.

## Bug fixes

- Fixed minor issue with the `print()`-method for `model_parameters.befa()`.
- Fixed issues in `model_parameters()` (for linear mixed models) with wrong order of degrees of freedom when `df_method` was different from default.
- Fixed issues in `model_parameters()` (for linear mixed models) with accuracy of p-values when `df_method = "kenward`.
- Fixed issues in `model_parameters()` with wrong test statistic for *lmerModLmerTest* models.
- Fixed issue in `format_parameters()` (which is used to format output of `model_parameters()`) for factors, when variable name was also part of factor levels.
- Fixed issue in `degrees_of_freedem()` for *logistf*-models, which unintentionally printed the complete model summary.
- Fixed issue in `model_parameters()` for *mlm* models.
- Fixed issue in `random_parameters()` for uncorrelated random effects.

# parameters 0.5.0

## Breaking changes

- `skewness()` now uses a different method to calculate the skewness by default. Different methods can be selected using the `type`-argument.
- `kurtosis()` now uses a different method to calculate the skewness by default. Different methods can be selected using the `type`-argument.

## New supported models

- Added support for `cglm` (*cglm*), `DirichletRegModel` (*DirichletReg*)

## General

- Added new vignettes on 'Standardized Model Parameters' and 'Robust Estimation of Standard Errors', and vignettes are now also published on CRAN.
- Improved handling of robust statistics in `model_parameters()`. This should now work for more models than before.
- Improved accuracy of `ci.merMod()` for `method = "satterthwaite"` and `method = "kenward"`.
- `select_parameters()` for *stanreg* models, which was temporarily removed due to the CRAN removal of package **projpred**, is now re-implemented.

## New functions

- `dof_betwithin()` to compute degrees of freedom based on a between-within approximation method (and related to that, `p_value_*()` and `se_*()` for this method were added as well).
- `random_parameters()` that returns information about the random effects such as variances, R2 or ICC.
- `closest_component()` as a small helper that returns the component index for each variable in a data frame that was used in `principal_components()`.
- `get_scores()` as a small helper to extract scales and calculate sum scores from a principal component analysis (PCA, `principal_components()`).

## Changes to functions

- `n_clusters()` gets the option `"M3C"` for the `package`-argument, so you can try to determine the number of cluster by using the `M3C::M3C()` function.
- The `print()`-method for `model_parameters()` gets a `select`-argument, to print only selected columns of the parameters table.
- `model_parameters()` for meta-analysis models has an improved `print()`-method for subgroups (see examples in `?model_parameters.rma`).
- `model_parameters()` for mixed models gets a `details`-argument to additionally print information about the random effects.
- `model_parameters()` now accepts the `df_method`-argument for more (mixed) models.
- The Intercept-parameter in `model_parameters()` for meta-analysis models was renamed to `"Overall"`.
- `skewness()` gets a `type`-argument, to compute different types of skewness.
- `kurtosis()` gets a `type`-argument, to compute different types of skewness.
- `describe_distribution()` now also works on data frames and gets a nicer print-method.

## Bug fixes

- Fixed issue in `model_parameters()` when `robust = TRUE`, which could sometimes mess up order of the statistic column.
- Fixed issues in `model_parameters()` with wrong `df` for `lme`-models.
- Fixed issues in `model_parameters.merMod()` when `df_method` was not set to default.
- Fixed issues in `model_parameters.merMod()` and `model_parameters.gee()` when `robust = TRUE`.
- Fixed issues with *coxph* models with only one parameter.
- Fixed issue in `format_p()` when argument `digits` was `"apa"`.
- Fixed issues in `model_parameters()` for `zeroinfl`-models.

# parameters 0.4.1

## Bug fixes

- Fix CRAN check issues, caused by removal of package 'projpred'.

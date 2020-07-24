# parameters 0.8.1

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

# parameters 0.4.0

## Breaking changes

- The column for degrees of freedom in `model_parameters()` was renamed from `df_residuals` to `df_error` for regression model objects, because these degrees of freedom actually were not always referring to _residuals_ - we consider `df_error` as a more generic name.
- `model_parameters()` for standardized parameters (i.e. `standardize` is not `NULL`) only returns standardized coefficients, CI and standard errors (and not both, unstandardized and standardized values).
- `format_ci()` was removed and re-implemented in the **insight** package.

## Renaming

- `model_bootstrap()` was renamed to `bootstrap_model()`. `model_bootstrap()` will remain as alias.
- `parameters_bootstrap()` was renamed to `bootstrap_parameters()`. `parameters_bootstrap()` will remain as alias.
- `model_simulate()` was renamed to `simulate_model()`. `model_simulate()` will remain as alias.
- `parameters_simulate()` was renamed to `simulate_parameters()`. `parameters_simulate()` will remain as alias.
- `parameters_selection()` was renamed to `select_parameters()`. `parameters_selection()` will remain as alias.
- `parameters_reduction()` was renamed to `reduce_parameters()`. `parameters_reduction()` will remain as alias.

## New supported models

- Added support for `vgam` (*VGAM*), `cgam`, `cgamm` (*cgam*), `complmrob` (*complmrob*), `cpglm`, `cpglmm` (*cplm*), `fixest` (*fixest*), `feglm` (*alpaca*), `glmx` (*glmx*), `glmmadmb` (*glmmADMB*), `mcmc` (*coda*), `mixor` (*mixor*).
- `model_parameters()` now supports `blavaan` models (*blavaan*).

## General

- Better handling of `clm2`, `clmm2` and `stanmvreg` models.
- Better handling of `psych::omega` models.

## New functions

- `dof_satterthwaite()` and `dof_ml1()` to compute degrees of freedom based on different approximation methods (and related to that, `p_value_*()` and `se_*()` for these methods were added as well).
- `rescale_weights()` to rescale design (probability or sampling) weights for use in multilevel-models without survey-design.

## Changes to functions

- Robust estimation (like `standard_error_robust()` or `ci_robust()`) can now also compute cluster-robust variance-covariance matrices, using the *clubSandwich*  package.
- `model_parameters()` gets a `robust`-argument, to compute robust standard errors, and confidence intervals and p-values based on robust standard errors.
- Arguments `p_method` and `ci_method` in `model_parameters.merMod()` were replaced by a single argument `df_method`.
- `model_parameters.principal()` includes a `MSA` column for objects from `principal_components()`.

## Bug fixes

- Fixed issue in `model_parameters()` with non-typical ordering of coefficients for mixed models.
- Fixed issues with models of class `rlmerMod`.
- Fixed minor issues `model_parameters.BFBayesFactor()`.

# parameters 0.3.0

## Breaking changes

Parts of the **parameter** package are restructured and functions focussing on anything related to effect sizes are now re-implemented in a new package, [**effectsize**](https://github.com/easystats/effectsize). In details, following breaking changes have been made:

- Functions for computing effect sizes (`cohens_f()`, `eta_squared()` etc.) have been removed and are now re-implemented in the **effectsize**-package.
- Functions for converting effect sizes (`d_to_odds()` etc.) have been removed and are now re-implemented in the **effectsize**-package.
- `standardize()` and `normalize()` (and hence, also `parameters_standardize()`) have been removed ;-( and are now re-implemented in the **effectsize**-package.

## New supported models

- Added support for `aareg` (*survival*), `bracl`, `brmultinom` (*brglm2*), `rma` (*metafor*) and `multinom` (*nnet*) to various functions.
- `model_parameters()` for `kmeans`.
- `p_value()`, `ci()`, `standard_error()` and `model_parameters()` now support *flexsurvreg* models (from package **flexsurv**).

## New functions

- `degrees_of_freedom()` to get DoFs.
- `p_value_robust()`, `ci_robust()` and `standard_error_robust()` to compute robust standard errors, and p-values or confidence intervals based on robust standard errors.
- `demean()` to calculate de-meaned and group-meaned variables (centering within groups, for panel-data regression).
- `n_parameters()` to get number of parameters.
- `n_clusters()` to determine the number of clusters to extract.
- `cluster_analysis()` to return group indices based on cluster analysis.
- `cluster_discrimination()` to determine the goodness of classification of cluster groups.
- `check_clusterstructure()` to check the suitability of data for clustering.
- `check_multimodal()` to check if a distribution is unimodal or multimodal.
- Add `plot()`-methods for `principal_components()`.

## Changes to functions

- Added indices of model fit to `n_factors()` ([Finch, 2019](https://doi.org/10.1177/0013164419865769))
- `standard_error()` for mixed models gets an `effects` argument, to return standard errors for random effects.
- The `method`-argument for `ci()` gets a new option, `"robust"`, to compute confidence intervals based on robust standard errors. Furthermore, `ci_wald()` gets a `robust`-argument to do the same.
- `format_p()` gets a `digits`-argument to set the amount of digits for p-values.
- `model_parameters()` now accepts (non-documented) arguments `digits`, `ci_digits` and `p_digits` to change the amount and style of formatting values. See [examples in `model_parameters.default()`](https://easystats.github.io/parameters/reference/model_parameters.default.html).
- Improved `print()` method for `model_parameters()` when used with Bayesian models.
- Added further method (gap-statistic) to `n_clusters()`.

## Bug fixes

- Interaction terms in `model_parameters()` were denoted as nested interaction when one of the interaction terms was surrounded by a function, e.g. `as.factor()`, `log()` or `I()`.
- Fixed bug in `parameters_type()` when a parameter occured multiple times in a model.
- Fixed bug with *multinom*-support.
- Fixed bug in `model_parameters()` for non-estimable GLMs.
- Fixed bug in `p_value()` for *MASS::rlm* models.
- Fixed bug in `reshape_loadings()` when converting loadings from wide to long and back again.

# parameters 0.2.0

## Breaking changes

- `format_value()` and `format_table()` have been removed and are now re-implemented in the **insight** package.

## General

- `parameters()` is an alias for `model_parameters()`.
- `p_value()`, `ci()`, `standard_error()`, `standardize()` and `model_parameters()` now support many more model objects, including mixed models from packages *nlme*, *glmmTMB* or *GLMMadaptive*, zero-inflated models from package *pscl* or other modelling packages. Along with these changes, functions for specific model objects with zero-inflated component get a `component`-argument to return the requested values for the complete model, the conditional (count) component or the zero-inflation component from the model only.

## New functions

- `parameters_simulate()` and `model_simulate()`, as computational faster alternatives to `parameters_bootstrap()` and `model_bootstrap()`.
- `data_partition()` to partition data into a test and a training set.
- `standardize_names()` to standardize column names from data frames, in particular objects returned from `model_parameters()`.
- `se_kenward()` to calculate approximated standard errors for model parameters, based on the Kenward-Roger (1997) approach.

## Changes to functions

- `format_value()` and `format_ci()` get a `width`-argument to set the minimum length of the returned formatted string.
- `format_ci()` gets a `bracket`-argument include or remove brackets around the ci-values.
- `eta_squared()`, `omega_squared()`, `epsilon_squared()` and `cohens_f()` now support more model objects.
- The `print()`-method for `model_parameters()` now better aligns confidence intervals and p-values.
- `normalize()` gets a `include_bounds`-argument, to compress normalized variables so they do not contain zeros or ones.
- The `method`-argument for `ci.merMod()` can now also be `"kenward"` to compute confidence intervals with degrees of freedom based on the Kenward-Roger (1997) approach.

## Bug fixes

- Fixed issue with wrong computation of wald-approximated confidence intervals.
- Fixed issue with wrong computation of degrees of freedom for `p_value_kenward()`.
- `paramerers_standardize()` resp. `standardize()` for model objects now no longer standardizes `log()` terms, count or ratio response variables, or variables of class `Surv` and `AsIs`.

# parameters 0.1.0

- Added a `NEWS.md` file to track changes to the package

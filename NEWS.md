# parameters 0.3.0

## Breaking changes

- Functions for computing effect sizes (`cohens_f()`, `eta_squared()` etc.) have been removed and are now re-implemented in the [**effectsize**-package](https://github.com/easystats/effectsize).
- Functions for converting effect sizes (`d_to_odds()` etc.) have been removed and are now re-implemented in the [**effectsize**-package](https://github.com/easystats/effectsize).
- `standardize()` and `normalize()` (and hence, also `parameters_standardize()`) have been removed ;-( and are now re-implemented in the [**effectsize**-package](https://github.com/easystats/effectsize).

## New supported models

- `model_parameters()` for `kmeans`.
- `p_value()`, `ci()`, `standard_error()` and `model_parameters()` now support *flexsurvreg* models (from package **flexsurv**).

## New functions

- `n_parameters()`: function to get number of parameters.
- `degrees_of_freedom()`: function to get DoFs.
- Added indices of model fit to `n_factors` ([Finch, 2019](https://doi.org/10.1177/0013164419865769))
- `check_clusterstructure()` to check the suitability of data for clustering.
- `n_clusters()` to determine the number of clusters to extract.
- `check_multimodal()` to check if a distribution is unimodal or multimodal.
- `p_value_robust()`, `ci_robust()` and `standard_error_robust()` to compute robust standard errors, and p-values or confidence intervals based on robust standard errors.
- The `method`-argument for `ci()` gets a new option, `"robust"`, to compute confidence intervals based on robust standard errors. Furthermore, `ci_wald()` gets a `robust`-argument to do the same.
- Add `plot()`-methods for `principal_components()`.

## Bug fixes

- Interaction terms in `model_parameters()` were denoted as nested interaction when one of the interaction terms was surrounded by a function, e.g. `as.factor()`, `log()` or `I()`.

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

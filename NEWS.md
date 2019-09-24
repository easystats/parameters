# parameters 0.2.0

## Breaking changes

- `format_value()` and `format_table()` have been removed and are now re-implemented in the **insight** package.

## General

- `parameters()` is an alias for `model_parameters()`.
- `p_value()`, `ci()`, `standard_error()` and `model_parameters()` now support more model objects, including mixed models from packages *nlme*, *glmmTMB* or *GLMMadaptive* or zero-inflated models from package *pscl*. Along with these changes, functions for specific model objects with zero-inflated component get a `component`-argument to return the requested values for the complete model, the conditional (count) component or the zero-inflation component from the model only.

## New functions

- `parameters_simulate()` and `model_simulate()`, as computational faster alternatives to `parameters_bootstrap()` and `model_bootstrap()`.
- `standardize_names()` to standardize column names from data frames, in particular objects returned from `model_parameters()`.

## Changes to functions

- `format_value()` and `format_ci()` get a `width`-argument to set the minimum length of the returned formatted string.
- `format_ci()` gets a `bracket`-argument include or remove brackets around the ci-values.
- `eta_squared()`, `omega_squared()`, `epsilon_squared()` and `cohens_f()` now support more model objects.
- The `print()`-method for `model_parameters()` now better aligns confidence intervals and p-values.
- `normalize()` gets a `include_bounds`-argument, to compress normalized variables so they do not contain zeros or ones.

## Bug fixes

- Fixed issue with wrong computation of wald-approximated confidence intervals.
- Fixed issue with wrong computation of degrees of freedom for `p_value_kenward()`.
- `paramerers_standardize()` resp. `standardize()` for model objects now no longer standardizes `log()` terms, count or ratio response variables, or variables of class `Surv` and `AsIs`.

# parameters 0.1.0

- Added a `NEWS.md` file to track changes to the package

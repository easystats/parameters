# parameters 0.1.1

## New functions

  - `parameters_simulate()` and `model_simulate()`, as computational faster alternatives to `parameters_bootstrap()` and `model_bootstrap()`.

## Changes to functions

  - `format_value()` and `format_ci()` get a `width`-argument to set the minimum length of the returned formatted string.
  - `p_value()` now support more model objects.
  - `eta_squared()`, `omega_squared()`, `epsilon_squared()` and `cohens_f()` now support more model objects.
  - The `print()`-method for `model_parameters()` now better aligns confidence intervals and p-values.

# parameters 0.1.0

- Added a `NEWS.md` file to track changes to the package

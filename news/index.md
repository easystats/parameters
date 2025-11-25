# Changelog

## parameters 0.28.3

CRAN release: 2025-11-25

- fixed bug in `standardize_info(<fixest>)` that was preventing
  [`standardise_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.md)
  from working for `fixest` models.

- [`equivalence_test()`](https://easystats.github.io/bayestestR/reference/equivalence_test.html)
  gets methods for objects from the *modelbased* package.

- Improved support for objects from package *survey*.

- Added support for package *lcmm*.

- Added `ci_method` options `"kenward-roger"` and `"satterthwaite"` for
  models from package *glmmTMB*. Consequently,
  [`se_kenward()`](https://easystats.github.io/parameters/reference/p_value_kenward.md),
  [`se_satterthwaite()`](https://easystats.github.io/parameters/reference/p_value_satterthwaite.md),
  [`ci_kenward()`](https://easystats.github.io/parameters/reference/p_value_kenward.md),
  [`ci_satterthwaite()`](https://easystats.github.io/parameters/reference/p_value_satterthwaite.md),
  [`p_value_kenward()`](https://easystats.github.io/parameters/reference/p_value_kenward.md)
  and
  [`p_value_satterthwaite()`](https://easystats.github.io/parameters/reference/p_value_satterthwaite.md)
  can now be used with `glmmTMB` models.

## parameters 0.28.2

CRAN release: 2025-09-10

### Bug fixes

- Updates tests to resolve issues with the latest version of the
  *fixest* package.

## parameters 0.28.1

CRAN release: 2025-08-30

### Changes

- Methods for *glmmTMB* objects
  ([`ci()`](https://easystats.github.io/bayestestR/reference/ci.html),
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md),
  [`standard_error()`](https://easystats.github.io/parameters/reference/standard_error.md))
  now support the `vcov` argument to compute robust standard errors.

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for *marginaleffects* objects is now more robust in detecting Bayesian
  models.

- Modified code base to address changes in the *marginaleffects* package
  from version 0.29.0 onwards.

### Bug fixes

- Fixed issue with
  [`equivalence_test()`](https://easystats.github.io/bayestestR/reference/equivalence_test.html)
  for models of class `glmmTMB` with
  [`beta_family()`](https://rdrr.io/pkg/glmmTMB/man/nbinom2.html).

- `exponentiate = TRUE` in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  did not exponentiate location and scale parameters for models from
  package *ordinal*.

## parameters 0.28.0

CRAN release: 2025-08-20

### Breaking Changes

- The experimental `print_table()` function was removed. The aim of this
  function was to test the implementation of the `tinytable` backend for
  printing. Now, `tinytable` is fully supported by
  [`insight::export_table()`](https://easystats.github.io/insight/reference/export_table.html)
  and thereby also by the various
  [`print()`](https://rdrr.io/r/base/print.html) resp.
  [`display()`](https://easystats.github.io/insight/reference/display.html)
  methods for model parameters.

### Changes

- All
  [`print_html()`](https://easystats.github.io/insight/reference/display.html)
  methods get an `engine` argument, to either use the `gt` package or
  the `tinytable` package for printing HTML tables. Since `tinytable`
  not only produces HTML tables, but rather different formats depending
  on the environment,
  [`print_html()`](https://easystats.github.io/insight/reference/display.html)
  may also generate a markdown table. Thus, the generic
  [`display()`](https://easystats.github.io/insight/reference/display.html)
  method can be used, too, which has a `format` argument that also
  supports `"tt"` for `tinytable`.

- Improved support for *coxme* models in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md).
  Random effects and group level estimates are now returned as well.

### Bug fixes

- Fixed issue with models of class `selection` with multiple outcomes.

## parameters 0.27.0

CRAN release: 2025-07-09

### Breaking Changes

- The `standardize` argument in
  [`factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.md)
  now defaults to `FALSE`.

- The `rotation` argument in
  [`factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.md)
  now defaults to `"oblimin"`, because the former default of `"none"`
  rarely makes sense in the context of factor analysis. If you want to
  use no rotation, please set `rotation = "none"`.

- The `cor` argument in
  [`n_factors()`](https://easystats.github.io/parameters/reference/n_factors.md)
  was renamed into `correlation_matrix`. In
  [`factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.md),
  the `cor` argument was completely removed to avoid naming collision
  with the `cor` argument of
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html), which now
  users can pass the `cor` argument to
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html) when using
  [`factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.md).

### Changes

- [`factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.md)
  gets a `.matrix` method, including a new argument `n_obs` (which can
  be a single value or a matrix of pairwise counts), to compute factor
  analysis for a correlation matrix or covariance matrix.

- New function
  [`factor_scores()`](https://easystats.github.io/parameters/reference/factor_scores.md)
  to extract factor scores from EFA
  ([`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html) or
  [`factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.md)).

- Added and/or improved print-methods for all functions around PCA, FA
  and Omega.

- Improved efficiency in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for models from packages *brms* and *rstanarm*.

- `p_adjust` for
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  gets a new options, `"sup-t"`, to calculate simultaneous confidence
  intervals.

### Bug fixes

- [`bootstrap_model()`](https://easystats.github.io/parameters/reference/bootstrap_model.md)
  did not work for intercept-only models. This has been fixed.

- Fixed issue with printing labels as pretty names for models from
  package *pscl*,
  i.e. `print(model_parameters(model), pretty_names = "labels")` now
  works as expected.

## parameters 0.26.0

CRAN release: 2025-05-22

### Changes

- The `effects` argument in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for classes `merMod`, `glmmTMB`, `brmsfit` and `stanreg` gets an
  additional `"grouplevel"` option, to return the group-level estimates
  for random effects.

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for Anova-objects gains a `p_adjust` argument, to apply p-adjustment
  where possible. Furthermore, for models from package *afex*, where
  p-adjustment was applied during model-fitting, the correct p-values
  are now returned (before, unadjusted p-values were returned in some
  cases).

- Revised code-base to address changes in latest *insight* update.
  Dealing with larger models (many parameters, many posterior samples)
  from packages *brms* and *rstanarm* is more efficient now.
  Furthermore, the options for the `effects` argument have a new
  behaviour. `"all"` only returns fixed effects and random effects
  variance components, but no longer the group level estimates. Use
  `effects = "full"` to return all parameters. This change is mainly to
  be more flexible and gain more efficiency for models with many
  parameters and / or many posterior draws.

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for Anova objects gains an `include_intercept` argument, to include
  intercepts in the Anova table, where possible.

## parameters 0.25.0

CRAN release: 2025-04-30

### Changes

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for objects from the *marginaleffects* packages now calls
  [`bayestestR::describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.html)
  to process Bayesian models. This offers more flexibility in
  summarizing the posterior draws from *marginaleffects*.

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  now shows a more informative coefficient name for binomial models with
  probit-link.

- Argument `wb_component` now defaults to `FALSE`.

- Improved support and printing for tests from package *WRS2*.

### Bug fixes

- Fixed printing issue with
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for `htest` objects when printing into markdown or HTML format.

- Fixed printing issue with
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for mixed models when `include_reference = TRUE`.

## parameters 0.24.2

CRAN release: 2025-03-04

### Changes

- The `effects` argument in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for classes `merMod`, `glmmTMB`, `brmsfit` and `stanreg` gets an
  additional `"random_total"` option, to return the overall coefficient
  for random effects (sum of fixed and random effects).

### Bug fixes

- Fixed issue in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for objects from package *marginaleffects* where columns were renamed
  when their names equaled to certain reserved words.

## parameters 0.24.1

CRAN release: 2025-01-14

### Changes

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  now supports objects of class `survfit`.

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  now gives informative error messages for more model classes than
  before when the function fails to extract model parameters.

- Improved information for credible intervals and sampling method from
  output of
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for Bayesian models.

### Bug fixes

- Fixed issue with `model_parameters(<aovlist>, table_wide = TRUE)` with
  complex error structures (
  [\#556](https://github.com/easystats/parameters/issues/556) )

- Fixed issue when printing
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  with models from
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html).

- Fixed issues due to breaking changes in the latest release of the
  *datawizard* package.

- Fixed issue with wrong column-header in printed output of
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for [`MASS::polr()`](https://rdrr.io/pkg/MASS/man/polr.html) models
  with probit-link.

## parameters 0.24.0

CRAN release: 2024-11-27

### Breaking Changes

- The `robust` argument, which was deprecated for a long time, is now no
  longer supported. Please use `vcov` and `vcov_args` instead.

### Changes

- Added support for `coxph.panel` models.

- Added support for [`anova()`](https://rdrr.io/r/stats/anova.html) from
  models of the *survey* package.

- Documentation was re-organized and clarified, and the index reduced by
  removing redundant class-documentation.

### Bug fixes

- Fixed bug in
  [`p_value()`](https://easystats.github.io/parameters/reference/p_value.md)
  for objects of class `averaging`.

- Fixed bug when extracting ‘pretty labels’ for model parameters, which
  could fail when predictors were character vectors.

- Fixed bug with inaccurate standard errors for models from package
  *fixest* that used the `sunab()` function in the formula.

## parameters 0.23.0

CRAN release: 2024-10-18

### Breaking Changes

- Argument `summary` in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  is now deprecated. Please use `include_info` instead.

- Changed output style for the included additional information on model
  formula, sigma and R2 when printing model parameters. This information
  now also includes the RMSE.

### Changes

- Used more accurate analytic approach to calculate normal distributions
  for the SGPV in
  [`equivalence_test()`](https://easystats.github.io/bayestestR/reference/equivalence_test.html)
  and used in
  [`p_significance()`](https://easystats.github.io/bayestestR/reference/p_significance.html).

- Added
  [`p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.html)
  methods for frequentist models. This is a convenient way to test the
  direction of the effect, which formerly was already (and still is)
  possible with `pd = TRUE` in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md).

- [`p_function()`](https://easystats.github.io/parameters/reference/p_function.md),
  [`p_significance()`](https://easystats.github.io/bayestestR/reference/p_significance.html)
  and
  [`equivalence_test()`](https://easystats.github.io/bayestestR/reference/equivalence_test.html)
  get a `vcov` and `vcov_args` argument, so that results can be based on
  robust standard errors and confidence intervals.

- [`equivalence_test()`](https://easystats.github.io/bayestestR/reference/equivalence_test.html)
  and
  [`p_significance()`](https://easystats.github.io/bayestestR/reference/p_significance.html)
  work with objects returned by
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md).

- [`pool_parameters()`](https://easystats.github.io/parameters/reference/pool_parameters.md)
  now better deals with models with multiple components
  (e.g. zero-inflation or dispersion).

- Revision / enhancement of some documentation.

- Updated *glmmTMB* methods to work with the latest version of the
  package.

- Improved printing for
  [`simulate_parameters()`](https://easystats.github.io/parameters/reference/simulate_parameters.md)
  for models from packages *mclogit*.

- [`print()`](https://rdrr.io/r/base/print.html) for
  [`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md)
  now also puts factor levels into square brackets, like the
  [`print()`](https://rdrr.io/r/base/print.html) method for
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md).

- `include_reference` now only adds the reference category of factors to
  the parameters table when those factors have appropriate contrasts
  (treatment or SAS contrasts).

### Bug fixes

- Arguments like `digits` etc. were ignored in \`model_parameters() for
  objects from the *marginaleffects* package.

## parameters 0.22.2

CRAN release: 2024-09-03

### New supported models

- Support for models `glm_weightit`, `multinom_weightit` and
  `ordinal_weightit` from package *WeightIt*.

### Changes

- Added
  [`p_significance()`](https://easystats.github.io/bayestestR/reference/p_significance.html)
  methods for frequentist models.

- Methods for
  [`degrees_of_freedom()`](https://easystats.github.io/parameters/reference/degrees_of_freedom.md)
  have been removed.
  [`degrees_of_freedom()`](https://easystats.github.io/parameters/reference/degrees_of_freedom.md)
  now calls
  [`insight::get_df()`](https://easystats.github.io/insight/reference/get_df.html).

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for data frames and `draws` objects from package *posterior* also gets
  an `exponentiate` argument.

### Bug fixes

- Fixed issue with warning for spuriously high coefficients for
  Stan-models (non-Gaussian).

## parameters 0.22.1

CRAN release: 2024-07-21

### Breaking changes

- Revised calculation of the second generation p-value (SGPV) in
  [`equivalence_test()`](https://easystats.github.io/bayestestR/reference/equivalence_test.html),
  which should now be more accurate related to the proportion of the
  interval that falls inside the ROPE. Formerly, the confidence interval
  was simply treated as uniformly distributed when calculating the SGPV,
  now the interval is assumed to be normally distributed.

### New supported models

- Support for `svy2lme` models from package *svylme*.

### Changes

- [`standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.md)
  now also prettifies labels of factors.

### Bug fixes

- Fixed issue with
  [`equivalence_test()`](https://easystats.github.io/bayestestR/reference/equivalence_test.html)
  when ROPE range was not symmetrically centered around zero (e.g.,
  `range = c(-99, 0.1)`).

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for [`anova()`](https://rdrr.io/r/stats/anova.html) from mixed models
  now also includes the denominator degrees of freedom in the output
  (`df_error`).

- `print(..., pretty_names = "labels")` for tobit-models from package
  *AER* now include value labels, if available.

- Patch release, to ensure that performance runs with older version of
  datawizard on Mac OS X with R (old-release).

## parameters 0.22.0

CRAN release: 2024-06-20

### Breaking changes

- Deprecated arguments in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for `htest`, `aov` and `BFBayesFactor` objects were removed.

- Argument `effectsize_type` is deprecated. Please use `es_type` now.
  This change was necessary to avoid conflicts with partial matching of
  argument names (here: `effects`).

### New supported models

- Support for objects from
  [`stats::Box.test()`](https://rdrr.io/r/stats/box.test.html).

- Support for `glmgee` models from package *glmtoolbox*.

### Bug fix

- Fixed edge case in [`predict()`](https://rdrr.io/r/stats/predict.html)
  for
  [`factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.md).

- Fixed wrong ORCID in `DESCRIPTION`.

## parameters 0.21.7

CRAN release: 2024-05-14

### Changes

- Fixed issues related to latest release from *marginaleffects*.

### Bug fixes

- Fixes issue in
  [`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md)
  for models from package *blme*.

- Fixed conflict in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  when both `include_reference = TRUE` and `pretty_names = "labels"`
  were used. Now, pretty labels are correctly updated and preserved.

## parameters 0.21.6

CRAN release: 2024-03-18

### New supported models

- Support for models of class `serp` (*serp*).

### Changes

- `include_reference` can now directly be set to `TRUE` in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  and doesn’t require a call to
  [`print()`](https://rdrr.io/r/base/print.html) anymore.

- [`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md)
  gains a `include_reference` argument, to add the reference category of
  categorical predictors to the parameters table.

- [`print_md()`](https://easystats.github.io/insight/reference/display.html)
  for
  [`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md)
  now by default uses the *tinytable* package to create markdown tables.
  This allows better control for column heading spanning over multiple
  columns.

### Bug fixes

- Fixed issue with parameter names for
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  and objects from package *epiR*.

- Fixed issue with `exponentiate = TRUE` for
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  with models of class `clmm` (package *ordinal*), when model had no
  `component` column (e.g., no scale or location parameters were
  returned).

- `include_reference` now also works when factor were created
  “on-the-fly” inside the model formula (i.e. `y ~ as.factor(x)`).

## parameters 0.21.5

CRAN release: 2024-02-07

### Bug fixes

- Fixes CRAN check errors related to the changes in the latest update of
  *marginaleffects*.

## parameters 0.21.4

CRAN release: 2024-02-03

### Breaking changes

- The `exponentiate` argument of
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for
  [`marginaleffects::predictions()`](https://marginaleffects.com/man/r/predictions.html)
  now defaults to `FALSE`, in line with all the other
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  methods.

### Changes

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for models of package *survey* now gives informative messages when
  `bootstrap = TRUE` (which is currently not supported).

- [`n_factors()`](https://easystats.github.io/parameters/reference/n_factors.md)
  now also returns the explained variance for the number of factors as
  attributes.

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for objects of package *metafor* now warns when unsupported arguments
  (like `vcov`) are used.

- Improved documentation for
  [`pool_parameters()`](https://easystats.github.io/parameters/reference/pool_parameters.md).

### Bug fixes

- `print(include_reference = TRUE)` for
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  did not work when run inside a pipe-chain.

- Fixed issues with [`format()`](https://rdrr.io/r/base/format.html) for
  objects returned by
  [`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md)
  that included mixed models.

## parameters 0.21.3

CRAN release: 2023-11-02

### Changes

- [`principal_components()`](https://easystats.github.io/parameters/reference/principal_components.md)
  and
  [`factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.md)
  now also work when argument `n = 1`.

- [`print_md()`](https://easystats.github.io/insight/reference/display.html)
  for
  [`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md)
  now gains more arguments, similar to the
  [`print()`](https://rdrr.io/r/base/print.html) method.

- [`bootstrap_parameters()`](https://easystats.github.io/parameters/reference/bootstrap_parameters.md)
  and
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  now accept bootstrapped samples returned by
  [`bootstrap_model()`](https://easystats.github.io/parameters/reference/bootstrap_model.md).

- The [`print()`](https://rdrr.io/r/base/print.html) method for
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  now also yields a warning for models with logit-links when possible
  issues with (quasi) complete separation occur.

### Bug fixes

- Fixed issue in
  [`print_html()`](https://easystats.github.io/insight/reference/display.html)
  for objects from package *ggeffects*.

- Fixed issues for
  [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html) with
  wide-format response variables (using
  [`cbind()`](https://amices.org/mice/reference/cbind.html)).

- Minor fixes for
  [`print_html()`](https://easystats.github.io/insight/reference/display.html)
  method for
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md).

- Robust standard errors (argument `vcov`) now works for `plm` models.

## parameters 0.21.2

CRAN release: 2023-09-16

### Changes

- Minor improvements to factor analysis functions.

- The `ci_digits` argument of the
  [`print()`](https://rdrr.io/r/base/print.html) method for
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  now defaults to the same value of `digits`.

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for objects from package *marginaleffects* now also accepts the
  `exponentiate` argument.

- The [`print()`](https://rdrr.io/r/base/print.html),
  [`print_html()`](https://easystats.github.io/insight/reference/display.html),
  [`print_md()`](https://easystats.github.io/insight/reference/display.html)
  and [`format()`](https://rdrr.io/r/base/format.html) methods for
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  get an `include_reference` argument, to add the reference category of
  categorical predictors to the parameters table.

### Bug fixes

- Fixed issue with wrong calculation of test-statistic and p-values in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for `fixest` models.

- Fixed issue with wrong column header for `glm` models with
  `family = binomial("identiy")`.

- Minor fixes for
  [`dominance_analysis()`](https://easystats.github.io/parameters/reference/dominance_analysis.md).

## parameters 0.21.1

CRAN release: 2023-05-26

### General

- Added support for models of class `nestedLogit` (*nestedLogit*).

### Changes to functions

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  now also prints correct “pretty names” when predictors where converted
  to ordered factors inside formulas, e.g. `y ~ as.ordered(x)`.

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  now prints a message when the `vcov` argument is provided and
  `ci_method` is explicitly set to `"profile"`. Else, when `vcov` is not
  `NULL` and `ci_method` is `NULL`, it defaults to `"wald"`, to return
  confidence intervals based on robust standard errors.

## parameters 0.21.0

CRAN release: 2023-04-19

### Breaking Changes

- It is no longer possible to calculate Satterthwaite-approximated
  degrees of freedom for mixed models from package *nlme*. This was
  based on the *lavaSearch2* package, which no longer seems to support
  models of class `lme`.

### Changes to functions

- Improved support for objects of class `mipo` for models with ordinal
  or categorical outcome.

## parameters 0.20.3

CRAN release: 2023-04-05

### General

- Added support for models of class `hglm` (*hglm*), `mblogit`
  (*mclogit*), `fixest_multi` (*fixest*), and `phylolm` / `phyloglm`
  (*phylolm*).

- `as.data.frame` methods for extracting posterior draws via
  [`bootstrap_model()`](https://easystats.github.io/parameters/reference/bootstrap_model.md)
  have been retired. Instead, directly using
  [`bootstrap_model()`](https://easystats.github.io/parameters/reference/bootstrap_model.md)
  is recommended.

### Changes to functions

- [`equivalence_test()`](https://easystats.github.io/bayestestR/reference/equivalence_test.html)
  gets a method for `ggeffects` objects from package *ggeffects*.

- [`equivalence_test()`](https://easystats.github.io/bayestestR/reference/equivalence_test.html)
  now prints the `SGPV` column instead of `% in ROPE`. This is because
  the former `% in ROPE` actually was equivalent to the second
  generation p-value (SGPV) and refers to the proportion of the *range*
  of the confidence interval that is covered by the ROPE. However,
  `% in ROPE` did not refer to the probability mass of the underlying
  distribution of a confidence interval that was covered by the ROPE,
  hence the old column name was a bit misleading.

- Fixed issue in `model_parameters.ggeffects()` to address forthcoming
  changes in the *ggeffects* package.

### Bug fixes

- When an invalid or not supported value for the `p_adjust` argument in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  is provided, the valid options were not shown in correct capital
  letters, where appropriate.

- Fixed bug in
  [`cluster_analysis()`](https://easystats.github.io/parameters/reference/cluster_analysis.md)
  for `include_factors = TRUE`.

- Fixed warning in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  and [`ci()`](https://easystats.github.io/bayestestR/reference/ci.html)
  for models from package *glmmTMB* when `ci_method` was either
  `"profile"` or `"uniroot"`.

## parameters 0.20.2

CRAN release: 2023-01-27

### General

- Reduce unnecessary warnings.

- The deprecated argument `df_method` in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)was
  removed.

- Output from
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for objects returned by
  [`manova()`](https://rdrr.io/r/stats/manova.html) and
  [`car::Manova()`](https://rdrr.io/pkg/car/man/Anova.html) is now more
  consistent.

### Bug fix

- Fixed issues in tests for `mmrm` models.

- Fixed issue in
  [`bootstrap_model()`](https://easystats.github.io/parameters/reference/bootstrap_model.md)
  for models of class `glmmTMB` with dispersion parameters.

- Fixed failing examples.

## parameters 0.20.1

CRAN release: 2023-01-11

### General

- Added support for models of class `flic` and `flac` (*logistf*),
  `mmrm` (*mmrm*).

### Changes

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  now includes a `Group` column for `stanreg` or `brmsfit` models with
  random effects.

- The [`print()`](https://rdrr.io/r/base/print.html) method for
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  now uses the same pattern to print random effect variances for
  Bayesian models as for frequentist models.

### Bug fix

- Fixed issue with the [`print()`](https://rdrr.io/r/base/print.html)
  method for
  [`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md),
  which duplicated random effects parameters rows in some edge cases.

- Fixed issue with the [`print()`](https://rdrr.io/r/base/print.html)
  method for
  [`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md),
  which didn’t work properly when `ci=NULL`.

## parameters 0.20.0

CRAN release: 2022-11-21

### Breaking

- The deprecated argument `df_method` in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  is now defunct and throws an error when used.

- The deprecated functions `ci_robust()`, `p_robust()` and
  `standard_error_robust` have been removed. These were superseded by
  the `vcov` argument in
  [`ci()`](https://easystats.github.io/bayestestR/reference/ci.html),
  [`p_value()`](https://easystats.github.io/parameters/reference/p_value.md),
  and
  [`standard_error()`](https://easystats.github.io/parameters/reference/standard_error.md),
  respectively.

- The `style` argument in
  [`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md)
  was renamed into `select`.

### New functions

- [`p_function()`](https://easystats.github.io/parameters/reference/p_function.md),
  to print and plot p-values and compatibility (confidence) intervals
  for statistical models, at different levels. This allows to see which
  estimates are most compatible with the model at various compatibility
  levels.

- [`p_calibrate()`](https://easystats.github.io/parameters/reference/p_calibrate.md),
  to compute calibrated p-values.

### Changes

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  and
  [`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md)
  now use the unicode character for the multiplication-sign as
  interaction mark (i.e. `\u00d7`). Use
  `options(parameters_interaction = <value>)` or the argument
  `interaction_mark` to use a different character as interaction mark.

- The `select` argument in
  [`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md),
  which is used to control the table column elements, now supports an
  experimental glue-like syntax. See this vignette *Printing Model
  Parameters*. Furthermore, the `select` argument can also be used in
  the [`print()`](https://rdrr.io/r/base/print.html) method for
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md).

- [`print_html()`](https://easystats.github.io/insight/reference/display.html)
  gets a `font_size` and `line_padding` argument to tweak the appearance
  of HTML tables. Furthermore, arguments `select` and `column_labels`
  are new, to customize the column layout of tables. See examples in
  [`?display`](https://easystats.github.io/insight/reference/display.html).

- Consolidation of vignettes on standardization of model parameters.

- Minor speed improvements.

### Bug fix

- `model_parameters().BFBayesFactor` no longer drops the `BF` column if
  the Bayes factor is `NA`.

- The [`print()`](https://rdrr.io/r/base/print.html) and
  [`display()`](https://easystats.github.io/insight/reference/display.html)
  methods for
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  from Bayesian models now pass the `...` to
  [`insight::format_table()`](https://easystats.github.io/insight/reference/format_table.html),
  allowing extra arguments to be recognized.

- Fixed footer message regarding the approximation method for CU and
  p-values for mixed models.

- Fixed issues in the [`print()`](https://rdrr.io/r/base/print.html)
  method for
  [`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md)
  with mixed models, when some models contained within-between
  components (see `wb_component`) and others did not.

## parameters 0.19.0

CRAN release: 2022-10-05

### Breaking

- Arguments that calculate effectsize in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for `htest`, Anova objects and objects of class `BFBayesFactor` were
  revised. Instead of single arguments for the different effectsizes,
  there is now one argument, `effectsize_type`. The reason behind this
  change is that meanwhile many new type of effectsizes have been added
  to the *effectsize* package, and the generic argument allows to make
  use of those effect sizes.

- The attribute name in PCA / EFA has been changed from `data_set` to
  `dataset`.

- The minimum needed R version has been bumped to `3.6`.

- Removed deprecated argument `parameters` from
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md).

- `standard_error_robust()`, `ci_robust()` and `p_value_robust()` are
  now deprecated and superseded by the `vcov` and `vcov_args` arguments
  in the related methods
  [`standard_error()`](https://easystats.github.io/parameters/reference/standard_error.md),
  [`ci()`](https://easystats.github.io/bayestestR/reference/ci.html) and
  [`p_value()`](https://easystats.github.io/parameters/reference/p_value.md),
  respectively.

- Following functions were moved from package *parameters* to
  *performance*:
  [`check_sphericity_bartlett()`](https://easystats.github.io/performance/reference/check_factorstructure.html),
  [`check_kmo()`](https://easystats.github.io/performance/reference/check_factorstructure.html),
  [`check_factorstructure()`](https://easystats.github.io/performance/reference/check_factorstructure.html)
  and
  [`check_clusterstructure()`](https://easystats.github.io/performance/reference/check_clusterstructure.html).

### Changes to functions

- Added `sparse` option to
  [`principal_components()`](https://easystats.github.io/parameters/reference/principal_components.md)
  for sparse PCA.

- The `pretty_names` argument from the
  [`print()`](https://rdrr.io/r/base/print.html) method can now also be
  `"labels"`, which will then use variable and value labels (if data is
  labelled) as pretty names. If no labels were found, default pretty
  names are used.

- [`bootstrap_model()`](https://easystats.github.io/parameters/reference/bootstrap_model.md)
  for models of class `glmmTMB` and `merMod` gains a `cluster` argument
  to specify optional clusters when the `parallel` option is set to
  `"snow"`.

- P-value adjustment (argument `p_adjust` in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md))
  is now performed after potential parameters were removed (using `keep`
  or `drop`), so adjusted p-values is only applied to the parameters of
  interest.

- Robust standard errors are now supported for `fixest` models with the
  `vcov` argument.

- [`print()`](https://rdrr.io/r/base/print.html) for
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  gains a `footer` argument, which can be used to suppress the footer in
  the output. Further more, if `footer = ""` or `footer = FALSE` in
  [`print_md()`](https://easystats.github.io/insight/reference/display.html),
  no footer is printed.

- [`simulate_model()`](https://easystats.github.io/parameters/reference/simulate_model.md)
  and
  [`simulate_parameters()`](https://easystats.github.io/parameters/reference/simulate_parameters.md)
  now pass `...` to
  [`insight::get_varcov()`](https://easystats.github.io/insight/reference/get_varcov.html),
  to allow simulated draws to be based on heteroscedasticity consistent
  variance covariance matrices.

- The [`print()`](https://rdrr.io/r/base/print.html) method for
  [`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md)
  was improved for models with multiple components (e.g., mixed models
  with fixed and random effects, or models with count- and
  zero-inflation parts). For these models,
  `compare_parameters(effects = "all", component = "all")` prints more
  nicely.

### Bug fixes

- Fix erroneous warning for *p*-value adjustments when the differences
  between original and adjusted *p*-values were very small.

## parameters 0.18.2

CRAN release: 2022-08-10

### New functions

- New function
  [`dominance_analysis()`](https://easystats.github.io/parameters/reference/dominance_analysis.md),
  to compute dominance analysis statistics and designations.

### Changes to functions

- Argument `ci_random` in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  defaults to `NULL`. It uses a heuristic to determine if random effects
  confidence intervals are likely to take a long time to compute, and
  automatically includes or excludes those confidence intervals. Set
  `ci_random` to `TRUE` or `FALSE` to explicitly calculate or omit
  confidence intervals for random effects.

### Bug fixes

- Fix issues in
  [`pool_parameters()`](https://easystats.github.io/parameters/reference/pool_parameters.md)
  for certain models with special components (like
  [`MASS::polr()`](https://rdrr.io/pkg/MASS/man/polr.html)), that failed
  when argument `component` was set to `"conditional"` (the default).

- Fix issues in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for multiple imputation models from package *Hmisc*.

## parameters 0.18.1

CRAN release: 2022-05-29

### General

- It is now possible to hide messages about CI method below tables by
  specifying `options("parameters_cimethod" = FALSE)`
  ([\#722](https://github.com/easystats/parameters/issues/722)). By
  default, these messages are displayed.

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  now supports objects from package *marginaleffects* and objects
  returned by
  [`car::linearHypothesis()`](https://rdrr.io/pkg/car/man/linearHypothesis.html).

- Added [`predict()`](https://rdrr.io/r/stats/predict.html) method to
  `cluster_meta` objects.

- Reorganization of docs for
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md).

### Changes to functions

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  now also includes standard errors and confidence intervals for
  slope-slope-correlations of random effects variances.

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for mixed models gains a `ci_random` argument, to toggle whether
  confidence intervals for random effects parameters should also be
  computed. Set to `FALSE` if calculation of confidence intervals for
  random effects parameters takes too long.

- [`ci()`](https://easystats.github.io/bayestestR/reference/ci.html) for
  *glmmTMB* models with `method = "profile"` is now more robust.

### Bug fixes

- Fixed issue with *glmmTMB* models when calculating confidence
  intervals for random effects failed due to singular fits.

- [`display()`](https://easystats.github.io/insight/reference/display.html)
  now correctly includes custom text and additional information in the
  footer ([\#722](https://github.com/easystats/parameters/issues/722)).

- Fixed issue with argument `column_names` in
  [`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md)
  when strings contained characters that needed to be escaped for
  regular expressions.

- Fixed issues with unknown arguments in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for *lavaan* models when `standardize = TRUE`.

## parameters 0.18.0

CRAN release: 2022-05-24

### Breaking Changes

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  now no longer treats data frame inputs as posterior samples. Rather,
  for data frames, now `NULL` is returned. If you want to treat a data
  frame as posterior samples, set the new argument `as_draws = TRUE`.

### New functions

- [`sort_parameters()`](https://easystats.github.io/parameters/reference/sort_parameters.md)
  to sort model parameters by coefficient values.

- [`standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.md),
  [`standardize_info()`](https://easystats.github.io/parameters/reference/standardize_info.md)
  and
  [`standardise_posteriors()`](https://easystats.github.io/parameters/reference/standardize_parameters.md)
  to standardize model parameters.

### Changes to functions

#### `model_parameters()`

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for mixed models from package *lme4* now also reports confidence
  intervals for random effect variances by default. Formerly, CIs were
  only included when `ci_method` was `"profile"` or `"boot"`. The
  *merDeriv* package is required for this feature.

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for `htest` objects now also supports models from
  [`var.test()`](https://rdrr.io/r/stats/var.test.html).

- Improved support for `anova.rms` models in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md).

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  now supports `draws` objects from package *posterior* and
  `deltaMethods` objects from package *car*.

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  now checks arguments and informs the user if specific given arguments
  are not supported for that model class (e.g., `"vcov"` is currently
  not supported for models of class *glmmTMB*).

### Bug fixes

- The `vcov` argument, used for computing robust standard errors, did
  not calculate the correct p-values and confidence intervals for models
  of class `lme`.

- [`pool_parameters()`](https://easystats.github.io/parameters/reference/pool_parameters.md)
  did not save all relevant model information as attributes.

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for models from package *glmmTMB* did not work when
  `exponentiate = TRUE` and model contained a dispersion parameter that
  was different than sigma. Furthermore, exponentiating falsely
  exponentiated the dispersion parameter.

## parameters 0.17.0

CRAN release: 2022-03-10

### General

- Added options to set defaults for different arguments. Currently
  supported:

  - `options("parameters_summary" = TRUE/FALSE)`, which sets the default
    value for the `summary` argument in
    [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
    for non-mixed models.
  - `options("parameters_mixed_summary" = TRUE/FALSE)`, which sets the
    default value for the `summary` argument in
    [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
    for mixed models.

- Minor improvements for [`print()`](https://rdrr.io/r/base/print.html)
  methods.

- Robust uncertainty estimates:

  - The `vcov_estimation`, `vcov_type`, and `robust` arguments are
    deprecated in these functions:
    [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md),
    [`parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md),
    [`standard_error()`](https://easystats.github.io/parameters/reference/standard_error.md),
    [`p_value()`](https://easystats.github.io/parameters/reference/p_value.md),
    and
    [`ci()`](https://easystats.github.io/bayestestR/reference/ci.html).
    They are replaced by the `vcov` and `vcov_args` arguments.
  - The `standard_error_robust()` and `p_value_robust()` functions are
    superseded by the `vcov` and `vcov_args` arguments of the
    [`standard_error()`](https://easystats.github.io/parameters/reference/standard_error.md)
    and
    [`p_value()`](https://easystats.github.io/parameters/reference/p_value.md)
    functions.
  - Vignette:
    <https://easystats.github.io/parameters/articles/model_parameters_robust.html>

### Bug fixes

- Fixed minor issues and edge cases in
  [`n_clusters()`](https://easystats.github.io/parameters/reference/n_clusters.md)
  and related cluster functions.

- Fixed issue in
  [`p_value()`](https://easystats.github.io/parameters/reference/p_value.md)
  that returned wrong p-values for
  [`fixest::feols()`](https://lrberge.github.io/fixest/reference/feols.html).

## parameters 0.16.0

CRAN release: 2022-01-12

### General

- Improved speed performance for
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md),
  in particular for glm’s and mixed models where random effect variances
  were calculated.

- Added more options for printing
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md).
  See also revised vignette:
  <https://easystats.github.io/parameters/articles/model_parameters_print.html>

### Changes to functions

#### `model_parameters()`

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for mixed models gains an `include_sigma` argument. If `TRUE`, adds
  the residual variance, computed from the random effects variances, as
  an attribute to the returned data frame. Including sigma was the
  default behaviour, but now defaults to `FALSE` and is only included
  when `include_sigma = TRUE`, because the calculation was very time
  consuming.

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for `merMod` models now also computes CIs for the random SD parameters
  when `ci_method="boot"` (previously, this was only possible when
  `ci_method` was `"profile"`).

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for `glmmTMB` models now computes CIs for the random SD parameters.
  Note that these are based on a Wald-z-distribution.

- Similar to
  [`model_parameters.htest()`](https://easystats.github.io/parameters/reference/model_parameters.htest.md),
  the
  [`model_parameters.BFBayesFactor()`](https://easystats.github.io/parameters/reference/model_parameters.BFBayesFactor.md)
  method gains `cohens_d` and `cramers_v` arguments to control if you
  need to add frequentist effect size estimates to the returned summary
  data frame. Previously, this was done by default.

- Column name for coefficients from *emmeans* objects are now more
  specific.

- `model_prameters()` for `MixMod` objects (package *GLMMadaptive*)
  gains a `robust` argument, to compute robust standard errors.

### Bug fixes

- Fixed bug with
  [`ci()`](https://easystats.github.io/bayestestR/reference/ci.html) for
  class `merMod` when `method="boot"`.

- Fixed issue with correct association of components for ordinal models
  of classes `clm` and `clm2`.

- Fixed issues in
  [`random_parameters()`](https://easystats.github.io/parameters/reference/random_parameters.md)
  and
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for mixed models without random intercept.

- Confidence intervals for random parameters in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  failed for (some?) `glmer` models.

- Fix issue with default `ci_type` in
  [`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md)
  for Bayesian models.

## parameters 0.15.0

CRAN release: 2021-10-18

### Breaking changes

- Following functions were moved to the new *datawizard* package and are
  now re-exported from *parameters* package:

  - `center()`

  - `convert_data_to_numeric()`

  - `data_partition()`

  - [`demean()`](https://easystats.github.io/datawizard/reference/demean.html)
    (and its aliases `degroup()` and `detrend()`)

  - [`kurtosis()`](https://easystats.github.io/datawizard/reference/skewness.html)

  - [`rescale_weights()`](https://easystats.github.io/datawizard/reference/rescale_weights.html)

  - [`skewness()`](https://easystats.github.io/datawizard/reference/skewness.html)

  - `smoothness()`

Note that these functions will be removed in the next release of
*parameters* package and they are currently being re-exported only as a
convenience for the package developers. This release should provide them
with time to make the necessary changes before this breaking change is
implemented.

- Following functions were moved to the *performance* package:

  - `check_heterogeneity()`

  - [`check_multimodal()`](https://easystats.github.io/performance/reference/check_multimodal.html)

### General

- The handling to approximate the degrees of freedom in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md),
  [`ci()`](https://easystats.github.io/bayestestR/reference/ci.html) and
  [`p_value()`](https://easystats.github.io/parameters/reference/p_value.md)
  was revised and should now be more consistent. Some bugs related to
  the previous computation of confidence intervals and p-values have
  been fixed. Now it is possible to change the method to approximate
  degrees of freedom for CIs and p-values using the `ci_method`, resp.
  `method` argument. This change has been documented in detail in
  [`?model_parameters`](https://easystats.github.io/parameters/reference/model_parameters.md),
  and online here:
  <https://easystats.github.io/parameters/reference/model_parameters.html>

- Minor changes to [`print()`](https://rdrr.io/r/base/print.html) for
  *glmmTMB* with dispersion parameter.

- Added vignette on printing options for model parameters.

### Changes to functions

#### `model_parameters()`

- The `df_method` argument in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  is deprecated. Please use `ci_method` now.

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  with `standardize = "refit"` now returns random effects from the
  standardized model.

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  and [`ci()`](https://easystats.github.io/bayestestR/reference/ci.html)
  for `lmerMod` models gain a `"residuals"` option for the `ci_method`
  (resp. `method`) argument, to explicitly calculate confidence
  intervals based on the residual degrees of freedom, when present.

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  supports following new objects: `trimcibt`, `wmcpAKP`, `dep.effect`
  (in *WRS2* package), `systemfit`

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  gains a new argument `table_wide` for ANOVA tables. This can be
  helpful for users who may wish to report ANOVA table in wide format
  (i.e., with numerator and denominator degrees of freedom on the same
  row).

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  gains two new arguments, `keep` and `drop`. `keep` is the new names
  for the former `parameters` argument and can be used to filter
  parameters. While `keep` selects those parameters whose names match
  the regular expression pattern defined in `keep`, `drop` is the
  counterpart and excludes matching parameter names.

- When
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  is called with `verbose = TRUE`, and `ci_method` is not the default
  value, the printed output includes a message indicating which
  approximation-method for degrees of freedom was used.

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for mixed models with `ci_method = "profile` computes (profiled)
  confidence intervals for both fixed and random effects. Thus,
  `ci_method = "profile` allows to add confidence intervals to the
  random effect variances.

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  should longer fail for supported model classes when robust standard
  errors are not available.

#### Other functions

- [`n_factors()`](https://easystats.github.io/parameters/reference/n_factors.md)
  the methods based on fit indices have been fixed and can be included
  separately (`package = "fit"`). Also added a `n_max` argument to crop
  the output.

- [`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md)
  now also accepts a list of model objects.

- [`describe_distribution()`](https://easystats.github.io/datawizard/reference/describe_distribution.html)
  gets `verbose` argument to toggle warnings and messages.

- [`format_parameters()`](https://easystats.github.io/parameters/reference/format_parameters.md)
  removes dots and underscores from parameter names, to make these more
  “human readable”.

- The experimental calculation of p-values in
  [`equivalence_test()`](https://easystats.github.io/bayestestR/reference/equivalence_test.html)
  was replaced by a proper calculation p-values. The argument `p_value`
  was removed and p-values are now always included.

- Minor improvements to [`print()`](https://rdrr.io/r/base/print.html),
  [`print_html()`](https://easystats.github.io/insight/reference/display.html)
  and
  [`print_md()`](https://easystats.github.io/insight/reference/display.html).

### Bug fixes

- The random effects returned by
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  mistakenly displayed the residuals standard deviation as square-root
  of the residual SD.

- Fixed issue with
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for *brmsfit* objects that model standard errors (i.e. for
  meta-analysis).

- Fixed issue in `model_parameters` for `lmerMod` models that, by
  default, returned residual degrees of freedom in the statistic column,
  but confidence intervals were based on `Inf` degrees of freedom
  instead.

- Fixed issue in
  [`ci_satterthwaite()`](https://easystats.github.io/parameters/reference/p_value_satterthwaite.md),
  which used `Inf` degrees of freedom instead of the Satterthwaite
  approximation.

- Fixed issue in
  [`model_parameters.mlm()`](https://easystats.github.io/parameters/reference/model_parameters.mlm.md)
  when model contained interaction terms.

- Fixed issue in
  [`model_parameters.rma()`](https://easystats.github.io/parameters/reference/model_parameters.rma.md)
  when model contained interaction terms.

- Fixed sign error for
  [`model_parameters.htest()`](https://easystats.github.io/parameters/reference/model_parameters.htest.md)
  for objects created with `t.test.formula()` (issue
  [\#552](https://github.com/easystats/parameters/issues/552))

- Fixed issue when computing random effect variances in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for mixed models with categorical random slopes.

## parameters 0.14.0

CRAN release: 2021-05-29

### Breaking changes

- [`check_sphericity()`](https://easystats.github.io/performance/reference/check_sphericity.html)
  has been renamed into
  [`check_sphericity_bartlett()`](https://easystats.github.io/performance/reference/check_factorstructure.html).

- Removed deprecated arguments.

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for bootstrapped samples used in *emmeans* now treats the bootstrap
  samples as samples from posterior distributions (Bayesian models).

### New supported model classes

- `SemiParBIV` (*GJRM*), `selection` (*sampleSelection*), `htest` from
  the *survey* package, `pgmm` (*plm*).

### General

- Performance improvements for models from package *survey*.

### New functions

- Added a [`summary()`](https://rdrr.io/r/base/summary.html) method for
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md),
  which is a convenient shortcut for `print(..., select = "minimal")`.

### Changes to functions

#### `model_parameters()`

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  gains a `parameters` argument, which takes a regular expression as
  string, to select specific parameters from the returned data frame.

- [`print()`](https://rdrr.io/r/base/print.html) for
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  and
  [`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md)
  gains a `groups` argument, to group parameters in the output.
  Furthermore, `groups` can be used directly as argument in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  and
  [`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md)
  and will be passed to the
  [`print()`](https://rdrr.io/r/base/print.html) method.

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for ANOVAs now saves the type as attribute and prints this information
  as footer in the output as well.

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for *htest*-objects now saves the alternative hypothesis as attribute
  and prints this information as footer in the output as well.

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  passes arguments `type`, `parallel` and `n_cpus` down to
  [`bootstrap_model()`](https://easystats.github.io/parameters/reference/bootstrap_model.md)
  when `bootstrap = TRUE`.

#### other

- `bootstrap_models()` for *merMod* and *glmmTMB* objects gains further
  arguments to set the type of bootstrapping and to allow parallel
  computing.

- [`bootstrap_parameters()`](https://easystats.github.io/parameters/reference/bootstrap_parameters.md)
  gains the `ci_method` type `"bci"`, to compute bias-corrected and
  accelerated bootstrapped intervals.

- [`ci()`](https://easystats.github.io/bayestestR/reference/ci.html) for
  `svyglm` gains a `method` argument.

### Bug fixes

- Fixed issue in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for *emmGrid* objects with Bayesian models.

- Arguments `digits`, `ci_digits` and `p_digits` were ignored for
  [`print()`](https://rdrr.io/r/base/print.html) and only worked when
  used in the call to
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  directly.

## parameters 0.13.0

CRAN release: 2021-04-08

### General

- Revised and improved the
  [`print()`](https://rdrr.io/r/base/print.html) method for
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md).

### New supported model classes

- `blrm` (*rmsb*), `AKP`, `med1way`, `robtab` (*WRS2*), `epi.2by2`
  (*epiR*), `mjoint` (*joineRML*), `mhurdle` (*mhurdle*), `sarlm`
  (*spatialreg*), `model_fit` (*tidymodels*), `BGGM` (*BGGM*), `mvord`
  (*mvord*)

### Changes to functions

#### `model_parameters()`

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for `blavaan` models is now fully treated as Bayesian model and thus
  relies on the functions from *bayestestR* (i.e. ROPE, Rhat or ESS are
  reported) .

- The `effects`-argument from
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for mixed models was revised and now shows the random effects
  variances by default (same functionality as
  [`random_parameters()`](https://easystats.github.io/parameters/reference/random_parameters.md),
  but mimicking the behaviour from
  [`broom.mixed::tidy()`](https://generics.r-lib.org/reference/tidy.html)).
  When the `group_level` argument is set to `TRUE`, the conditional
  modes (BLUPs) of the random effects are shown.

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for mixed models now returns an `Effects` column even when there is
  just one type of “effects”, to mimic the behaviour from
  [`broom.mixed::tidy()`](https://generics.r-lib.org/reference/tidy.html).
  In conjunction with
  [`standardize_names()`](https://easystats.github.io/insight/reference/standardize_names.html)
  users can get the same column names as in
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) for
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  objects.

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for t-tests now uses the group values as column names.

- [`print()`](https://rdrr.io/r/base/print.html) for
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  gains a `zap_small` argument, to avoid scientific notation for very
  small numbers. Instead, `zap_small` forces to round to the specified
  number of digits.

- To be internally consistent, the degrees of freedom column for
  `lqm(m)` and `cgam(m)` objects (with *t*-statistic) is called
  `df_error`.

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  gains a `summary` argument to add summary information about the model
  to printed outputs.

- Minor improvements for models from *quantreg*.

- `model_parameters` supports rank-biserial, rank epsilon-squared, and
  Kendall’s *W* as effect size measures for
  [`wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html),
  `kruskal.test`, and `friedman.test`, respectively.

#### Other functions

- [`describe_distribution()`](https://easystats.github.io/datawizard/reference/describe_distribution.html)
  gets a `quartiles` argument to include 25th and 75th quartiles of a
  variable.

### Bug fixes

- Fixed issue with non-initialized argument `style` in
  [`display()`](https://easystats.github.io/insight/reference/display.html)
  for
  [`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md).

- Make [`print()`](https://rdrr.io/r/base/print.html) for
  [`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md)
  work with objects that have “simple” column names for confidence
  intervals with missing CI-level (i.e. when column is named `"CI"`
  instead of, say, `"95% CI"`).

- Fixed issue with `p_adjust` in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md),
  which did not work for adjustment-methods `"BY"` and `"BH"`.

- Fixed issue with `show_sigma` in
  [`print()`](https://rdrr.io/r/base/print.html) for
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md).

- Fixed issue in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  with incorrect order of degrees of freedom.

## parameters 0.12.0

CRAN release: 2021-02-21

### General

- Roll-back R dependency to R \>= 3.4.

- Bootstrapped estimates (from
  [`bootstrap_model()`](https://easystats.github.io/parameters/reference/bootstrap_model.md)
  or
  [`bootstrap_parameters()`](https://easystats.github.io/parameters/reference/bootstrap_parameters.md))
  can be passed to `emmeans` to obtain bootstrapped estimates,
  contrasts, simple slopes (etc) and their CIs.

  - These can then be passed to
    [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
    and related functions to obtain standard errors, p-values, etc.

### Breaking changes

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  now always returns the confidence level for as additional `CI` column.

- The `rule` argument in `equivalenct_test()` defaults to `"classic"`.

### New supported model classes

- `crr` (*cmprsk*), `leveneTest()` (*car*), `varest` (*vars*), `ergm`
  (*ergm*), `btergm` (*btergm*), `Rchoice` (*Rchoice*), `garch`
  (*tseries*)

### New functions

- [`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md)
  (and its alias
  [`compare_models()`](https://easystats.github.io/parameters/reference/compare_parameters.md))
  to show / print parameters of multiple models in one table.

### Changes to functions

- Estimation of bootstrapped *p*-values has been re-written to be more
  accurate.

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for mixed models gains an `effects`-argument, to return fixed, random
  or both fixed and random effects parameters.

- Revised printing for
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for *metafor* models.

- [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  for *metafor* models now recognized confidence levels specified in the
  function call (via argument `level`).

- Improved support for effect sizes in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  from *anova* objects.

### Bug fixes

- Fixed edge case when formatting parameters from polynomial terms with
  many degrees.

- Fixed issue with random sampling and dropped factor levels in
  [`bootstrap_model()`](https://easystats.github.io/parameters/reference/bootstrap_model.md).

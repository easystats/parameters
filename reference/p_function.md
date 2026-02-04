# p-value or consonance function

Compute p-values and compatibility (confidence) intervals for
statistical models, at different levels. This function is also called
consonance function. It allows to see which estimates are compatible
with the model at various compatibility levels. Use
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) to generate
plots of the *p* resp. *consonance* function and compatibility intervals
at different levels.

## Usage

``` r
p_function(
  model,
  ci_levels = c(0.25, 0.5, 0.75, emph = 0.95),
  exponentiate = FALSE,
  effects = "fixed",
  component = "all",
  vcov = NULL,
  vcov_args = NULL,
  keep = NULL,
  drop = NULL,
  verbose = TRUE,
  ...
)

consonance_function(
  model,
  ci_levels = c(0.25, 0.5, 0.75, emph = 0.95),
  exponentiate = FALSE,
  effects = "fixed",
  component = "all",
  vcov = NULL,
  vcov_args = NULL,
  keep = NULL,
  drop = NULL,
  verbose = TRUE,
  ...
)

confidence_curve(
  model,
  ci_levels = c(0.25, 0.5, 0.75, emph = 0.95),
  exponentiate = FALSE,
  effects = "fixed",
  component = "all",
  vcov = NULL,
  vcov_args = NULL,
  keep = NULL,
  drop = NULL,
  verbose = TRUE,
  ...
)

# S3 method for class 'parameters_p_function'
format(
  x,
  digits = 2,
  format = NULL,
  ci_width = NULL,
  ci_brackets = TRUE,
  pretty_names = TRUE,
  ...
)

# S3 method for class 'parameters_p_function'
print(
  x,
  digits = 2,
  ci_width = "auto",
  ci_brackets = TRUE,
  pretty_names = TRUE,
  ...
)

# S3 method for class 'parameters_p_function'
print_html(
  x,
  digits = 2,
  ci_width = "auto",
  ci_brackets = c("(", ")"),
  pretty_names = TRUE,
  ...
)
```

## Arguments

- model:

  Statistical Model.

- ci_levels:

  Vector of scalars, indicating the different levels at which
  compatibility intervals should be printed or plotted. In plots, these
  levels are highlighted by vertical lines. It is possible to increase
  thickness for one or more of these lines by providing a names vector,
  where the to be highlighted values should be named `"emph"`, e.g
  `ci_levels = c(0.25, 0.5, emph = 0.95)`.

- exponentiate:

  Logical, indicating whether or not to exponentiate the coefficients
  (and related confidence intervals). This is typical for logistic
  regression, or more generally speaking, for models with log or logit
  links. It is also recommended to use `exponentiate = TRUE` for models
  with log-transformed response values. For models with a
  log-transformed response variable, when `exponentiate = TRUE`, a
  one-unit increase in the predictor is associated with multiplying the
  outcome by that predictor's coefficient. **Note:** Delta-method
  standard errors are also computed (by multiplying the standard errors
  by the transformed coefficients). This is to mimic behaviour of other
  software packages, such as Stata, but these standard errors poorly
  estimate uncertainty for the transformed coefficient. The transformed
  confidence interval more clearly captures this uncertainty. For
  [`compare_parameters()`](https://easystats.github.io/parameters/reference/compare_parameters.md),
  `exponentiate = "nongaussian"` will only exponentiate coefficients
  from non-Gaussian families.

- effects:

  Should parameters for fixed effects (`"fixed"`), random effects
  (`"random"`), or both fixed and random effects (`"all"`) be returned?
  By default, the variance components for random effects are returned.
  If group-level effects are requested, `"grouplevel"` returns the
  group-level random effects (BLUPs), while `"random_total"` return the
  overall (sum of fixed and random) effects (similar to what
  [`coef()`](https://rdrr.io/r/stats/coef.html) returns). Using
  `"grouplevel"` is equivalent to setting `group_level = TRUE`. The
  `effects` argument only applies to mixed models. If the calculation of
  random effects parameters takes too long, you may use
  `effects = "fixed"`.

- component:

  Which type of parameters to return, such as parameters for the
  conditional model, the zero-inflation part of the model, the
  dispersion term, or other auxiliary parameters be returned? Applies to
  models with zero-inflation and/or dispersion formula, or if parameters
  such as `sigma` should be included. May be abbreviated. Note that the
  *conditional* component is also called *count* or *mean* component,
  depending on the model. There are three convenient shortcuts:
  `component = "all"` returns all possible parameters. If
  `component = "location"`, location parameters such as `conditional`,
  `zero_inflated`, or `smooth_terms`, are returned (everything that are
  fixed or random effects - depending on the `effects` argument - but no
  auxiliary parameters). For `component = "distributional"` (or
  `"auxiliary"`), components like `sigma`, `dispersion`, or `beta` (and
  other auxiliary parameters) are returned.

- vcov:

  Variance-covariance matrix used to compute uncertainty estimates
  (e.g., for robust standard errors). This argument accepts a covariance
  matrix, a function which returns a covariance matrix, or a string
  which identifies the function to be used to compute the covariance
  matrix.

  - A covariance matrix

  - A function which returns a covariance matrix (e.g.,
    [`stats::vcov()`](https://rdrr.io/r/stats/vcov.html))

  - A string which indicates the kind of uncertainty estimates to
    return.

    - Heteroskedasticity-consistent: `"HC"`, `"HC0"`, `"HC1"`, `"HC2"`,
      `"HC3"`, `"HC4"`, `"HC4m"`, `"HC5"`. See
      [`?sandwich::vcovHC`](https://sandwich.R-Forge.R-project.org/reference/vcovHC.html)

    - Cluster-robust: `"CR"`, `"CR0"`, `"CR1"`, `"CR1p"`, `"CR1S"`,
      `"CR2"`, `"CR3"`. See
      [`?clubSandwich::vcovCR`](http://jepusto.github.io/clubSandwich/reference/vcovCR.md)

    - Bootstrap: `"BS"`, `"xy"`, `"residual"`, `"wild"`, `"mammen"`,
      `"fractional"`, `"jackknife"`, `"norm"`, `"webb"`. See
      [`?sandwich::vcovBS`](https://sandwich.R-Forge.R-project.org/reference/vcovBS.html)

    - Other `sandwich` package functions: `"HAC"`, `"PC"`, `"CL"`,
      `"OPG"`, `"PL"`.

- vcov_args:

  List of arguments to be passed to the function identified by the
  `vcov` argument. This function is typically supplied by the
  **sandwich** or **clubSandwich** packages. Please refer to their
  documentation (e.g.,
  [`?sandwich::vcovHAC`](https://sandwich.R-Forge.R-project.org/reference/vcovHAC.html))
  to see the list of available arguments. If no estimation type
  (argument `type`) is given, the default type for `"HC"` equals the
  default from the **sandwich** package; for type `"CR"`, the default is
  set to `"CR3"`.

- keep:

  Character containing a regular expression pattern that describes the
  parameters that should be included (for `keep`) or excluded (for
  `drop`) in the returned data frame. `keep` may also be a named list of
  regular expressions. All non-matching parameters will be removed from
  the output. If `keep` is a character vector, every parameter name in
  the *"Parameter"* column that matches the regular expression in `keep`
  will be selected from the returned data frame (and vice versa, all
  parameter names matching `drop` will be excluded). Furthermore, if
  `keep` has more than one element, these will be merged with an `OR`
  operator into a regular expression pattern like this:
  `"(one|two|three)"`. If `keep` is a named list of regular expression
  patterns, the names of the list-element should equal the column name
  where selection should be applied. This is useful for model objects
  where
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  returns multiple columns with parameter components, like in
  [`model_parameters.lavaan()`](https://easystats.github.io/parameters/reference/model_parameters.principal.md).
  Note that the regular expression pattern should match the parameter
  names as they are stored in the returned data frame, which can be
  different from how they are printed. Inspect the `$Parameter` column
  of the parameters table to get the exact parameter names.

- drop:

  See `keep`.

- verbose:

  Toggle warnings and messages.

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

- x:

  An object returned by
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md).

- digits:

  Number of digits for rounding or significant figures. May also be
  `"signif"` to return significant figures or `"scientific"` to return
  scientific notation. Control the number of digits by adding the value
  as suffix, e.g. `digits = "scientific4"` to have scientific notation
  with 4 decimal places, or `digits = "signif5"` for 5 significant
  figures (see also [`signif()`](https://rdrr.io/r/base/Round.html)).

- format:

  Name of output-format, as string. If `NULL` (or `"text"`), returned
  output is used for basic printing. Can be one of `NULL` (the default)
  resp. `"text"` for plain text, `"markdown"` (or `"md"`) for markdown
  and `"html"` for HTML output. A special option is `"tt"`, which
  creates a
  [`tinytable::tt()`](https://vincentarelbundock.github.io/tinytable/man/tt.html)
  object, where the output format is dependent on the context where the
  table is used, i.e. it can be markdown format when `export_table()` is
  used in markdown files, or LaTeX format when creating PDFs etc.

- ci_width:

  Minimum width of the returned string for confidence intervals. If not
  `NULL` and width is larger than the string's length, leading
  whitespaces are added to the string. If `width="auto"`, width will be
  set to the length of the longest string.

- ci_brackets:

  Logical, if `TRUE` (default), CI-values are encompassed in square
  brackets (else in parentheses).

- pretty_names:

  Can be `TRUE`, which will return "pretty" (i.e. more human readable)
  parameter names. Or `"labels"`, in which case value and variable
  labels will be used as parameters names. The latter only works for
  "labelled" data, i.e. if the data used to fit the model had `"label"`
  and `"labels"` attributes. See also section *Global Options to
  Customize Messages when Printing*.

## Value

A data frame with p-values and compatibility intervals.

## Details

### Compatibility intervals and continuous *p*-values for different estimate values

`p_function()` only returns the compatibility interval estimates, not
the related *p*-values. The reason for this is because the *p*-value for
a given estimate value is just `1 - CI_level`. The values indicating the
lower and upper limits of the intervals are the related estimates
associated with the *p*-value. E.g., if a parameter `x` has a 75%
compatibility interval of `(0.81, 1.05)`, then the *p*-value for the
estimate value of `0.81` would be `1 - 0.75`, which is `0.25`. This
relationship is more intuitive and better to understand when looking at
the plots (using
[`plot()`](https://rdrr.io/r/graphics/plot.default.html)).

### Conditional versus unconditional interpretation of *p*-values and intervals

`p_function()`, and in particular its
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) method, aims at
re-interpreting *p*-values and confidence intervals (better named:
*compatibility* intervals) in *unconditional* terms. Instead of
referring to the long-term property and repeated trials when
interpreting interval estimates (so-called "aleatory probability",
*Schweder 2018*), and assuming that all underlying assumptions are
correct and met, `p_function()` interprets *p*-values in a Fisherian way
as "*continuous* measure of evidence against the very test hypothesis
*and* entire model (all assumptions) used to compute it" (*P-Values Are
Tough and S-Values Can Help*, lesslikely.com/statistics/s-values; see
also *Amrhein and Greenland 2022*).

The common definition of p-values can be considered as "conditional"
interpretation:

*The p-value is the probability of obtaining test results at least as
extreme as the result actually observed, under the assumption that the
null hypothesis is correct (Wikipedia).*

However, this definition or interpretation is inadequate because it only
refers to the test hypothesis (often the null hypothesis), which is only
one component of the entire model that is being tested. Thus, *Greenland
et al. 2022* suggest an "unconditional" interpretation.

This interpretation as a continuous measure of evidence against the test
hypothesis and the entire model used to compute it can be seen in the
figure below (taken from *P-Values Are Tough and S-Values Can Help*,
lesslikely.com/statistics/s-values). The "conditional" interpretation of
*p*-values and interval estimates (A) implicitly assumes certain
assumptions to be true, thus the interpretation is "conditioned" on
these assumptions (i.e. assumptions are taken as given, only the
hypothesis is tested). The unconditional interpretation (B), however,
questions *all* these assumptions.

A non-significant p-value could occur because the test hypothesis is
false, but could also be the result of any of the model assumptions
being incorrect.

\
![Conditional versus unconditional interpretations of
P-values](figures/unconditional_interpretation.png)\

"Emphasizing unconditional interpretations helps avoid overconfident and
misleading inferences in light of uncertainties about the assumptions
used to arrive at the statistical results." (*Greenland et al. 2022*).

**Note:** The term "conditional" as used by Rafi and Greenland probably
has a slightly different meaning than normally. "Conditional" in this
notion means that all model assumptions are taken as given - it should
not be confused with terms like "conditional probability". See also
*Greenland et al. 2022* for a detailed elaboration on this issue.

In other words, the term compatibility interval emphasizes "the
dependence of the *p*-value on the assumptions as well as on the data,
recognizing that *p*\<0.05 can arise from assumption violations even if
the effect under study is null" (*Gelman/Greenland 2019*).

### Probabilistic interpretation of p-values and compatibility intervals

Schweder (2018) resp. Schweder and Hjort (2016) (and others) argue that
confidence curves (as produced by `p_function()`) have a valid
probabilistic interpretation. They distinguish between *aleatory
probability*, which describes the aleatory stochastic element of a
distribution *ex ante*, i.e. before the data are obtained. This is the
classical interpretation of confidence intervals following the
Neyman-Pearson school of statistics. However, there is also an *ex post*
probability, called *epistemic* probability, for confidence curves. The
shift in terminology from *confidence* intervals to *compatibility*
intervals may help emphasizing this interpretation.

In this sense, the probabilistic interpretation of *p*-values and
compatibility intervals is "conditional" - on the data *and* model
assumptions (which is in line with the *"unconditional"* interpretation
in the sense of Rafi and Greenland).

Ascribing a probabilistic interpretation to one realized confidence
interval is possible without repeated sampling of the specific
experiment. Important is the assumption that a *sampling distribution*
is a good description of the variability of the parameter (*Vos and
Holbert 2022*). At the core, the interpretation of a confidence interval
is "I assume that this sampling distribution is a good description of
the uncertainty of the parameter. If that's a good assumption, then the
values in this interval are the most plausible or compatible with the
data". The source of confidence in probability statements is the
assumption that the selected sampling distribution is appropriate.

"The realized confidence distribution is clearly an epistemic
probability distribution" (*Schweder 2018*). In Bayesian words,
compatibility intervals (or confidence distributons, or consonance
curves) are "posteriors without priors" (*Schweder, Hjort, 2003*).

The *p*-value indicates the degree of compatibility of the endpoints of
the interval at a given confidence level with (1) the observed data and
(2) model assumptions. The observed point estimate (*p*-value = 1) is
the value estimated to be *most compatible* with the data and model
assumptions, whereas values values far from the observed point estimate
(where *p* approaches 0) are least compatible with the data and model
assumptions (*Schweder and Hjort 2016, pp. 60-61; Amrhein and Greenland
2022*). In this regards, *p*-values are statements about *confidence* or
*compatibility*: The p-value is not an absolute measure of evidence for
a model (such as the null/alternative model), it is a continuous measure
of the compatibility of the observed data with the model used to compute
it (*Greenland et al. 2016*, *Greenland 2023*). Going one step further,
and following *Schweder*, p-values can be considered as *epistemic
probability* - "not necessarily of the hypothesis being true, but of it
*possibly* being true" (*Schweder 2018*). Hence, the interpretation of
*p*-values might be guided using
[`bayestestR::p_to_pd()`](https://easystats.github.io/bayestestR/reference/pd_to_p.html).

### Probability or compatibility?

We here presented the discussion of p-values and confidence intervals
from the perspective of two paradigms, one saying that probability
statements can be made, one saying that interpretation is guided in
terms of "compatibility". Cox and Hinkley say, "interval estimates
cannot be taken as probability statements" (*Cox and Hinkley 1979:
208*), which conflicts with the Schweder and Hjort confidence
distribution school. However, if you view interval estimates as being
intervals of values being consistent with the data, this comes close to
the idea of (epistemic) probability. We do not believe that these two
paradigms contradict or exclude each other. Rather, the aim is to
emphasize the one point of view or the other, i.e. to place the
linguistic nuances either on 'compatibility' or 'probability'.

The main take-away is *not* to interpret p-values as dichotomous
decisions that distinguish between "we found an effect" (statistically
significant)" vs. "we found no effect" (statistically not significant)
(*Altman and Bland 1995*).

### Compatibility intervals - is their interpretation "conditional" or not?

The fact that the term "conditional" is used in different meanings in
statistics, is confusing. Thus, we would summarize the (probabilistic)
interpretation of compatibility intervals as follows: The intervals are
built from the data *and* our modeling assumptions. The accuracy of the
intervals depends on our model assumptions. If a value is outside the
interval, that might be because (1) that parameter value isn't supported
by the data, or (2) the modeling assumptions are a poor fit for the
situation. When we make bad assumptions, the compatibility interval
might be too wide or (more commonly and seriously) too narrow, making us
think we know more about the parameter than is warranted.

When we say "there is a 95% chance the true value is in the interval",
that is a statement of *epistemic probability* (i.e. description of
uncertainty related to our knowledge or belief). When we talk about
repeated samples or sampling distributions, that is referring to
*aleatoric* (physical properties) probability. Frequentist inference is
built on defining estimators with known *aleatoric* probability
properties, from which we can draw *epistemic* probabilistic statements
of uncertainty (*Schweder and Hjort 2016*).

### Functions in the parameters package to check for effect existence and significance

The **parameters** package provides several options or functions to aid
statistical inference. Beyond `p_function()`, there are, for example:

- [`equivalence_test()`](https://easystats.github.io/parameters/reference/equivalence_test.lm.md),
  to compute the (conditional) equivalence test for frequentist models

- [`p_significance()`](https://easystats.github.io/parameters/reference/p_significance.lm.md),
  to compute the probability of *practical significance*, which can be
  conceptualized as a unidirectional equivalence test

- the `pd` argument (setting `pd = TRUE`) in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  includes a column with the *probability of direction*, i.e. the
  probability that a parameter is strictly positive or negative. See
  [`bayestestR::p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.html)
  for details. If plotting is desired, the
  [`p_direction()`](https://easystats.github.io/parameters/reference/p_direction.lm.md)
  function can be used, together with
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

- the `s_value` argument (setting `s_value = TRUE`) in
  [`model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.md)
  replaces the p-values with their related *S*-values (*Rafi and
  Greenland 2020*)

- finally, it is possible to generate distributions of model
  coefficients by generating bootstrap-samples (setting
  `bootstrap = TRUE`) or simulating draws from model coefficients using
  [`simulate_model()`](https://easystats.github.io/parameters/reference/simulate_model.md).
  These samples can then be treated as "posterior samples" and used in
  many functions from the **bayestestR** package.

## Note

Curently, `p_function()` computes intervals based on Wald t- or
z-statistic. For certain models (like mixed models), profiled intervals
may be more accurate, however, this is currently not supported.

## References

- Altman DG, Bland JM. Absence of evidence is not evidence of absence.
  BMJ. 1995;311(7003):485.
  [doi:10.1136/bmj.311.7003.485](https://doi.org/10.1136/bmj.311.7003.485)

- Amrhein V, Greenland S. Discuss practical importance of results based
  on interval estimates and p-value functions, not only on point
  estimates and null p-values. Journal of Information Technology
  2022;37:316–20.
  [doi:10.1177/02683962221105904](https://doi.org/10.1177/02683962221105904)

- Cox DR, Hinkley DV. 1979. Theoretical Statistics. 6th edition. Chapman
  and Hall/CRC

- Fraser DAS. The P-value function and statistical inference. The
  American Statistician. 2019;73(sup1):135-147.
  [doi:10.1080/00031305.2018.1556735](https://doi.org/10.1080/00031305.2018.1556735)

- Gelman A, Greenland S. Are confidence intervals better termed
  "uncertainty intervals"? BMJ (2019)l5381.
  [doi:10.1136/bmj.l5381](https://doi.org/10.1136/bmj.l5381)

- Greenland S, Rafi Z, Matthews R, Higgs M. To Aid Scientific Inference,
  Emphasize Unconditional Compatibility Descriptions of
  Statistics. (2022) https://arxiv.org/abs/1909.08583v7 (Accessed
  November 10, 2022)

- Greenland S, Senn SJ, Rothman KJ, Carlin JB, Poole C, Goodman SN, et
  al. (2016). Statistical tests, P values, confidence intervals, and
  power: A guide to misinterpretations. European Journal of
  Epidemiology. 31:337-350.
  [doi:10.1007/s10654-016-0149-3](https://doi.org/10.1007/s10654-016-0149-3)

- Greenland S (2023). Divergence versus decision P-values: A distinction
  worth making in theory and keeping in practice: Or, how divergence
  P-values measure evidence even when decision P-values do not. Scand J
  Statist, 50(1), 54-88.

- Rafi Z, Greenland S. Semantic and cognitive tools to aid statistical
  science: Replace confidence and significance by compatibility and
  surprise. BMC Medical Research Methodology. 2020;20(1):244.
  [doi:10.1186/s12874-020-01105-9](https://doi.org/10.1186/s12874-020-01105-9)

- Schweder T. Confidence is epistemic probability for empirical science.
  Journal of Statistical Planning and Inference (2018) 195:116–125.
  [doi:10.1016/j.jspi.2017.09.016](https://doi.org/10.1016/j.jspi.2017.09.016)

- Schweder T, Hjort NL. Confidence and Likelihood. Scandinavian Journal
  of Statistics. 2002;29(2):309-332.
  [doi:10.1111/1467-9469.00285](https://doi.org/10.1111/1467-9469.00285)

- Schweder T, Hjort NL. Frequentist analogues of priors and posteriors.
  In Stigum, B. (ed.), Econometrics and the Philosophy of Economics:
  Theory Data Confrontation in Economics, pp. 285-217. Princeton
  University Press, Princeton, NJ, 2003

- Schweder T, Hjort NL. Confidence, Likelihood, Probability: Statistical
  inference with confidence distributions. Cambridge University Press,
  2016.

- Vos P, Holbert D. Frequentist statistical inference without repeated
  sampling. Synthese 200, 89 (2022).
  [doi:10.1007/s11229-022-03560-x](https://doi.org/10.1007/s11229-022-03560-x)

## See also

See also
[`equivalence_test()`](https://easystats.github.io/bayestestR/reference/equivalence_test.html)
and
[`p_significance()`](https://easystats.github.io/bayestestR/reference/p_significance.html)
for functions related to checking effect existence and significance.

## Examples

``` r
model <- lm(Sepal.Length ~ Species, data = iris)
p_function(model)
#> Consonance Function
#> 
#> Parameter            |       25% CI |       50% CI |       75% CI |       95% CI
#> --------------------------------------------------------------------------------
#> (Intercept)          | [4.98, 5.03] | [4.96, 5.06] | [4.92, 5.09] | [4.86, 5.15]
#> Species [versicolor] | [0.90, 0.96] | [0.86, 1.00] | [0.81, 1.05] | [0.73, 1.13]
#> Species [virginica]  | [1.55, 1.61] | [1.51, 1.65] | [1.46, 1.70] | [1.38, 1.79]

model <- lm(mpg ~ wt + as.factor(gear) + am, data = mtcars)
result <- p_function(model)

# single panels
plot(result, n_columns = 2)


# integrated plot, the default
plot(result)
```

# Arguments passed to or from other methods. For instance, when default methods, glm (almost default)

#################### .default ----------------------

#' Model Parameters
#'
#' Compute and extract model parameters. The available options and arguments depend
#' on the modeling **package** and model `class`. Follow one of these links to read
#' the model-specific documentation:
#' - [Default method][model_parameters.default()]: `lm`, `glm`, **stats**, **censReg**,
#'   **MASS**, **survey**, ...
#' - [Additive models][model_parameters.cgam()]: **bamlss**, **gamlss**, **mgcv**,
#'   **scam**, **VGAM**, `Gam` (although the output of `Gam` is more Anova-alike),
#'   `gamm`, ...
#' - [ANOVA][model_parameters.aov()]: **afex**, `aov`, `anova`, `Gam`, ...
#' - [Bayesian][model_parameters.brmsfit()]: **BayesFactor**, **blavaan**, **brms**,
#'   **MCMCglmm**, **posterior**, **rstanarm**, `bayesQR`, `bcplm`, `BGGM`, `blmrm`,
#'   `blrm`, `mcmc.list`, `MCMCglmm`, ...
#' - [Clustering][model_parameters.hclust()]: **hclust**, **kmeans**, **mclust**, **pam**, ...
#' - [Correlations, t-tests, etc.][model_parameters.htest()]: **lmtest**, `htest`,
#'   `pairwise.htest`, ...
#' - [Meta-Analysis][model_parameters.rma()]: **metaBMA**, **metafor**, **metaplus**, ...
#' - [Mixed models][model_parameters.glmmTMB()]: **cplm**, **glmmTMB**, **lme4**,
#'   **lmerTest**, **nlme**, **ordinal**, **robustlmm**, **spaMM**, `mixed`, `MixMod`, ...
#' - [Multinomial, ordinal and cumulative link][model_parameters.mlm()]: **brglm2**,
#'   **DirichletReg**, **nnet**, **ordinal**, `mlm`, ...
#' - [Multiple imputation][model_parameters.mira()]: **mice**
#' - [PCA, FA, CFA, SEM][model_parameters.principal()]: **FactoMineR**, **lavaan**,
#'   **psych**, `sem`, ...
#' - [Zero-inflated and hurdle][model_parameters.zcpglm()]: **cplm**, **mhurdle**,
#'   **pscl**, ...
#' - [Other models][model_parameters.glimML()]: **aod**, **bbmle**, **betareg**,
#'   **emmeans**, **epiR**, **glmx**, **ivfixed**, **ivprobit**, **JRM**,
#'   **lmodel2**, **logitsf**, **marginaleffects**, **margins**, **maxLik**,
#'   **mediation**, **mfx**, **multcomp**, **mvord**, **plm**, **PMCMRplus**,
#'   **quantreg**, **selection**, **systemfit**, **tidymodels**, **varEST**,
#'   **WRS2**, `bfsl`, `deltaMethod`, `fitdistr`, `mjoint`, `mle`, `model.avg`,
#'   ...
#'
#' A full overview can be found here:
#' https://easystats.github.io/parameters/reference/
#'
#' @param model Statistical Model.
#' @param ... Arguments passed to or from other methods. Non-documented
#' arguments are
#' - `digits`, `p_digits`, `ci_digits` and `footer_digits` to set the number of
#'   digits for the output. `groups` can be used to group coefficients. These
#'   arguments will be passed to the print-method, or can directly be used in
#'   `print()`, see documentation in [`print.parameters_model()`].
#' - If `s_value = TRUE`, the p-value will be replaced by the S-value in the
#'   output (cf. _Rafi and Greenland 2020_).
#' - `pd` adds an additional column with the _probability of direction_ (see
#'   [`bayestestR::p_direction()`] for details). Furthermore, see 'Examples' in
#'   [`model_parameters.default()`].
#' - For developers, whose interest mainly is to get a "tidy" data frame of
#'   model summaries, it is recommended to set `pretty_names = FALSE` to speed
#'   up computation of the summary table.
#'
#' @seealso [insight::standardize_names()] to rename columns into a consistent,
#'   standardized naming scheme.
#'
#' @note The [`print()`][print.parameters_model] method has several
#'   arguments to tweak the output. There is also a
#'   [`plot()`-method](https://easystats.github.io/see/articles/parameters.html)
#'   implemented in the
#'   [**see**-package](https://easystats.github.io/see/), and a dedicated
#'   method for use inside rmarkdown files,
#'   [`print_md()`][print_md.parameters_model]. \cr \cr **For developers**, if
#'   speed performance is an issue, you can use the (undocumented) `pretty_names`
#'   argument, e.g. `model_parameters(..., pretty_names = FALSE)`. This will
#'   skip the formatting of the coefficient names and makes `model_parameters()`
#'   faster.
#'
#' @section Standardization of model coefficients:
#' Standardization is based on [standardize_parameters()]. In case
#' of `standardize = "refit"`, the data used to fit the model will be
#' standardized and the model is completely refitted. In such cases, standard
#' errors and confidence intervals refer to the standardized coefficient. The
#' default, `standardize = "refit"`, never standardizes categorical predictors
#' (i.e. factors), which may be a different behaviour compared to other R
#' packages or other software packages (like SPSS). To mimic behaviour of SPSS
#' or packages such as **lm.beta**, use `standardize = "basic"`.
#'
#' @section Standardization Methods:
#' - **refit**: This method is based on a complete model re-fit with a
#' standardized version of the data. Hence, this method is equal to
#' standardizing the variables before fitting the model. It is the "purest" and
#' the most accurate (Neter et al., 1989), but it is also the most
#' computationally costly and long (especially for heavy models such as Bayesian
#' models). This method is particularly recommended for complex models that
#' include interactions or transformations (e.g., polynomial or spline terms).
#' The `robust` (default to `FALSE`) argument enables a robust standardization
#' of data, i.e., based on the `median` and `MAD` instead of the `mean` and
#' `SD`. **See [`datawizard::standardize()`] for more details.**
#' **Note** that `standardize_parameters(method = "refit")` may not return
#' the same results as fitting a model on data that has been standardized with
#' `standardize()`; `standardize_parameters()` used the data used by the model
#' fitting function, which might not be same data if there are missing values.
#' see the `remove_na` argument in `standardize()`.
#'
#' - **posthoc**: Post-hoc standardization of the parameters, aiming at
#' emulating the results obtained by "refit" without refitting the model. The
#' coefficients are divided by the standard deviation (or MAD if `robust`) of
#' the outcome (which becomes their expression 'unit'). Then, the coefficients
#' related to numeric variables are additionally multiplied by the standard
#' deviation (or MAD if `robust`) of the related terms, so that they correspond
#' to changes of 1 SD of the predictor (e.g., "A change in 1 SD of `x` is
#' related to a change of 0.24 of the SD of `y`). This does not apply to binary
#' variables or factors, so the coefficients are still related to changes in
#' levels. This method is not accurate and tend to give aberrant results when
#' interactions are specified.
#'
#' - **basic**: This method is similar to `method = "posthoc"`, but treats all
#' variables as continuous: it also scales the coefficient by the standard
#' deviation of model's matrix' parameter of factors levels (transformed to
#' integers) or binary predictors. Although being inappropriate for these cases,
#' this method is the one implemented by default in other software packages,
#' such as [lm.beta::lm.beta()].
#'
#' - **smart** (Standardization of Model's parameters with Adjustment,
#' Reconnaissance and Transformation - *experimental*): Similar to `method =
#' "posthoc"` in that it does not involve model refitting. The difference is
#' that the SD (or MAD if `robust`) of the response is computed on the relevant
#' section of the data. For instance, if a factor with 3 levels A (the
#' intercept), B and C is entered as a predictor, the effect corresponding to B
#' vs. A will be scaled by the variance of the response at the intercept only.
#' As a results, the coefficients for effects of factors are similar to a Glass'
#' delta.
#'
#' - **pseudo** (*for 2-level (G)LMMs only*): In this (post-hoc) method, the
#' response and the predictor are standardized based on the level of prediction
#' (levels are detected with [performance::check_group_variation()]): Predictors
#' are standardized based on their SD at level of prediction (see also
#' [datawizard::demean()]); The outcome (in linear LMMs) is standardized based
#' on a fitted random-intercept-model, where `sqrt(random-intercept-variance)`
#' is used for level 2 predictors, and `sqrt(residual-variance)` is used for
#' level 1 predictors (Hoffman 2015, page 342). A warning is given when a
#' within-group variable is found to have access between-group variance.
#'
#' See also [package vignette](https://easystats.github.io/parameters/articles/standardize_parameters_effsize.html).
#'
#' @section Labeling the Degrees of Freedom:
#' Throughout the **parameters** package, we decided to label the residual
#' degrees of freedom *df_error*. The reason for this is that these degrees
#' of freedom not always refer to the residuals. For certain models, they refer
#' to the estimate error - in a linear model these are the same, but in - for
#' instance - any mixed effects model, this isn't strictly true. Hence, we
#' think that `df_error` is the most generic label for these degrees of
#' freedom.
#'
#' @section Confidence intervals and approximation of degrees of freedom:
#' There are different ways of approximating the degrees of freedom depending
#' on different assumptions about the nature of the model and its sampling
#' distribution. The `ci_method` argument modulates the method for computing degrees
#' of freedom (df) that are used to calculate confidence intervals (CI) and the
#' related p-values. Following options are allowed, depending on the model
#' class:
#'
#' **Classical methods:**
#'
#' Classical inference is generally based on the **Wald method**.
#' The Wald approach to inference computes a test statistic by dividing the
#' parameter estimate by its standard error (Coefficient / SE),
#' then comparing this statistic against a t- or normal distribution.
#' This approach can be used to compute CIs and p-values.
#'
#' `"wald"`:
#' - Applies to *non-Bayesian models*. For *linear models*, CIs
#'   computed using the Wald method (SE and a *t-distribution with residual df*);
#'   p-values computed using the Wald method with a *t-distribution with residual df*.
#'   For other models, CIs computed using the Wald method (SE and a *normal distribution*);
#'   p-values computed using the Wald method with a *normal distribution*.
#'
#' `"normal"`
#' - Applies to *non-Bayesian models*. Compute Wald CIs and p-values,
#'   but always use a normal distribution.
#'
#' `"residual"`
#' - Applies to *non-Bayesian models*. Compute Wald CIs and p-values,
#'   but always use a *t-distribution with residual df* when possible. If the
#'   residual df for a model cannot be determined, a normal distribution is
#'   used instead.
#'
#' **Methods for mixed models:**
#'
#' Compared to fixed effects (or single-level) models, determining appropriate
#' df for Wald-based inference in mixed models is more difficult.
#' See [the R GLMM FAQ](https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#what-are-the-p-values-listed-by-summaryglmerfit-etc.-are-they-reliable)
#' for a discussion.
#'
#' Several approximate methods for computing df are available, but you should
#' also consider instead using profile likelihood (`"profile"`) or bootstrap ("`boot"`)
#' CIs and p-values instead.
#'
#' `"satterthwaite"`
#' - Applies to *linear mixed models*. CIs computed using the
#'   Wald method (SE and a *t-distribution with Satterthwaite df*); p-values
#'   computed using the Wald method with a *t-distribution with Satterthwaite df*.
#'
#' `"kenward"`
#' - Applies to *linear mixed models*. CIs computed using the Wald
#'   method (*Kenward-Roger SE* and a *t-distribution with Kenward-Roger df*);
#'   p-values computed using the Wald method with *Kenward-Roger SE and t-distribution with Kenward-Roger df*.
#'
#' `"ml1"`
#' - Applies to *linear mixed models*. CIs computed using the Wald
#'   method (SE and a *t-distribution with m-l-1 approximated df*); p-values
#'   computed using the Wald method with a *t-distribution with m-l-1 approximated df*.
#'   See [`ci_ml1()`].
#'
#' `"betwithin"`
#' - Applies to *linear mixed models* and *generalized linear mixed models*.
#'   CIs computed using the Wald method (SE and a *t-distribution with between-within df*);
#'   p-values computed using the Wald method with a *t-distribution with between-within df*.
#'   See [`ci_betwithin()`].
#'
#' **Likelihood-based methods:**
#'
#' Likelihood-based inference is based on comparing the likelihood for the
#' maximum-likelihood estimate to the the likelihood for models with one or more
#' parameter values changed (e.g., set to zero or a range of alternative values).
#' Likelihood ratios for the maximum-likelihood and alternative models are compared
#' to a \eqn{\chi}-squared distribution to compute CIs and p-values.
#'
#' `"profile"`
#' - Applies to *non-Bayesian models* of class `glm`, `polr`, `merMod` or `glmmTMB`.
#'   CIs computed by *profiling the likelihood curve for a parameter*, using
#'   linear interpolation to find where likelihood ratio equals a critical value;
#'   p-values computed using the Wald method with a *normal-distribution* (note:
#'   this might change in a future update!)
#'
#' `"uniroot"`
#' - Applies to *non-Bayesian models* of class `glmmTMB`. CIs
#'   computed by *profiling the likelihood curve for a parameter*, using root
#'   finding to find where likelihood ratio equals a critical value; p-values
#'   computed using the Wald method with a *normal-distribution* (note: this
#'   might change in a future update!)
#'
#' **Methods for bootstrapped or Bayesian models:**
#'
#' Bootstrap-based inference is based on **resampling** and refitting the model
#' to the resampled datasets. The distribution of parameter estimates across
#' resampled datasets is used to approximate the parameter's sampling
#' distribution. Depending on the type of model, several different methods for
#' bootstrapping and constructing CIs and p-values from the bootstrap
#' distribution are available.
#'
#' For Bayesian models, inference is based on drawing samples from the model
#' posterior distribution.
#'
#' `"quantile"` (or `"eti"`)
#' - Applies to *all models (including Bayesian models)*.
#'   For non-Bayesian models, only applies if `bootstrap = TRUE`. CIs computed
#'   as *equal tailed intervals* using the quantiles of the bootstrap or
#'   posterior samples; p-values are based on the *probability of direction*.
#'   See [`bayestestR::eti()`].
#'
#' `"hdi"`
#' - Applies to *all models (including Bayesian models)*. For non-Bayesian
#'   models, only applies if `bootstrap = TRUE`. CIs computed as *highest density intervals*
#'   for the bootstrap or posterior samples; p-values are based on the *probability of direction*.
#'   See [`bayestestR::hdi()`].
#'
#' `"bci"` (or `"bcai"`)
#' - Applies to *all models (including Bayesian models)*.
#'   For non-Bayesian models, only applies if `bootstrap = TRUE`. CIs computed
#'   as *bias corrected and accelerated intervals* for the bootstrap or
#'   posterior samples; p-values are based on the *probability of direction*.
#'   See [`bayestestR::bci()`].
#'
#' `"si"`
#' - Applies to *Bayesian models* with proper priors. CIs computed as
#'   *support intervals* comparing the posterior samples against the prior samples;
#'   p-values are based on the *probability of direction*. See [`bayestestR::si()`].
#'
#' `"boot"`
#' - Applies to *non-Bayesian models* of class `merMod`. CIs computed
#'   using *parametric bootstrapping* (simulating data from the fitted model);
#'   p-values computed using the Wald method with a *normal-distribution)*
#'   (note: this might change in a future update!).
#'
#' For all iteration-based methods other than `"boot"`
#' (`"hdi"`, `"quantile"`, `"ci"`, `"eti"`, `"si"`, `"bci"`, `"bcai"`),
#' p-values are based on the probability of direction ([`bayestestR::p_direction()`]),
#' which is converted into a p-value using [`bayestestR::pd_to_p()`].
#'
#' @section Statistical inference - how to quantify evidence:
#' There is no standardized approach to drawing conclusions based on the
#' available data and statistical models. A frequently chosen but also much
#' criticized approach is to evaluate results based on their statistical
#' significance (*Amrhein et al. 2017*).
#'
#' A more sophisticated way would be to test whether estimated effects exceed
#' the "smallest effect size of interest", to avoid even the smallest effects
#' being considered relevant simply because they are statistically significant,
#' but clinically or practically irrelevant (*Lakens et al. 2018, Lakens 2024*).
#'
#' A rather unconventional approach, which is nevertheless advocated by various
#' authors, is to interpret results from classical regression models either in
#' terms of probabilities, similar to the usual approach in Bayesian statistics
#' (*Schweder 2018; Schweder and Hjort 2003; Vos 2022*) or in terms of relative
#' measure of "evidence" or "compatibility" with the data (*Greenland et al. 2022;
#' Rafi and Greenland 2020*), which nevertheless comes close to a probabilistic
#' interpretation.
#'
#' A more detailed discussion of this topic is found in the documentation of
#' [`p_function()`].
#'
#' The **parameters** package provides several options or functions to aid
#' statistical inference. These are, for example:
#' - [`equivalence_test()`][equivalence_test.lm], to compute the (conditional)
#'   equivalence test for frequentist models
#' - [`p_significance()`][p_significance.lm], to compute the probability of
#'   *practical significance*, which can be conceptualized as a unidirectional
#'   equivalence test
#' - [`p_function()`], or _consonance function_, to compute p-values and
#'   compatibility (confidence) intervals for statistical models
#' - the `pd` argument (setting `pd = TRUE`) in `model_parameters()` includes
#'   a column with the *probability of direction*, i.e. the probability that a
#'   parameter is strictly positive or negative. See [`bayestestR::p_direction()`]
#'   for details. If plotting is desired, the [`p_direction()`][p_direction.lm]
#'   function can be used, together with `plot()`.
#' - the `s_value` argument (setting `s_value = TRUE`) in `model_parameters()`
#'   replaces the p-values with their related _S_-values (*Rafi and Greenland 2020*)
#' - finally, it is possible to generate distributions of model coefficients by
#'   generating bootstrap-samples (setting `bootstrap = TRUE`) or simulating
#'   draws from model coefficients using [`simulate_model()`]. These samples
#'   can then be treated as "posterior samples" and used in many functions from
#'   the **bayestestR** package.
#'
#' Most of the above shown options or functions derive from methods originally
#' implemented for Bayesian models (*Makowski et al. 2019*). However, assuming
#' that model assumptions are met (which means, the model fits well to the data,
#' the correct model is chosen that reflects the data generating process
#' (distributional model family) etc.), it seems appropriate to interpret
#' results from classical frequentist models in a "Bayesian way" (more details:
#' documentation in [`p_function()`]).
#'
#' @inheritSection format_parameters Interpretation of Interaction Terms
#' @inheritSection print.parameters_model Global Options to Customize Messages and Tables when Printing
#'
#' @references
#'
#'   - Amrhein, V., Korner-Nievergelt, F., and Roth, T. (2017). The earth is
#'     flat (p > 0.05): Significance thresholds and the crisis of unreplicable
#'     research. PeerJ, 5, e3544. \doi{10.7717/peerj.3544}
#'
#'   - Greenland S, Rafi Z, Matthews R, Higgs M. To Aid Scientific Inference,
#'     Emphasize Unconditional Compatibility Descriptions of Statistics. (2022)
#'     https://arxiv.org/abs/1909.08583v7 (Accessed November 10, 2022)
#'
#'   - Hoffman, L. (2015). Longitudinal analysis: Modeling within-person
#'     fluctuation and change. Routledge.
#'
#'   - Lakens, D. (2024). Improving Your Statistical Inferences (Version v1.5.1).
#'     Retrieved from https://lakens.github.io/statistical_inferences/.
#'     \doi{10.5281/ZENODO.6409077}
#'
#'   - Lakens, D., Scheel, A. M., and Isager, P. M. (2018). Equivalence Testing
#'     for Psychological Research: A Tutorial. Advances in Methods and Practices
#'     in Psychological Science, 1(2), 259–269.
#'
#'   - Makowski, D., Ben-Shachar, M. S., Chen, S. H. A., and Lüdecke, D. (2019).
#'     Indices of Effect Existence and Significance in the Bayesian Framework.
#'     Frontiers in Psychology, 10, 2767. \doi{10.3389/fpsyg.2019.02767}
#'
#'   - Montiel Olea, J. L., and Plagborg-Møller, M. (2019). Simultaneous
#'     confidence bands: Theory, implementation, and an application to SVARs.
#'     Journal of Applied Econometrics, 34(1), 1–17. \doi{10.1002/jae.2656}
#'
#'   - Neter, J., Wasserman, W., and Kutner, M. H. (1989). Applied linear
#'     regression models.
#'
#'   - Rafi Z, Greenland S. Semantic and cognitive tools to aid statistical
#'     science: replace confidence and significance by compatibility and surprise.
#'     BMC Medical Research Methodology (2020) 20:244.
#'
#'   - Schweder T. Confidence is epistemic probability for empirical science.
#'     Journal of Statistical Planning and Inference (2018) 195:116–125.
#'     \doi{10.1016/j.jspi.2017.09.016}
#'
#'   - Schweder T, Hjort NL. Frequentist analogues of priors and posteriors.
#'     In Stigum, B. (ed.), Econometrics and the Philosophy of Economics: Theory
#'     Data Confrontation in Economics, pp. 285-217. Princeton University Press,
#'     Princeton, NJ, 2003
#'
#'   - Vos P, Holbert D. Frequentist statistical inference without repeated sampling.
#'     Synthese 200, 89 (2022). \doi{10.1007/s11229-022-03560-x}
#'
#' @return A data frame of indices related to the model's parameters.
#' @export
model_parameters <- function(model, ...) {
  UseMethod("model_parameters")
}


# DF naming convention --------------------


# DF column naming
# F has df, df_error
# t has df_error
# z has df_error = Inf
# Chisq has df
# https://github.com/easystats/parameters/issues/455


# Options -------------------------------------

# Add new options to the docs in "print.parameters_model"

# getOption("parameters_info"): show model summary
# getOption("parameters_mixed_info"): show model summary for mixed models
# getOption("parameters_cimethod"): show message about CI approximation
# getOption("parameters_exponentiate"): show warning about exp for log/logit links
# getOption("parameters_labels"): use value/variable labels instead pretty names
# getOption("parameters_interaction"): separator char for interactions
# getOption("parameters_select"): default for the `select` argument


#' @rdname model_parameters
#' @export
parameters <- model_parameters


#' @title Parameters from (General) Linear Models
#' @name model_parameters.default
#'
#' @description Extract and compute indices and measures to describe parameters
#' of (generalized) linear models (GLMs).
#'
#' @param model Model object.
#' @param ci Confidence Interval (CI) level. Default to `0.95` (`95%`).
#' @param bootstrap Should estimates be based on bootstrapped model? If `TRUE`,
#'   then arguments of [Bayesian regressions][model_parameters.brmsfit] apply
#'   (see also [`bootstrap_parameters()`]).
#' @param iterations The number of bootstrap replicates. This only apply in the
#'   case of bootstrapped frequentist models.
#' @param standardize The method used for standardizing the parameters. Can be
#'   `NULL` (default; no standardization), `"refit"` (for re-fitting the model
#'   on standardized data) or one of `"basic"`, `"posthoc"`, `"smart"`,
#'   `"pseudo"`. See 'Details' in [`standardize_parameters()`].
#'   **Importantly**:
#'   - The `"refit"` method does *not* standardize categorical predictors (i.e.
#'     factors), which may be a different behaviour compared to other R packages
#'     (such as **lm.beta**) or other software packages (like SPSS). to mimic
#'     such behaviours, either use `standardize="basic"` or standardize the data
#'     with `datawizard::standardize(force=TRUE)` *before* fitting the model.
#'   - By default, the response (dependent) variable is also standardized, *if
#'     applicable*. Set `include_response = FALSE` to avoid standardization of
#'     the response variable. See details in [`datawizard::standardize.default()`].
#'   - For mixed models, when using methods other than `"refit"`, only the fixed
#'     effects will be standardized.
#'   - Robust estimation (i.e., `vcov` set to a value other than `NULL`) of
#'     standardized parameters only works when `standardize="refit"`.
#' @param exponentiate Logical, indicating whether or not to exponentiate the
#'   coefficients (and related confidence intervals). This is typical for
#'   logistic regression, or more generally speaking, for models with log or
#'   logit links. It is also recommended to use `exponentiate = TRUE` for models
#'   with log-transformed response values. For models with a log-transformed
#'   response variable, when `exponentiate = TRUE`, a one-unit increase in the
#'   predictor is associated with multiplying the outcome by that predictor's
#'   coefficient. **Note:** Delta-method standard errors are also computed (by
#'   multiplying the standard errors by the transformed coefficients). This is
#'   to mimic behaviour of other software packages, such as Stata, but these
#'   standard errors poorly estimate uncertainty for the transformed
#'   coefficient. The transformed confidence interval more clearly captures this
#'   uncertainty. For `compare_parameters()`, `exponentiate = "nongaussian"`
#'   will only exponentiate coefficients from non-Gaussian families.
#' @param p_adjust String value, if not `NULL`, indicates the method to adjust
#'   p-values. See [`stats::p.adjust()`] for details. Further possible
#'   adjustment methods are `"tukey"`, `"scheffe"`, `"sidak"`, `"sup-t"`, and
#'   `"none"` to explicitly disable adjustment for `emmGrid` objects (from
#'   **emmeans**). `"sup-t"` computes simultaneous confidence bands, also called
#'   sup-t confidence band (Montiel Olea & Plagborg-Møller, 2019).
#' @param ci_method Method for computing degrees of freedom for
#'   confidence intervals (CI) and the related p-values. Allowed are following
#'   options (which vary depending on the model class): `"residual"`,
#'   `"normal"`, `"likelihood"`, `"satterthwaite"`, `"kenward"`, `"wald"`,
#'   `"profile"`, `"boot"`, `"uniroot"`, `"ml1"`, `"betwithin"`, `"hdi"`,
#'   `"quantile"`, `"ci"`, `"eti"`, `"si"`, `"bci"`, or `"bcai"`. See section
#'   _Confidence intervals and approximation of degrees of freedom_ in
#'   [`model_parameters()`] for further details. When `ci_method=NULL`, in most
#'   cases `"wald"` is used then.
#' @param include_info Logical, if `TRUE`, prints summary information about the
#'   model (model formula, number of observations, residual standard deviation
#'   and more).
#' @param keep Character containing a regular expression pattern that
#'   describes the parameters that should be included (for `keep`) or excluded
#'   (for `drop`) in the returned data frame. `keep` may also be a
#'   named list of regular expressions. All non-matching parameters will be
#'   removed from the output. If `keep` is a character vector, every parameter
#'   name in the *"Parameter"* column that matches the regular expression in
#'   `keep` will be selected from the returned data frame (and vice versa,
#'   all parameter names matching `drop` will be excluded). Furthermore, if
#'   `keep` has more than one element, these will be merged with an `OR`
#'   operator into a regular expression pattern like this: `"(one|two|three)"`.
#'   If `keep` is a named list of regular expression patterns, the names of the
#'   list-element should equal the column name where selection should be
#'   applied. This is useful for model objects where `model_parameters()`
#'   returns multiple columns with parameter components, like in
#'   [model_parameters.lavaan()]. Note that the regular expression pattern
#'   should match the parameter names as they are stored in the returned data
#'   frame, which can be different from how they are printed. Inspect the
#'   `$Parameter` column of the parameters table to get the exact parameter
#'   names.
#' @param ... Arguments passed to or from other methods. For instance, when
#'   `bootstrap = TRUE`, arguments like `type` or `parallel` are passed down to
#'   `bootstrap_model()`.
#'
#' Further non-documented arguments are:
#'
#' - `digits`, `p_digits`, `ci_digits` and `footer_digits` to set the number of
#'   digits for the output. `groups` can be used to group coefficients. These
#'   arguments will be passed to the print-method, or can directly be used in
#'   `print()`, see documentation in [`print.parameters_model()`].
#' - If `s_value = TRUE`, the p-value will be replaced by the S-value in the
#'   output (cf. _Rafi and Greenland 2020_).
#' - `pd` adds an additional column with the _probability of direction_ (see
#'   [`bayestestR::p_direction()`] for details). Furthermore, see 'Examples' for
#'   this function.
#' - For developers, whose interest mainly is to get a "tidy" data frame of
#'   model summaries, it is recommended to set `pretty_names = FALSE` to speed
#'   up computation of the summary table.
#' @param drop See `keep`.
#' @param verbose Toggle warnings and messages.
#' @inheritParams standard_error
#'
#' @seealso [`insight::standardize_names()`] to rename columns into a
#'   consistent, standardized naming scheme.
#'
#' @inheritSection model_parameters Confidence intervals and approximation of degrees of freedom
#'
#' @examplesIf require("boot", quietly = TRUE) && require("sandwich") && require("clubSandwich") && require("brglm2")
#' library(parameters)
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#'
#' model_parameters(model)
#'
#' # bootstrapped parameters
#' model_parameters(model, bootstrap = TRUE)
#'
#' # standardized parameters
#' model_parameters(model, standardize = "refit")
#'
#' # robust, heteroskedasticity-consistent standard errors
#' model_parameters(model, vcov = "HC3")
#'
#' model_parameters(model,
#'   vcov = "vcovCL",
#'   vcov_args = list(cluster = mtcars$cyl)
#' )
#'
#' # different p-value style in output
#' model_parameters(model, p_digits = 5)
#' model_parameters(model, digits = 3, ci_digits = 4, p_digits = "scientific")
#'
#' # report S-value or probability of direction for parameters
#' model_parameters(model, s_value = TRUE)
#' model_parameters(model, pd = TRUE)
#'
#' \donttest{
#' # logistic regression model
#' model <- glm(vs ~ wt + cyl, data = mtcars, family = "binomial")
#' model_parameters(model)
#'
#' # show odds ratio / exponentiated coefficients
#' model_parameters(model, exponentiate = TRUE)
#'
#' # bias-corrected logistic regression with penalized maximum likelihood
#' model <- glm(
#'   vs ~ wt + cyl,
#'   data = mtcars,
#'   family = "binomial",
#'   method = "brglmFit"
#' )
#' model_parameters(model)
#' }
#' @return A data frame of indices related to the model's parameters.
#' @export
model_parameters.default <- function(model,
                                     ci = 0.95,
                                     ci_method = NULL,
                                     bootstrap = FALSE,
                                     iterations = 1000,
                                     standardize = NULL,
                                     exponentiate = FALSE,
                                     p_adjust = NULL,
                                     vcov = NULL,
                                     vcov_args = NULL,
                                     include_info = getOption("parameters_info", FALSE),
                                     keep = NULL,
                                     drop = NULL,
                                     verbose = TRUE,
                                     ...) {
  # validation check for inputs
  .is_model_valid(model)

  # validation check, warn if unsupported argument is used.
  # unsupported arguments will be removed from the argument list.
  dots <- .check_dots(
    dots = list(...),
    not_allowed = c("include_sigma", "wb_component"),
    class(model)[1],
    verbose = FALSE
  )

  # extract model parameters table, as data frame
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    ci_method = ci_method,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = "Parameter",
    standardize = standardize,
    exponentiate = exponentiate,
    p_adjust = p_adjust,
    include_info = include_info,
    keep_parameters = keep,
    drop_parameters = drop,
    vcov = vcov,
    vcov_args = vcov_args,
    verbose = verbose,
    ...
  )

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}


.fail_error_message <- function(out, model) {
  # tell user if something went wrong...
  if (length(out) == 1 && isTRUE(is.na(out))) {
    insight::format_error(
      paste0(
        "Sorry, `model_parameters()` failed with the following error (possible class `",
        class(model)[1],
        "` not supported):\n"
      ),
      attr(out, "error")
    )
  } else if (is.null(out)) {
    insight::format_error(
      paste0(
        "Sorry, `model_parameters()` does not currently work for objects of class `",
        class(model)[1],
        "`."
      )
    )
  }
}


# helper function for the composition of the parameters table,
# including a bunch of attributes required for further processing
# (like printing etc.)

.model_parameters_generic <- function(model,
                                      ci = 0.95,
                                      bootstrap = FALSE,
                                      iterations = 1000,
                                      merge_by = "Parameter",
                                      standardize = NULL,
                                      exponentiate = FALSE,
                                      effects = "fixed",
                                      component = "conditional",
                                      ci_method = NULL,
                                      p_adjust = NULL,
                                      include_info = FALSE,
                                      keep_parameters = NULL,
                                      drop_parameters = NULL,
                                      vcov = NULL,
                                      vcov_args = NULL,
                                      verbose = TRUE,
                                      ...) {
  dots <- list(...)

  out <- tryCatch(
    {
      # ==== 1. first step, extracting (bootstrapped) model parameters -------

      # Processing, bootstrapped parameters
      if (bootstrap) {
        # set default method for bootstrapped CI
        if (is.null(ci_method) || missing(ci_method)) {
          ci_method <- "quantile"
        }

        fun_args <- list(
          model,
          iterations = iterations,
          ci = ci,
          ci_method = ci_method
        )
        fun_args <- c(fun_args, dots)
        params <- do.call("bootstrap_parameters", fun_args)

        # Processing, non-bootstrapped parameters
      } else {
        # set default method for CI
        if (is.null(ci_method) || missing(ci_method)) {
          ci_method <- "wald"
        }

        fun_args <- list(
          model,
          ci = ci,
          component = component,
          merge_by = merge_by,
          standardize = standardize,
          effects = effects,
          ci_method = ci_method,
          p_adjust = p_adjust,
          keep_parameters = keep_parameters,
          drop_parameters = drop_parameters,
          verbose = verbose,
          vcov = vcov,
          vcov_args = vcov_args
        )
        fun_args <- c(fun_args, dots)
        params <- do.call(".extract_parameters_generic", fun_args)
      }


      # ==== 2. second step, exponentiate -------

      # exponentiate coefficients and SE/CI, if requested
      params <- .exponentiate_parameters(params, model, exponentiate)


      # ==== 3. third step, add information as attributes -------

      # add further information as attributes
      params <- .add_model_parameters_attributes(
        params,
        model,
        ci,
        exponentiate,
        bootstrap,
        iterations,
        ci_method = ci_method,
        p_adjust = p_adjust,
        include_info = include_info,
        verbose = verbose,
        ...
      )

      class(params) <- c("parameters_model", "see_parameters_model", class(params))
      params
    },
    error = function(e) {
      fail <- NA
      attr(fail, "error") <- gsub("  ", " ", gsub("\\n", "", e$message), fixed = TRUE)
      fail
    }
  )

  # check if everything is ok
  .fail_error_message(out, model)

  out
}


#################### .glm ----------------------


#' @export
model_parameters.glm <- function(model,
                                 ci = 0.95,
                                 ci_method = NULL,
                                 bootstrap = FALSE,
                                 iterations = 1000,
                                 standardize = NULL,
                                 exponentiate = FALSE,
                                 p_adjust = NULL,
                                 vcov = NULL,
                                 vcov_args = NULL,
                                 include_info = getOption("parameters_info", FALSE),
                                 keep = NULL,
                                 drop = NULL,
                                 verbose = TRUE,
                                 ...) {
  dots <- list(...)

  # set default
  if (is.null(ci_method)) {
    if (isTRUE(bootstrap)) {
      ci_method <- "quantile"
    } else if (!is.null(vcov) || !is.null(vcov_args)) {
      ci_method <- "wald"
    } else {
      ci_method <- "profile"
    }
  }

  # profiled CIs may take a long time to compute, so we warn the user about it
  if (insight::n_obs(model) > 1e4 && identical(ci_method, "profile")) {
    insight::format_alert(
      "Profiled confidence intervals may take longer time to compute.",
      "Use `ci_method=\"wald\"` for faster computation of CIs."
    )
  }

  # tell user that profiled CIs don't respect vcov-args
  if (identical(ci_method, "profile") && (!is.null(vcov) || !is.null(vcov_args)) && isTRUE(verbose)) {
    insight::format_alert(
      "When `ci_method=\"profile\"`, `vcov` only modifies standard errors, test-statistic and p-values, but not confidence intervals.", # nolint
      "Use `ci_method=\"wald\"` to return confidence intervals based on robust standard errors."
    )
  }

  fun_args <- list(
    model = model,
    ci = ci,
    ci_method = ci_method,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = "Parameter",
    standardize = standardize,
    exponentiate = exponentiate,
    p_adjust = p_adjust,
    include_info = include_info,
    keep_parameters = keep,
    drop_parameters = drop,
    vcov = vcov,
    vcov_args = vcov_args,
    verbose = verbose
  )
  fun_args <- c(fun_args, dots)
  out <- do.call(".model_parameters_generic", fun_args)

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}

#' @export
model_parameters.zoo <- model_parameters.default

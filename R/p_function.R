#' @title p-value or consonance function
#' @name p_function
#'
#' @description Compute p-values and compatibility (confidence) intervals for
#' statistical models, at different levels. This function is also called
#' consonance function. It allows to see which estimates are compatible with
#' the model at various compatibility levels. Use `plot()` to generate plots
#' of the _p_ resp. _consonance_ function and compatibility intervals at
#' different levels.
#'
#' @param ci_levels Vector of scalars, indicating the different levels at which
#' compatibility intervals should be printed or plotted. In plots, these levels
#' are highlighted by vertical lines. It is possible to increase thickness for
#' one or more of these lines by providing a names vector, where the to be
#' highlighted values should be named `"emph"`, e.g
#' `ci_levels = c(0.25, 0.5, emph = 0.95)`.
#'
#' @inheritParams model_parameters
#' @inheritParams model_parameters.default
#' @inheritParams model_parameters.glmmTMB
#' @inheritParams standard_error
#'
#' @note
#' Curently, `p_function()` computes intervals based on Wald t- or z-statistic.
#' For certain models (like mixed models), profiled intervals may be more
#' accurate, however, this is currently not supported.
#'
#' @seealso See also [`equivalence_test()`] and [`p_significance()`] for
#' functions related to checking effect existence and significance.
#'
#' @details
#' ## Compatibility intervals and continuous _p_-values for different estimate values
#'
#' `p_function()` only returns the compatibility interval estimates, not the
#' related _p_-values. The reason for this is because the _p_-value for a
#' given estimate value is just `1 - CI_level`. The values indicating the lower
#' and upper limits of the intervals are the related estimates associated with
#' the _p_-value. E.g., if a parameter `x` has a 75% compatibility interval
#' of `(0.81, 1.05)`, then the _p_-value for the estimate value of `0.81`
#' would be `1 - 0.75`, which is `0.25`. This relationship is more intuitive and
#' better to understand when looking at the plots (using `plot()`).
#'
#' ## Conditional versus unconditional interpretation of _p_-values and intervals
#'
#' `p_function()`, and in particular its `plot()` method, aims at re-interpreting
#' _p_-values and confidence intervals (better named: _compatibility_ intervals)
#' in _unconditional_ terms. Instead of referring to the long-term property and
#' repeated trials when interpreting interval estimates (so-called "aleatory
#' probability", _Schweder 2018_), and assuming that all underlying assumptions
#' are correct and met, `p_function()` interprets _p_-values in a Fisherian way
#' as "_continuous_ measure of evidence against the very test hypothesis _and_
#' entire model (all assumptions) used to compute it"
#' (*P-Values Are Tough and S-Values Can Help*, lesslikely.com/statistics/s-values;
#' see also _Amrhein and Greenland 2022_).
#'
#' This interpretation as a continuous measure of evidence against the test
#' hypothesis and the entire model used to compute it can be seen in the
#' figure below (taken from *P-Values Are Tough and S-Values Can Help*,
#' lesslikely.com/statistics/s-values). The "conditional" interpretation of
#' _p_-values and interval estimates (A) implicitly assumes certain assumptions
#' to be true, thus the interpretation is "conditioned" on these assumptions
#' (i.e. assumptions are taken as given). The unconditional interpretation (B),
#' however, questions all these assumptions.
#'
#' \if{html}{\cr \figure{unconditional_interpretation.png}{options: alt="Conditional versus unconditional interpretations of P-values"} \cr}
#'
#' "Emphasizing unconditional interpretations helps avoid overconfident and
#' misleading inferences in light of uncertainties about the assumptions used
#' to arrive at the statistical results." (_Greenland et al. 2022_).
#'
#' **Note:** The term "conditional" as used by Rafi and Greenland probably has
#' a slightly different meaning than normally. "Conditional" in this notion
#' means that all model assumptions are taken as given - it should not be
#' confused with terms like "conditional probability". See also _Greenland et al. 2022_
#' for a detailed elaboration on this issue.
#'
#' In other words, the term compatibility interval emphasizes "the dependence
#' of the _p_-value on the assumptions as well as on the data, recognizing that
#' _p_<0.05 can arise from assumption violations even if the effect under
#' study is null" (_Gelman/Greenland 2019_).
#'
#' ## Probabilistic interpretation of p-values and compatibility intervals
#'
#' Schweder (2018) resp. Schweder and Hjort (2016) (and others) argue that
#' confidence curves (as produced by `p_function()`) have a valid probabilistic
#' interpretation. They distinguish between _aleatory probability_, which
#' describes the aleatory stochastic element of a distribution _ex ante_, i.e.
#' before the data are obtained. This is the classical interpretation of
#' confidence intervals following the Neyman-Pearson school of statistics.
#' However, there is also an _ex post_ probability, called _epistemic_ probability,
#' for confidence curves. The shift in terminology from _confidence_ intervals
#' to _compatibility_ intervals may help emphasizing this interpretation.
#'
#' In this sense, the probabilistic interpretation of _p_-values and
#' compatibility intervals is "conditional" - on the data _and_ model assumptions
#' (which is in line with the "unconditional" interpretation in the sense of
#' Rafi and Greenland).
#'
#' Ascribing a probabilistic interpretation to one realized confidence interval
#' is possible without repeated sampling of the specific experiment. Important
#' is the assumption that a _sampling distribution_ is a good description of the
#' variability of the parameter (_Vos and Holbert 2022_). At the core, the
#' interpretation of a confidence interval is "I assume that this sampling
#' distribution is a good description of the uncertainty of the parameter. If
#' that's a good assumption, then the values in this interval are the most
#' plausible or compatible with the data". The source of confidence in
#' probability statements is the assumption that the selected sampling
#' distribution is appropriate.
#'
#' "The realized confidence distribution is clearly an epistemic probability
#' distribution" (_Schweder 2018_). In Bayesian words, compatibility intervals
#' (or confidence distributons, or consonance curves) are "posteriors without
#' priors" (_Schweder, Hjort, 2003_).
#'
#' The _p_-value indicates the degree of compatibility of the endpoints of the
#' interval at a given confidence level with (1) the observed data and (2) model
#' assumptions. The observed point estimate (_p_-value = 1) is the value
#' estimated to be _most compatible_ with the data and model assumptions,
#' whereas values values far from the observed point estimate (where _p_
#' approaches 0) are least compatible with the data and model assumptions
#' (_Schweder and Hjort 2016, pp. 60-61; Amrhein and Greenland 2022_). In this
#' regards, _p_-values are statements about _confidence_ or _compatibility_:
#' The p-value is not an absolute measure of evidence for a model (such as the
#' null/alternative model), it is a continuous measure of the compatibility of
#' the observed data with the model used to compute it (_Greenland et al. 2016_,
#' _Greenland 2023_). Going one step further, and following _Schweder_, p-values
#' can be considered as _epistemic probability_ - "not necessarily of the
#' hypothesis being true, but of it _possibly_ being true" (_Schweder 2018_).
#' Hence, the interpretation of _p_-values might be guided using
#' [`bayestestR::p_to_pd()`].
#'
#' ## Probability or compatibility?
#'
#' We here presented the discussion of p-values and confidence intervals from the
#' perspective of two paradigms, one saying that probability statements can be
#' made, one saying that interpretation is guided in terms of "compatibility".
#' Cox and Hinkley say, "interval estimates cannot be taken as probability
#' statements" (_Cox and Hinkley 1979: 208_), which conflicts with the Schweder
#' and Hjort confidence distribution school. However, if you view interval
#' estimates as being intervals of values being consistent with the data,
#' this comes close to the idea of (epistemic) probability. We do not believe that
#' these two paradigms contradict or exclude each other. Rather, the aim is to
#' emphasize one point of view or the other, i.e. to place the linguistic
#' nuances either on 'compatibility' or 'probability'.
#'
#' The main take-away is *not* to interpret p-values as dichotomous decisions
#' that distinguish between "we found an effect" (statistically significant)" vs.
#' "we found no effect" (statistically not significant) (_Altman and Bland 1995_).
#'
#' ## Compatibility intervals - is their interpretation "conditional" or not?
#'
#' The fact that the term "conditional" is used in different meanings in
#' statistics, is confusing and unfortunate. Thus, we would summarize the
#' (probabilistic) interpretation of compatibility intervals as follows: The
#' intervals are built from the data _and_ our modeling assumptions. The
#' accuracy of the intervals depends on our model assumptions. If a value is
#' outside the interval, that might be because (1) that parameter value isn't
#' supported by the data, or (2) the modeling assumptions are a poor fit for the
#' situation. When we make bad assumptions, the compatibility interval might be
#' too wide or (more commonly and seriously) too narrow, making us think we know
#' more about the parameter than is warranted.
#'
#' When we say "there is a 95% chance the true value is in the interval", that is
#' a statement of _epistemic probability_ (i.e. description of uncertainty related
#' to our knowledge or belief). When we talk about repeated samples or sampling
#' distributions, that is referring to _aleatoric_ (physical properties) probability.
#' Frequentist inference is built on defining estimators with known _aleatoric_
#' probability properties, from which we can draw _epistemic_ probabilistic
#' statements of uncertainty (_Schweder and Hjort 2016_).
#'
#' ## Functions in the parameters package to check for effect existence and significance
#'
#' The **parameters** package provides several options or functions to aid
#' statistical inference. Beyond `p_function()`, there are, for example:
#' - [`equivalence_test()`][equivalence_test.lm], to compute the (conditional)
#'   equivalence test for frequentist models
#' - [`p_significance()`][p_significance.lm], to compute the probability of
#'   *practical significance*, which can be conceptualized as a unidirectional
#'   equivalence test
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
#' @return A data frame with p-values and compatibility intervals.
#'
#' @references
#' - Altman DG, Bland JM. Absence of evidence is not evidence of absence. BMJ.
#'   1995;311(7003):485. \doi{10.1136/bmj.311.7003.485}
#'
#' - Amrhein V, Greenland S. Discuss practical importance of results based on
#'   interval estimates and p-value functions, not only on point estimates and
#'   null p-values. Journal of Information Technology 2022;37:316–20.
#'   \doi{10.1177/02683962221105904}
#'
#' - Cox DR, Hinkley DV. 1979. Theoretical Statistics. 6th edition.
#'   Chapman and Hall/CRC
#'
#' - Fraser DAS. The P-value function and statistical inference. The American
#'   Statistician. 2019;73(sup1):135-147. \doi{10.1080/00031305.2018.1556735}
#'
#' - Gelman A, Greenland S. Are confidence intervals better termed "uncertainty
#'   intervals"? BMJ (2019)l5381. \doi{10.1136/bmj.l5381}
#'
#' - Greenland S, Rafi Z, Matthews R, Higgs M. To Aid Scientific Inference,
#'   Emphasize Unconditional Compatibility Descriptions of Statistics. (2022)
#'   https://arxiv.org/abs/1909.08583v7 (Accessed November 10, 2022)
#'
#' - Greenland S, Senn SJ, Rothman KJ, Carlin JB, Poole C, Goodman SN, et al.
#'   (2016). Statistical tests, P values, confidence intervals, and power: A
#'   guide to misinterpretations. European Journal of Epidemiology. 31:337-350.
#'   \doi{10.1007/s10654-016-0149-3}
#'
#' - Greenland S (2023). Divergence versus decision P-values: A distinction
#'   worth making in theory and keeping in practice: Or, how divergence P-values
#'   measure evidence even when decision P-values do not. Scand J Statist, 50(1),
#'   54-88.
#'
#' - Rafi Z, Greenland S. Semantic and cognitive tools to aid statistical
#'   science: Replace confidence and significance by compatibility and surprise.
#'   BMC Medical Research Methodology. 2020;20(1):244. \doi{10.1186/s12874-020-01105-9}
#'
#' - Schweder T. Confidence is epistemic probability for empirical science.
#'   Journal of Statistical Planning and Inference (2018) 195:116–125.
#'   \doi{10.1016/j.jspi.2017.09.016}
#'
#' - Schweder T, Hjort NL. Confidence and Likelihood. Scandinavian Journal of
#'   Statistics. 2002;29(2):309-332. \doi{10.1111/1467-9469.00285}
#'
#' - Schweder T, Hjort NL. Frequentist analogues of priors and posteriors.
#'   In Stigum, B. (ed.), Econometrics and the Philosophy of Economics: Theory
#'   Data Confrontation in Economics, pp. 285-217. Princeton University Press,
#'   Princeton, NJ, 2003
#'
#' - Schweder T, Hjort NL. Confidence, Likelihood, Probability: Statistical
#'   inference with confidence distributions. Cambridge University Press, 2016.
#'
#' - Vos P, Holbert D. Frequentist statistical inference without repeated sampling.
#'   Synthese 200, 89 (2022). \doi{10.1007/s11229-022-03560-x}
#'
#' @examplesIf requireNamespace("see")
#' model <- lm(Sepal.Length ~ Species, data = iris)
#' p_function(model)
#'
#' model <- lm(mpg ~ wt + as.factor(gear) + am, data = mtcars)
#' result <- p_function(model)
#'
#' # single panels
#' plot(result, n_columns = 2)
#'
#' # integrated plot, the default
#' plot(result)
#' @export
p_function <- function(model,
                       ci_levels = c(0.25, 0.5, 0.75, emph = 0.95),
                       exponentiate = FALSE,
                       effects = "fixed",
                       component = "all",
                       vcov = NULL,
                       vcov_args = NULL,
                       keep = NULL,
                       drop = NULL,
                       verbose = TRUE,
                       ...) {
  # degrees of freedom
  dof <- insight::get_df(model, type = "wald")

  # standard errors
  se <- standard_error(
    model,
    effects = effects,
    component = component,
    vcov = vcov,
    vcov_args = vcov_args
  )$SE

  if (is.null(dof) || length(dof) == 0 || .is_chi2_model(model, dof)) {
    dof <- Inf
  }

  x <- do.call(rbind, lapply(seq(0, 1, 0.01), function(i) {
    suppressMessages(.ci_dof(
      model,
      ci = i,
      dof,
      effects,
      component,
      method = "wald",
      se = se,
      vcov = NULL,
      vcov_args = NULL,
      verbose = TRUE
    ))
  }))

  # data for plotting
  out <- x[!is.infinite(x$CI_low) & !is.infinite(x$CI_high), ]
  out$CI <- round(out$CI, 2)

  # most plausible value (point estimate)
  point_estimate <- out$CI_low[which.min(out$CI)]

  if (!is.null(keep) || !is.null(drop)) {
    out <- .filter_parameters(out,
      keep = keep,
      drop = drop,
      verbose = verbose
    )
  }

  # transform non-Gaussian
  if (isTRUE(exponentiate)) {
    out$CI_low <- exp(out$CI_low)
    out$CI_high <- exp(out$CI_high)
  }

  # data for p_function ribbon
  data_ribbon <- datawizard::data_to_long(
    out,
    select = c("CI_low", "CI_high"),
    values_to = "x"
  )

  # data for vertical CI level lines
  out <- out[out$CI %in% ci_levels, ]
  out$group <- 1

  # emphasize focal hypothesis line
  emphasize <- which(names(ci_levels) == "emph")
  if (length(emphasize)) {
    out$group[out$CI == ci_levels[emphasize]] <- 2
  }

  attr(out, "data") <- data_ribbon
  attr(out, "point_estimate") <- point_estimate
  attr(out, "pretty_names") <- suppressWarnings(format_parameters(model, ...))

  class(out) <- c("parameters_p_function", "see_p_function", "data.frame")
  out
}

#' @rdname p_function
#' @export
consonance_function <- p_function

#' @rdname p_function
#' @export
confidence_curve <- p_function


# methods ----------------------


#' @export
plot.parameters_p_function <- function(x, ...) {
  insight::check_if_installed("see")
  NextMethod()
}


#' @export
format.parameters_p_function <- function(x,
                                         digits = 2,
                                         format = NULL,
                                         ci_width = NULL,
                                         ci_brackets = TRUE,
                                         pretty_names = TRUE,
                                         ...) {
  # print
  dat <- lapply(split(x, x$CI), function(i) {
    ci <- as.character(i$CI)[1]
    out <- datawizard::data_rename(
      i,
      select = c("CI_low", "CI_high"),
      replacement = c(sprintf("CI_low_%s", ci), sprintf("CI_high_%s", ci))
    )
    out$CI <- NULL
    out$group <- NULL
    out
  })

  out <- do.call(datawizard::data_merge, list(dat, by = "Parameter"))
  attr(out, "pretty_names") <- attributes(x)$pretty_names

  insight::format_table(
    out,
    digits = digits,
    ci_width = ci_width,
    ci_brackets = ci_brackets,
    format = format,
    pretty_names = pretty_names
  )
}


#' @export
print.parameters_p_function <- function(x,
                                        digits = 2,
                                        ci_width = "auto",
                                        ci_brackets = TRUE,
                                        pretty_names = TRUE,
                                        ...) {
  cat(.print_p_function(
    x,
    digits,
    ci_width,
    ci_brackets,
    pretty_names = pretty_names,
    format = "text",
    ...
  ))
}


#' @export
print_md.parameters_p_function <- function(x,
                                           digits = 2,
                                           ci_width = "auto",
                                           ci_brackets = c("(", ")"),
                                           pretty_names = TRUE,
                                           ...) {
  .print_p_function(x, digits, ci_width, ci_brackets, pretty_names, format = "markdown", ...)
}


#' @export
print_html.parameters_p_function <- function(x,
                                             digits = 2,
                                             ci_width = "auto",
                                             ci_brackets = c("(", ")"),
                                             pretty_names = TRUE,
                                             ...) {
  .print_p_function(x, digits, ci_width, ci_brackets, pretty_names, format = "html", ...)
}



# helper ----------

.print_p_function <- function(x,
                              digits = 2,
                              ci_width = "auto",
                              ci_brackets = c("(", ")"),
                              pretty_names = TRUE,
                              format = "html",
                              ...) {
  formatted_table <- format(
    x,
    digits = digits,
    format = format,
    ci_width = ci_width,
    ci_brackets = ci_brackets,
    pretty_names = pretty_names,
    ...
  )

  insight::export_table(
    formatted_table,
    format = format,
    caption = "Consonance Function",
    ...
  )
}

# model <- lm(Sepal.Length ~ Species, data = iris)
# for later use: highlight p-value for secific parameter estimate values
# stat <- insight::get_statistic(model)
# se <- parameters::standard_error(model)
# estimate to test against - compute p-value for specific estimate
# null_estimate <- 1.5
# p <- 2 * stats::pt(abs(stat$Statistic[3]) - (null_estimate / se$SE[3]), df = 147, lower.tail = FALSE)
# bayestestR::p_to_pd(p)

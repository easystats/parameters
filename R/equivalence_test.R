#' @importFrom bayestestR equivalence_test
#' @export
bayestestR::equivalence_test


#' @title Equivalence test
#'
#' @description Compute the (conditional) equivalence test for frequentist models.
#'
#' @param x A statistical model.
#' @param range The range of practical equivalence of an effect. May be
#' `"default"`, to automatically define this range based on properties of the
#' model's data.
#' @param ci Confidence Interval (CI) level. Default to `0.95` (`95%`).
#' @param rule Character, indicating the rules when testing for practical
#' equivalence. Can be `"bayes"`, `"classic"` or `"cet"`. See 'Details'.
#' @param verbose Toggle warnings and messages.
#' @param ... Arguments passed to or from other methods.
#' @inheritParams model_parameters.glmmTMB
#' @inheritParams p_value
#'
#' @seealso For more details, see [bayestestR::equivalence_test()]. Further
#' readings can be found in the references. See also [`p_significance()`] for
#' a unidirectional equivalence test.
#'
#' @details In classical null hypothesis significance testing (NHST) within a
#' frequentist framework, it is not possible to accept the null hypothesis, H0 -
#' unlike in Bayesian statistics, where such probability statements are
#' possible. "[...] one can only reject the null hypothesis if the test
#' statistics falls into the critical region(s), or fail to reject this
#' hypothesis. In the latter case, all we can say is that no significant effect
#' was observed, but one cannot conclude that the null hypothesis is true."
#' (_Pernet 2017_). One way to address this issues without Bayesian methods is
#' *Equivalence Testing*, as implemented in `equivalence_test()`. While you
#' either can reject the null hypothesis or claim an inconclusive result in
#' NHST, the equivalence test - according to _Pernet_ - adds a third category,
#' *"accept"*. Roughly speaking, the idea behind equivalence testing in a
#' frequentist framework is to check whether an estimate and its uncertainty
#' (i.e. confidence interval) falls within a region of "practical equivalence".
#' Depending on the rule for this test (see below), statistical significance
#' does not necessarily indicate whether the null hypothesis can be rejected or
#' not, i.e. the classical interpretation of the p-value may differ from the
#' results returned from the equivalence test.
#'
#' ## Calculation of equivalence testing
#' - "bayes" - Bayesian rule (Kruschke 2018)
#'
#'   This rule follows the "HDI+ROPE decision rule" (_Kruschke, 2014, 2018_) used
#'   for the [`Bayesian counterpart()`][bayestestR::equivalence_test]. This
#'   means, if the confidence intervals are completely outside the ROPE, the
#'   "null hypothesis" for this parameter is "rejected". If the ROPE
#'   completely covers the CI, the null hypothesis is accepted. Else, it's
#'   undecided whether to accept or reject the null hypothesis. Desirable
#'   results are low proportions inside the ROPE (the closer to zero the
#'   better).
#'
#' - "classic" - The TOST rule (Lakens 2017)
#'
#'   This rule follows the "TOST rule", i.e. a two one-sided test procedure
#'   (_Lakens 2017_). Following this rule...
#'   - practical equivalence is assumed (i.e. H0 *"accepted"*) when the narrow
#'     confidence intervals are completely inside the ROPE, no matter if the
#'     effect is statistically significant or not;
#'   - practical equivalence (i.e. H0) is *rejected*, when the coefficient is
#'     statistically significant, both when the narrow confidence intervals
#'     (i.e. `1-2*alpha`) include or exclude the the ROPE boundaries, but the
#'     narrow confidence intervals are *not fully covered* by the ROPE;
#'   - else the decision whether to accept or reject practical equivalence is
#'     undecided (i.e. when effects are *not* statistically significant *and*
#'     the narrow confidence intervals overlaps the ROPE).
#'
#' - "cet" - Conditional Equivalence Testing (Campbell/Gustafson 2018)
#'
#'   The Conditional Equivalence Testing as described by _Campbell and
#'   Gustafson 2018_. According to this rule, practical equivalence is
#'   rejected when the coefficient is statistically significant. When the
#'   effect is *not* significant and the narrow confidence intervals are
#'   completely inside the ROPE, we accept (i.e. assume) practical equivalence,
#'   else it is undecided.
#'
#' ## Levels of Confidence Intervals used for Equivalence Testing
#' For `rule = "classic"`, "narrow" confidence intervals are used for
#' equivalence testing. "Narrow" means, the the intervals is not 1 - alpha,
#' but 1 - 2 * alpha. Thus, if `ci = .95`, alpha is assumed to be 0.05
#' and internally a ci-level of 0.90 is used. `rule = "cet"` uses
#' both regular and narrow confidence intervals, while `rule = "bayes"`
#' only uses the regular intervals.
#'
#' ## p-Values
#' The equivalence p-value is the area of the (cumulative) confidence
#' distribution that is outside of the region of equivalence. It can be
#' interpreted as p-value for *rejecting* the alternative hypothesis and
#' *accepting* the "null hypothesis" (i.e. assuming practical equivalence). That
#' is, a high p-value means we reject the assumption of practical equivalence
#' and accept the alternative hypothesis.
#'
#' ## Second Generation p-Value (SGPV)
#' Second generation p-values (SGPV) were proposed as a statistic that
#' represents _the proportion of data-supported hypotheses that are also null
#' hypotheses_ _(Blume et al. 2018, Lakens and Delacre 2020)_. It represents the
#' proportion of the _full_ confidence interval range (assuming a normally or
#' t-distributed, equal-tailed interval, based on the model) that is inside the
#' ROPE. The SGPV ranges from zero to one. Higher values indicate that the
#' effect is more likely to be practically equivalent ("not of interest").
#'
#' Note that the assumed interval, which is used to calculate the SGPV, is an
#' estimation of the _full interval_ based on the chosen confidence level. For
#' example, if the 95% confidence interval of a coefficient ranges from -1 to 1,
#' the underlying _full (normally or t-distributed) interval_ approximately
#' ranges from -1.9 to 1.9, see also following code:
#'
#' ```
#' # simulate full normal distribution
#' out <- bayestestR::distribution_normal(10000, 0, 0.5)
#' # range of "full" distribution
#' range(out)
#' # range of 95% CI
#' round(quantile(out, probs = c(0.025, 0.975)), 2)
#' ```
#'
#' This ensures that the SGPV always refers to the general compatible parameter
#' space of coefficients, independent from the confidence interval chosen for
#' testing practical equivalence. Therefore, the SGPV of the _full interval_ is
#' similar to the ROPE coverage of Bayesian equivalence tests, see following
#' code:
#'
#' ```
#' library(bayestestR)
#' library(brms)
#' m <- lm(mpg ~ gear + wt + cyl + hp, data = mtcars)
#' m2 <- brm(mpg ~ gear + wt + cyl + hp, data = mtcars)
#' # SGPV for frequentist models
#' equivalence_test(m)
#' # similar to ROPE coverage of Bayesian models
#' equivalence_test(m2)
#' # similar to ROPE coverage of simulated draws / bootstrap samples
#' equivalence_test(simulate_model(m))
#' ```
#'
#' ## ROPE range
#' Some attention is required for finding suitable values for the ROPE limits
#' (argument `range`). See 'Details' in [bayestestR::rope_range()]
#' for further information.
#'
#' @inheritSection model_parameters Statistical inference - how to quantify evidence
#'
#' @note There is also a [`plot()`-method](https://easystats.github.io/see/articles/parameters.html)
#' implemented in the [**see**-package](https://easystats.github.io/see/).
#'
#' @references
#'
#'   - Amrhein, V., Korner-Nievergelt, F., and Roth, T. (2017). The earth is
#'     flat (p > 0.05): Significance thresholds and the crisis of unreplicable
#'     research. PeerJ, 5, e3544. \doi{10.7717/peerj.3544}
#'
#'   - Blume, J. D., D'Agostino McGowan, L., Dupont, W. D., & Greevy, R. A.
#'     (2018). Second-generation p-values: Improved rigor, reproducibility, &
#'     transparency in statistical analyses. PLOS ONE, 13(3), e0188299.
#'     https://doi.org/10.1371/journal.pone.0188299
#'
#'   - Campbell, H., & Gustafson, P. (2018). Conditional equivalence
#'     testing: An alternative remedy for publication bias. PLOS ONE, 13(4),
#'     e0195145. doi: 10.1371/journal.pone.0195145
#'
#'   - Greenland S, Rafi Z, Matthews R, Higgs M. To Aid Scientific Inference,
#'     Emphasize Unconditional Compatibility Descriptions of Statistics. (2022)
#'     https://arxiv.org/abs/1909.08583v7 (Accessed November 10, 2022)
#'
#'   - Kruschke, J. K. (2014). Doing Bayesian data analysis: A tutorial with
#'     R, JAGS, and Stan. Academic Press
#'
#'   - Kruschke, J. K. (2018). Rejecting or accepting parameter values in
#'     Bayesian estimation. Advances in Methods and Practices in Psychological
#'     Science, 1(2), 270-280. doi: 10.1177/2515245918771304
#'
#'   - Lakens, D. (2017). Equivalence Tests: A Practical Primer for t Tests,
#'     Correlations, and Meta-Analyses. Social Psychological and Personality
#'     Science, 8(4), 355–362. doi: 10.1177/1948550617697177
#'
#'   - Lakens, D. (2024). Improving Your Statistical Inferences (Version v1.5.1).
#'     Retrieved from https://lakens.github.io/statistical_inferences/.
#'     \doi{10.5281/ZENODO.6409077}
#'
#'   - Lakens, D., and Delacre, M. (2020). Equivalence Testing and the Second
#'     Generation P-Value. Meta-Psychology, 4.
#'     https://doi.org/10.15626/MP.2018.933
#'
#'   - Lakens, D., Scheel, A. M., and Isager, P. M. (2018). Equivalence Testing
#'     for Psychological Research: A Tutorial. Advances in Methods and Practices
#'     in Psychological Science, 1(2), 259–269.
#'
#'   - Makowski, D., Ben-Shachar, M. S., Chen, S. H. A., and Lüdecke, D. (2019).
#'     Indices of Effect Existence and Significance in the Bayesian Framework.
#'     Frontiers in Psychology, 10, 2767. \doi{10.3389/fpsyg.2019.02767}
#'
#'   - Pernet, C. (2017). Null hypothesis significance testing: A guide to
#'     commonly misunderstood concepts and recommendations for good practice.
#'     F1000Research, 4, 621. doi: 10.12688/f1000research.6963.5
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
#' @return A data frame.
#' @examplesIf requireNamespace("sandwich")
#' data(qol_cancer)
#' model <- lm(QoL ~ time + age + education, data = qol_cancer)
#'
#' # default rule
#' equivalence_test(model)
#'
#' # using heteroscedasticity-robust standard errors
#' equivalence_test(model, vcov = "HC3")
#'
#' # conditional equivalence test
#' equivalence_test(model, rule = "cet")
#'
#' # plot method
#' if (require("see", quietly = TRUE)) {
#'   result <- equivalence_test(model)
#'   plot(result)
#' }
#' @export
equivalence_test.lm <- function(x,
                                range = "default",
                                ci = 0.95,
                                rule = "classic",
                                effects = "fixed",
                                vcov = NULL,
                                vcov_args = NULL,
                                verbose = TRUE,
                                ...) {
  rule <- insight::validate_argument(tolower(rule), c("bayes", "classic", "cet"))
  out <- .equivalence_test_frequentist(
    x,
    range = range,
    ci = ci,
    rule = rule,
    vcov = vcov,
    vcov_args = vcov_args,
    verbose,
    ...
  )

  if (is.null(attr(out, "pretty_names", exact = TRUE))) {
    attr(out, "pretty_names") <- format_parameters(x)
  }
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  attr(out, "rule") <- rule
  class(out) <- c("equivalence_test_lm", "see_equivalence_test_lm", class(out))
  out
}


# standard models, only fixed effects ----------------------

#' @export
equivalence_test.glm <- equivalence_test.lm

#' @export
equivalence_test.wbm <- equivalence_test.lm

#' @export
equivalence_test.lme <- equivalence_test.lm

#' @export
equivalence_test.gee <- equivalence_test.lm

#' @export
equivalence_test.gls <- equivalence_test.lm

#' @export
equivalence_test.feis <- equivalence_test.lm

#' @export
equivalence_test.felm <- equivalence_test.lm

#' @export
equivalence_test.mixed <- equivalence_test.lm

#' @export
equivalence_test.hurdle <- equivalence_test.lm

#' @export
equivalence_test.zeroinfl <- equivalence_test.lm

#' @export
equivalence_test.rma <- equivalence_test.lm


# mixed models, also random effects ----------------------

#' @export
equivalence_test.merMod <- function(x,
                                    range = "default",
                                    ci = 0.95,
                                    rule = "classic",
                                    effects = "fixed",
                                    vcov = NULL,
                                    vcov_args = NULL,
                                    verbose = TRUE,
                                    ...) {
  # ==== argument matching ====

  rule <- insight::validate_argument(tolower(rule), c("bayes", "classic", "cet"))
  effects <- insight::validate_argument(effects, c("fixed", "random"))


  # ==== equivalent testing for fixed or random effects ====

  if (effects == "fixed") {
    out <- .equivalence_test_frequentist(
      x,
      range = range,
      ci = ci,
      rule = rule,
      vcov = vcov,
      vcov_args = vcov_args,
      verbose,
      ...
    )
  } else {
    out <- .equivalence_test_frequentist_random(x, range, ci, rule, verbose, ...)
  }


  # ==== result ====

  if (is.null(attr(out, "pretty_names", exact = TRUE))) {
    attr(out, "pretty_names") <- format_parameters(x)
  }
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(x))
  attr(out, "rule") <- rule
  class(out) <- c("equivalence_test_lm", "see_equivalence_test_lm", class(out))
  out
}


#' @export
equivalence_test.glmmTMB <- equivalence_test.merMod

#' @export
equivalence_test.MixMod <- equivalence_test.merMod


# Special classes -------------------------

#' @export
equivalence_test.parameters_simulate_model <- function(x,
                                                       range = "default",
                                                       ci = 0.95,
                                                       verbose = TRUE,
                                                       ...) {
  # ==== retrieve model, to define rope range for simulated model parameters ====

  model <- .get_object(x)

  if (all(range == "default") && !is.null(model)) {
    range <- bayestestR::rope_range(model, verbose = verbose)
  } else if (!all(is.numeric(range)) || length(range) != 2) {
    insight::format_error(
      "`range` should be \"default\" or a vector of 2 numeric values (e.g., `c(-0.1, 0.1)`)."
    )
  }


  # ==== classical equivalent testing for data frames ====

  out <- equivalence_test(as.data.frame(x), range = range, ci = ci, verbose = verbose, ...)

  if (is.null(attr(out, "pretty_names", exact = TRUE))) {
    attr(out, "pretty_names") <- format_parameters(x)
  }
  attr(out, "object_name") <- attr(x, "object_name")
  attr(out, "data") <- x
  class(out) <- unique(c("equivalence_test", "see_equivalence_test", "equivalence_test_simulate_model", class(out)))
  out
}


#' @export
equivalence_test.parameters_model <- function(x,
                                              range = "default",
                                              ci = 0.95,
                                              rule = "classic",
                                              verbose = TRUE,
                                              ...) {
  model <- .get_object(x)
  equivalence_test(x = model, range = range, ci = ci, rule = rule, verbose = verbose, ...)
}


# helper -------------------


#' @keywords internal
.check_rope_range <- function(x, range, verbose) {
  if (all(range == "default")) {
    range <- bayestestR::rope_range(x, verbose = verbose)
    if (is.list(range)) {
      range <- range[[which.max(sapply(range, diff))]]
    }
  } else if (!all(is.numeric(range)) || length(range) != 2) {
    insight::format_error(
      "`range` should be \"default\" or a vector of 2 numeric values (e.g., `c(-0.1, 0.1)`)."
    )
  }
  range
}


#' @keywords internal
.equivalence_test_frequentist <- function(x,
                                          range = "default",
                                          ci = 0.95,
                                          rule = "classic",
                                          vcov = NULL,
                                          vcov_args = NULL,
                                          verbose = TRUE,
                                          ...) {
  # ==== define rope range ====

  range <- .check_rope_range(x, range, verbose)

  if (length(ci) > 1) {
    insight::format_alert("`ci` may only be of length 1. Using first ci-value now.")
    ci <- ci[1]
  }


  # ==== check degrees of freedom ====

  df_column <- grep("(df|df_error)", colnames(x))
  if (length(df_column) > 0) {
    dof <- unique(x[[df_column]])
    if (length(dof) > 1) {
      dof <- Inf
    }
  } else {
    dof <- Inf
  }

  # ==== requested confidence intervals ====

  params <- conf_int <- .ci_generic(x, ci = ci, vcov = vcov, vcov_args = vcov_args, ...)
  conf_int <- as.data.frame(t(conf_int[, c("CI_low", "CI_high")]))


  # ==== the "narrower" intervals (1-2*alpha) for CET-rules. ====

  alpha <- 1 - ci
  conf_int2 <- .ci_generic(x, ci = (ci - alpha), vcov = vcov, vcov_args = vcov_args, ...)
  conf_int2 <- as.data.frame(t(conf_int2[, c("CI_low", "CI_high")]))


  # ==== equivalence test for each parameter ====

  l <- Map(
    function(ci_wide, ci_narrow) {
      .equivalence_test_numeric(
        ci = ci,
        ci_wide,
        ci_narrow,
        range_rope = range,
        rule = rule,
        dof = dof,
        verbose = verbose
      )
    }, conf_int, conf_int2
  )

  dat <- do.call(rbind, l)
  if ("Component" %in% colnames(params)) dat$Component <- params$Component

  out <- data.frame(
    Parameter = params$Parameter,
    CI = ifelse(rule == "bayes", ci, ci - alpha),
    dat,
    stringsAsFactors = FALSE
  )


  # ==== (adjusted) p-values for tests ====

  out$p <- .add_p_to_equitest(x, ci, range, vcov = vcov, vcov_args = vcov_args, ...)

  attr(out, "rope") <- range
  out
}


#' @keywords internal
.equivalence_test_frequentist_random <- function(x,
                                                 range = "default",
                                                 ci = 0.95,
                                                 rule = "classic",
                                                 verbose = TRUE,
                                                 ...) {
  if (all(range == "default")) {
    range <- bayestestR::rope_range(x, verbose = verbose)
  } else if (!all(is.numeric(range)) || length(range) != 2) {
    insight::format_error(
      "`range` should be \"default\" or a vector of 2 numeric values (e.g., `c(-0.1, 0.1)`)."
    )
  }

  if (length(ci) > 1) {
    if (isTRUE(verbose)) {
      insight::format_alert("`ci` may only be of length 1. Using first ci-value now.")
    }
    ci <- ci[1]
  }

  params <- insight::get_parameters(x, effects = "random", component = "conditional", verbose = FALSE)
  se <- standard_error(x, effects = "random", component = "conditional")

  alpha <- (1 + ci) / 2
  fac <- stats::qnorm(alpha)

  alpha_narrow <- (1 + ci - (1 - ci)) / 2
  fac_narrow <- stats::qnorm(alpha_narrow)


  out <- do.call(rbind, lapply(names(params), function(np) {
    est <- params[[np]][, "(Intercept)"]
    std_err <- se[[np]][, "(Intercept)"]

    d <- data.frame(
      Parameter = rownames(params[[np]]),
      Estimate = est,
      CI = ifelse(rule == "bayes", ci, ci - (1 - ci)),
      Group = np,
      stringsAsFactors = FALSE
    )

    conf_int <- as.data.frame(t(data.frame(
      CI_low = est - std_err * fac,
      CI_high = est + std_err * fac
    )))

    conf_int2 <- as.data.frame(t(data.frame(
      CI_low = est - std_err * fac_narrow,
      CI_high = est + std_err * fac_narrow
    )))

    l <- Map(
      function(ci_wide, ci_narrow) {
        .equivalence_test_numeric(
          ci = ci,
          ci_wide,
          ci_narrow,
          range_rope = range,
          rule = rule,
          verbose = verbose
        )
      }, conf_int, conf_int2
    )

    dat <- do.call(rbind, l)
    cbind(d, dat)
  }))

  attr(out, "rope") <- range
  out
}


#' @keywords internal
.equivalence_test_numeric <- function(ci = 0.95,
                                      ci_wide,
                                      ci_narrow,
                                      range_rope,
                                      rule,
                                      dof = Inf,
                                      verbose) {
  final_ci <- NULL

  # ==== HDI+ROPE decision rule, by Kruschke ====

  if (rule == "bayes") {
    final_ci <- ci_wide
    if (min(ci_wide) > max(range_rope) || max(ci_wide) < min(range_rope)) {
      decision <- "Rejected"
    } else if (max(ci_wide) <= max(range_rope) && min(ci_wide) >= min(range_rope)) {
      decision <- "Accepted"
    } else {
      decision <- "Undecided"
    }
  }


  # ==== Lakens' rule ====

  if (rule == "classic") {
    final_ci <- ci_narrow
    if (all(ci_narrow < max(range_rope)) && all(ci_narrow > min(range_rope))) {
      # narrow CI is fully inside ROPE - always accept
      decision <- "Accepted"
    } else if (min(ci_narrow) < 0 && max(ci_narrow) > 0) {
      # non-significant results - undecided
      decision <- "Undecided"
    } else {
      decision <- "Rejected"
    }
  }


  # ==== CET rule ====

  if (rule == "cet") {
    final_ci <- ci_narrow
    # significant result?
    if (min(ci_wide) > 0 || max(ci_wide) < 0) {
      decision <- "Rejected"
      # non-significant results, all narrow CI inside ROPE
    } else if (all(ci_narrow < max(range_rope)) && all(ci_narrow > min(range_rope))) {
      decision <- "Accepted"
    } else {
      decision <- "Undecided"
    }
  }

  data.frame(
    CI_low = final_ci[1],
    CI_high = final_ci[2],
    SGPV = .rope_coverage(ci = ci, range_rope, ci_range = final_ci, dof = dof),
    ROPE_low = range_rope[1],
    ROPE_high = range_rope[2],
    ROPE_Equivalence = decision,
    stringsAsFactors = FALSE
  )
}


# helper ---------------------

# this function simply takes the length of the range and calculates the proportion
# of that range that is inside the rope. However, this assumed a "flat", i.e.
# uniformly distributed interval, which is not accurate for standard confidence
# intervals. thus, we no longer use this function, but switch to ".rope_coverage()".
.sgpv <- function(range_rope, ci) {
  diff_rope <- abs(diff(range_rope))
  diff_ci <- abs(diff(ci))

  # inside?
  if (min(ci) >= min(range_rope) && max(ci) <= max(range_rope)) {
    coverage <- 1

    # outside?
  } else if (max(ci) < min(range_rope) || min(ci) > max(range_rope)) {
    coverage <- 0

    # CI covers completely rope?
  } else if (max(ci) > max(range_rope) && min(ci) < min(range_rope)) {
    coverage <- diff_rope / diff_ci

    # CI inside rope and outside max rope?
  } else if (min(ci) >= min(range_rope) && max(ci) > max(range_rope)) {
    diff_in_rope <- max(range_rope) - min(ci)
    coverage <- diff_in_rope / diff_ci

    # CI inside rope and outside min rope?
  } else if (max(ci) <= max(range_rope) && min(ci) < min(range_rope)) {
    diff_in_rope <- max(ci) - min(range_rope)
    coverage <- diff_in_rope / diff_ci
  }

  coverage
}


# this function simulates a normal distribution, which approximately has the
# same range / limits as the confidence interval, thus indeed representing a
# normally distributed confidence interval. We then calculate the probability
# mass of this interval that is inside the ROPE.
.rope_coverage <- function(ci = 0.95, range_rope, ci_range, dof = Inf) {
  out <- .generate_posterior_from_ci(ci, ci_range, dof = dof)
  # compare: ci_range and range(out)
  # The SGPV refers to the proportion of the confidence interval inside the
  # full ROPE - thus, we set ci = 1 here
  rc <- bayestestR::rope(out, range = range_rope, ci = 1)
  rc$ROPE_Percentage
}


.generate_posterior_from_ci <- function(ci = 0.95, ci_range, dof = Inf, precision = 10000) {
  # this function creates an approximate normal distribution that covers the
  # CI-range, i.e. we "simulate" a posterior distribution from a frequentist CI

  # sanity check - dof argument
  if (is.null(dof)) {
    dof <- Inf
  }
  # first we need the range of the CI (in units), also to calculate the mean of
  # the normal distribution
  diff_ci <- abs(diff(ci_range))
  mean_dist <- ci_range[2] - (diff_ci / 2)
  # then we need the critical values of the quantiles from the CI range
  z_value <- stats::qt((1 + ci) / 2, df = dof)
  # the range of Z-scores (from lower to upper quantile) gives us the range of
  # the provided interval in terms of standard deviations. now we divide the
  # known range of the provided CI (in units) by the z-score-range, which will
  # give us the standard deviation of the distribution.
  sd_dist <- diff_ci / diff(c(-1 * z_value, z_value))
  # generate normal-distribution if we don't have t-distribution, or if
  # we don't have necessary packages installed
  if (is.infinite(dof) || !insight::check_if_installed("distributional", quietly = TRUE)) {
    # tell user to install "distributional"
    if (!is.infinite(dof)) {
      insight::format_alert("For models with only few degrees of freedom, install the {distributional} package to increase accuracy of `p_direction()`, `p_significance()` and `equivalence_test()`.") # nolint
    }
    # we now know all parameters (mean and sd) to simulate a normal distribution
    bayestestR::distribution_normal(n = precision, mean = mean_dist, sd = sd_dist)
  } else {
    insight::check_if_installed("distributional")
    out <- distributional::dist_student_t(df = dof, mu = mean_dist, sigma = sd_dist)
    sort(unlist(distributional::generate(out, times = precision), use.names = FALSE))
  }
}


.add_p_to_equitest <- function(model, ci, range, vcov = NULL, vcov_args = NULL, ...) {
  tryCatch(
    {
      params <- insight::get_parameters(model)

      # remove dispersion components
      if (!is.null(params$Component)) {
        params <- params[params$Component != "dispersion", ]
      }

      # degrees of freedom
      dof <- insight::get_df(x = model, type = "wald")

      # mu
      params$mu <- params$Estimate * -1

      # se
      se <- standard_error(model, vcov = vcov, vcov_args = vcov_args, ...)

      stats::pt((range[1] - params$mu) / se$SE, df = dof, lower.tail = TRUE) +
        stats::pt((range[2] - params$mu) / se$SE, df = dof, lower.tail = FALSE)
    },
    error = function(e) {
      NULL
    }
  )
}


# methods ----------------


#' @export
format.equivalence_test_lm <- function(x,
                                       digits = 2,
                                       ci_digits = digits,
                                       p_digits = 3,
                                       ci_width = NULL,
                                       ci_brackets = NULL,
                                       format = "text",
                                       zap_small = FALSE,
                                       ...) {
  # default brackets are parenthesis for HTML / MD
  if ((is.null(ci_brackets) || isTRUE(ci_brackets)) && (identical(format, "html") || identical(format, "markdown"))) {
    ci_brackets <- c("(", ")")
  } else if (is.null(ci_brackets) || isTRUE(ci_brackets)) {
    ci_brackets <- c("[", "]")
  }

  # main formatting
  out <- insight::format_table(
    x,
    digits = digits,
    ci_digits = ci_digits,
    p_digits = p_digits,
    ci_width = ci_width,
    ci_brackets = ci_brackets,
    zap_small = zap_small,
    ...
  )

  # format column names
  colnames(out)[which(colnames(out) == "Equivalence (ROPE)")] <- "Equivalence"
  out$ROPE <- NULL

  # only show supported components
  if ("Component" %in% colnames(out)) {
    out <- out[out$Component %in% c("conditional", "count"), ]
  }

  out
}


#' @export
print.equivalence_test_lm <- function(x,
                                      digits = 2,
                                      ci_digits = digits,
                                      p_digits = 3,
                                      ci_brackets = NULL,
                                      zap_small = FALSE,
                                      ...) {
  orig_x <- x

  rule <- attributes(x)$rule
  if (is.null(rule)) {
    insight::print_color("# Test for Practical Equivalence\n\n", "blue")
  } else if (rule == "cet") {
    insight::print_color("# Conditional Equivalence Testing\n\n", "blue")
  } else if (rule == "classic") {
    insight::print_color("# TOST-test for Practical Equivalence\n\n", "blue")
  } else {
    insight::print_color("# Test for Practical Equivalence\n\n", "blue")
  }

  .rope <- attr(x, "rope", exact = TRUE)
  cat(sprintf("  ROPE: [%.*f %.*f]\n\n", digits, .rope[1], digits, .rope[2]))

  # formatting
  x <- format(x,
    digits = digits,
    ci_digits = ci_digits,
    p_digits = p_digits,
    ci_width = "auto",
    ci_brackets = ci_brackets,
    format = "text",
    zap_small = zap_small,
    ...
  )

  if ("Group" %in% colnames(x)) {
    out <- split(x, x$Group)
    for (i in names(out)) {
      insight::print_color(sprintf("Group: %s\n\n", i), "red")
      cat(insight::export_table(out[[i]]))
    }
  } else {
    cat(insight::export_table(x))
  }
  invisible(orig_x)
}


#' @export
plot.equivalence_test_lm <- function(x, ...) {
  insight::check_if_installed("see")
  NextMethod()
}


# helper for print_html / print_md --------------------

.print_equivalence_test_lm <- function(
  x,
  digits = 2,
  ci_brackets = c("(", ")"),
  zap_small = FALSE,
  format = "markdown",
  ...
) {
  rule <- attributes(x)$rule
  rope <- attributes(x)$rope

  if (is.null(rule)) {
    table_caption <- "Test for Practical Equivalence"
  } else if (rule == "cet") {
    table_caption <- "Conditional Equivalence Testing"
  } else if (rule == "classic") {
    table_caption <- "TOST-test for Practical Equivalence"
  } else {
    table_caption <- "Test for Practical Equivalence"
  }

  if ("Component" %in% colnames(x)) {
    x <- x[x$Component %in% c("conditional", "count"), ]
  }

  formatted_table <- insight::format_table(
    x,
    pretty_names = TRUE,
    digits = digits,
    ci_width = NULL,
    ci_brackets = ci_brackets,
    zap_small = zap_small,
    ...
  )

  colnames(formatted_table)[which(colnames(formatted_table) == "Equivalence (ROPE)")] <- "H0"
  formatted_table$ROPE <- NULL

  # col_order <- c("Parameter", "H0", "% in ROPE", colnames(formatted_table)[grepl(" CI$", colnames(formatted_table))])
  # col_order <- c(col_order, setdiff(colnames(formatted_table), col_order))
  # formatted_table <- formatted_table[col_order]

  # replace brackets by parenthesis
  if (!is.null(ci_brackets) && "Parameter" %in% colnames(formatted_table)) {
    formatted_table$Parameter <- gsub("[", ci_brackets[1], formatted_table$Parameter, fixed = TRUE)
    formatted_table$Parameter <- gsub("]", ci_brackets[2], formatted_table$Parameter, fixed = TRUE)
  }

  if (!is.null(rope)) {
    names(formatted_table)[names(formatted_table) == "% in ROPE"] <- sprintf(
      "%% in ROPE (%.*f, %.*f)",
      digits,
      rope[1],
      digits,
      rope[2]
    ) # nolint
  }

  insight::export_table(
    formatted_table,
    format = format,
    caption = table_caption,
    align = "firstleft",
    ...
  )
}

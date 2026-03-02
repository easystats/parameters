#' @importFrom bayestestR p_direction
#' @export
bayestestR::p_direction


#' @title Probability of Direction (pd)
#' @name p_direction.lm
#'
#' @description Compute the **Probability of Direction** (*pd*, also known as
#' the Maximum Probability of Effect - *MPE*). This can be interpreted as the
#' probability that a parameter (described by its full confidence, or
#' "compatibility" interval) is strictly positive or negative (whichever is the
#' most probable). Although differently expressed, this index is fairly similar
#' (i.e., is strongly correlated) to the frequentist *p-value* (see 'Details').
#'
#' @param x A statistical model.
#' @inheritParams bayestestR::p_direction
#' @inheritParams model_parameters.default
#' @param ... Arguments passed to other methods, e.g. `ci()`. Arguments like
#' `vcov` or `vcov_args` can be used to compute confidence intervals using a
#' specific variance-covariance matrix for the standard errors.
#'
#' @seealso See also [`equivalence_test()`], [`p_function()`] and
#' [`p_significance()`] for functions related to checking effect existence and
#' significance.
#'
#' @inheritSection bayestestR::p_direction What is the *pd*?
#'
#' @inheritSection bayestestR::p_direction Relationship with the p-value
#'
#' @inheritSection bayestestR::p_direction Possible Range of Values
#'
#' @inheritSection model_parameters Statistical inference - how to quantify evidence
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
#'
#' @examplesIf requireNamespace("bayestestR") && require("see", quietly = TRUE) && requireNamespace("sandwich")
#' data(qol_cancer)
#' model <- lm(QoL ~ time + age + education, data = qol_cancer)
#' p_direction(model)
#'
#' # based on heteroscedasticity-robust standard errors
#' p_direction(model, vcov = "HC3")
#'
#' result <- p_direction(model)
#' plot(result)
#' @export
p_direction.lm <- function(x,
                           ci = 0.95,
                           method = "direct",
                           null = 0,
                           vcov = NULL,
                           vcov_args = NULL,
                           ...) {
  # generate normal distribution based on CI range
  result <- .posterior_ci(x, ci, vcov = vcov, vcov_args = vcov_args, ...)

  # copy
  out <- result$out
  posterior <- result$posterior

  # add pd
  out$pd <- as.numeric(bayestestR::p_direction(
    posterior,
    method = method,
    null = null,
    ...
  ))

  # reorder
  out <- out[intersect(c("Parameter", "CI", "CI_low", "CI_high", "pd", "Effects", "Component"), colnames(out))]

  attr(out, "data") <- posterior
  attr(out, "null") <- null
  class(out) <- c("p_direction_lm", "p_direction", "see_p_direction", "data.frame")
  out
}


# methods ---------------------------------------------------------------------

#' @export
print.p_direction_lm <- function(x, digits = 2, p_digits = 3, ...) {
  null <- attributes(x)$null
  caption <- sprintf(
    "Probability of Direction (null: %s)",
    insight::format_value(null, digits = digits, protect_integer = TRUE)
  )

  x <- insight::format_table(x, digits = digits, p_digits = p_digits)
  cat(insight::export_table(x, title = caption, ...))
}


# other classes --------------------------------------------------------------

#' @export
p_direction.glm <- p_direction.lm

#' @export
p_direction.coxph <- p_direction.lm

#' @export
p_direction.svyglm <- p_direction.lm

#' @export
p_direction.glmmTMB <- p_direction.lm

#' @export
p_direction.merMod <- p_direction.lm

#' @export
p_direction.wbm <- p_direction.lm

#' @export
p_direction.lme <- p_direction.lm

#' @export
p_direction.gee <- p_direction.lm

#' @export
p_direction.gls <- p_direction.lm

#' @export
p_direction.feis <- p_direction.lm

#' @export
p_direction.felm <- p_direction.lm

#' @export
p_direction.mixed <- p_direction.lm

#' @export
p_direction.hurdle <- p_direction.lm

#' @export
p_direction.zeroinfl <- p_direction.lm

#' @export
p_direction.rma <- p_direction.lm

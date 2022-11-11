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
#'
#' @note
#' Curently, `p_function()` computes intervals based on Wald t- or z-statistic.
#' For certain models (like mixed models), profiled intervals may be more
#' accurate, however, this is currently not supported.
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
#' in unconditional terms. Instead of referring to the long-term property and
#' repeated trials when interpreting interval estimates (so-called "aleatory
#' probability", _Schweder 2018_), and assuming that all underlying assumptions
#' are correct and met, `p_function()` interprets _p_-values in a Fisherian way
#' as "_continuous_ measure of evidence against the very test hypothesis _and_
#' entire model (all assumptions) used to compute it"
#' ([P-Values Are Tough and S-Values Can Help](https://lesslikely.com/statistics/s-values/)).
#'
#' This interpretation as a continuous measure of evidence against the test
#' hypothesis and the entire model used to compute it can be seen in the
#' figure below (taken from
#' [P-Values Are Tough and S-Values Can Help](https://lesslikely.com/statistics/s-values/)).
#' The "conditional" interpretation of _p_-values and interval
#' estimates (A) implicitly assumes certain assumptions to be true, thus the
#' interpretation is "conditioned" on these assumptions (i.e. assumptions are
#' taken as given). The unconditional interpretation (B), however, questions all
#' these assumptions.
#'
#' \if{html}{\cr \figure{unconditional_interpretation.png}{options: alt="Conditional versus unconditional interpretations of P-values"} \cr}
#'
#' "Emphasizing unconditional interpretations helps avoid overconfident and
#' misleading inferences in light of uncertainties about the assumptions used
#' to arrive at the statistical results." (_Greenland et al. 2022_).
#'
#' In other words, the term compatibility interval emphasizes "the dependence
#' of the _p_-value on the assumptions as well as on the data, recognizing that
#' _p_<0.05 can arise from assumption violations even if the effect under
#' study is null" (_Gelman/Greenland 2019_).
#'
#' ## Probabilistic interpretation of compatibility intervals
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
#' "The realized confidence distribution is clearly an epistemic probability
#' distribution" (_Schweder 2018_). In Bayesian word, compatibility intervals
#' (or confidence distributons, or consonance curves) are "posteriors without
#' priors" (_Schweder, Hjort, 2003_). In this regard, interpretation of _p_-values
#' might be guided using [`bayestestR::p_to_pd()`].
#'
#' @return A data frame with p-values and compatibility intervals.
#'
#' @references
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
#'   In Stigum, B. (ed.),Econometrics and the Philosophy of Economics: Theory
#'   Data Confrontation in Economics, pp. 285-217. Princeton University Press,
#'   Princeton, NJ, 2003
#'
#' - Schweder T, Hjort NL. Confidence, Likelihood, Probability: Statistical
#'   inference with condidence distributions. Cambridge University Press, 2016.
#'
#' @examples
#' model <- lm(Sepal.Length ~ Species, data = iris)
#' p_function(model)
#' @export
p_function <- function(model,
                       ci_levels = c(0.25, 0.5, 0.75, emph = 0.95),
                       exponentiate = FALSE,
                       effects = "fixed",
                       component = "all",
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
    component = component
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
      pattern = c("CI_low", "CI_high"),
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

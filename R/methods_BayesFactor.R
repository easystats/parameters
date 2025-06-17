# classes: .BFBayesFactor


#' Parameters from BayesFactor objects
#'
#' Parameters from `BFBayesFactor` objects from `{BayesFactor}` package.
#'
#' @param model Object of class `BFBayesFactor`.
#' @param include_proportions Logical that decides whether to include posterior
#'   cell proportions/counts for Bayesian contingency table analysis (from
#'   `BayesFactor::contingencyTableBF()`). Defaults to `FALSE`, as this
#'   information is often redundant.
#' @inheritParams bayestestR::describe_posterior
#' @inheritParams p_value
#' @inheritParams model_parameters.htest
#'
#' @details
#' The meaning of the extracted parameters:
#'
#' - For [BayesFactor::ttestBF()]: `Difference` is the raw difference between
#'   the means.
#' - For [BayesFactor::correlationBF()]: `rho` is the linear correlation
#'   estimate (equivalent to Pearson's *r*).
#' - For [BayesFactor::lmBF()] / [BayesFactor::generalTestBF()]
#'   / [BayesFactor::regressionBF()] / [BayesFactor::anovaBF()]: in addition to
#'   parameters of the fixed and random effects, there are: `mu` is the
#'   (mean-centered) intercept; `sig2` is the model's sigma; `g` / `g_*` are
#'   the *g* parameters; See the *Bayes Factors for ANOVAs* paper
#'   (\doi{10.1016/j.jmp.2012.08.001}).
#'
#' @examplesIf require("BayesFactor")
#' \donttest{
#' # Bayesian t-test
#' model <- BayesFactor::ttestBF(x = rnorm(100, 1, 1))
#' model_parameters(model)
#' model_parameters(model, es_type = "cohens_d", ci = 0.9)
#'
#' # Bayesian contingency table analysis
#' data(raceDolls)
#' bf <- BayesFactor::contingencyTableBF(
#'   raceDolls,
#'   sampleType = "indepMulti",
#'   fixedMargin = "cols"
#' )
#' model_parameters(bf,
#'   centrality = "mean",
#'   dispersion = TRUE,
#'   verbose = FALSE,
#'   es_type = "cramers_v"
#' )
#' }
#' @return A data frame of indices related to the model's parameters.
#' @export
model_parameters.BFBayesFactor <- function(model,
                                           centrality = "median",
                                           dispersion = FALSE,
                                           ci = 0.95,
                                           ci_method = "eti",
                                           test = "pd",
                                           rope_range = "default",
                                           rope_ci = 0.95,
                                           priors = TRUE,
                                           es_type = NULL,
                                           include_proportions = FALSE,
                                           verbose = TRUE,
                                           ...) {
  insight::check_if_installed("BayesFactor")

  if (any(startsWith(names(model@numerator), "Null"))) {
    if (isTRUE(verbose)) {
      insight::format_alert(
        "Nothing to compute for point-null models.",
        "See github.com/easystats/parameters/issues/226"
      )
    }
    return(NULL)
  }

  if (is.null(insight::get_parameters(model, verbose = FALSE))) {
    if (isTRUE(verbose)) {
      insight::format_warning("Can't extract model parameters.")
    }
    return(NULL)
  }

  out <- bayestestR::describe_posterior(
    model,
    centrality = centrality,
    dispersion = dispersion,
    ci = ci,
    ci_method = ci_method,
    test = test,
    rope_range = rope_range,
    rope_ci = rope_ci,
    priors = priors,
    verbose = verbose,
    ...
  )

  bf_type <- .classify_BFBayesFactor(model)

  # Add components and effects columns
  cleaned_params <- NULL
  out <- tryCatch(
    {
      cleaned_params <- insight::clean_parameters(model)
      merge(out, cleaned_params[, c("Parameter", "Effects", "Component")], sort = FALSE)
    },
    error = function(e) {
      out
    }
  )

  # Extract BF
  tryCatch(
    {
      bfm <- as.data.frame(bayestestR::bayesfactor_models(model)[-1, ])
      if (is.null(bfm$log_BF)) {
        out$BF <- bfm$BF
      } else {
        out$BF <- exp(bfm$log_BF)
      }
    },
    error = function(e) {
      NULL
    }
  )

  # leave out redundant posterior cell proportions/counts
  if (bf_type == "xtable" && isFALSE(include_proportions)) {
    out <- out[which(!startsWith(out$Parameter, "cell[")), , drop = FALSE]
  }

  # Effect size?
  if (!is.null(es_type)) {
    # needs {effectsize} to be installed
    insight::check_if_installed("effectsize")

    tryCatch(
      {
        effsize <- effectsize::effectsize(model,
          centrality = centrality,
          dispersion = dispersion,
          ci = ci,
          ci_method = ci_method,
          rope_ci = rope_ci,
          type = es_type,
          ...
        )

        if (bf_type == "xtable" && isTRUE(include_proportions)) {
          out <- merge(out, effsize, sort = FALSE, all = TRUE)
        } else {
          if (bf_type == "xtable") {
            prefix <- "Cramers_"
          } else {
            prefix <- "d_"
          }
          ci_cols <- startsWith(colnames(effsize), "CI_")
          colnames(effsize)[ci_cols] <- paste0(prefix, colnames(effsize)[ci_cols])
          out$CI <- NULL
          out <- cbind(out, effsize)
        }
      },
      error = function(e) {
        NULL
      }
    )
  }

  # # Remove unnecessary columns
  # if ("CI" %in% names(out) && length(stats::na.omit(unique(out$CI))) == 1) {
  #   out$CI <- NULL
  # }
  if ("ROPE_CI" %in% names(out) && length(stats::na.omit(unique(out$ROPE_CI))) == 1) {
    out$ROPE_CI <- NULL
  }
  if ("ROPE_low" %in% names(out)) {
    out$ROPE_low <- NULL
    out$ROPE_high <- NULL
  }

  # ==== remove Component column if not needed

  if (!is.null(out$Component) && insight::has_single_value(out$Component, remove_na = TRUE)) out$Component <- NULL
  if (!is.null(out$Effects) && insight::has_single_value(out$Effects, remove_na = TRUE)) out$Effects <- NULL


  # ==== remove rows and columns with complete `NA`s

  out <- datawizard::remove_empty(out)

  # validation check: make sure BF column still exists,
  # see https://github.com/easystats/correlation/issues/269
  if (is.null(out$BF)) {
    out$BF <- NA
  }


  # ==== pretty parameter names

  cp <- out$Parameter

  if (!is.null(cleaned_params) && length(cleaned_params$Cleaned_Parameter) == length(cp) && bf_type == "linear") {
    match_params <- stats::na.omit(match(cp, cleaned_params$Parameter))
    cp <- cleaned_params$Cleaned_Parameter[match_params]
  }

  pretty_names <- stats::setNames(
    gsub("Cohens_d", "Cohen's D", gsub("Cramers_v", "Cramer's V", cp, fixed = TRUE), fixed = TRUE),
    out$Parameter
  )

  if (!"Method" %in% names(out)) {
    out$Method <- .method_BFBayesFactor(model)
  }

  # reorder
  col_order <- c(
    "Parameter", "Mean", "Median", "MAD",
    "CI", "CI_low", "CI_high", "SD", "Cohens_d", "Cramers_v", "Cramers_v_adjusted", "d_CI_low", "d_CI_high",
    "Cramers_CI_low", "Cramers_CI_high", "pd", "ROPE_Percentage", "Prior_Distribution",
    "Prior_Location", "Prior_Scale", "Effects", "Component", "BF", "Method"
  )
  out <- out[col_order[col_order %in% names(out)]]


  attr(out, "title") <- unique(out$Method)
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  attr(out, "pretty_names") <- pretty_names
  attr(out, "ci_test") <- ci

  out <- .add_model_parameters_attributes(
    params = out,
    model = model,
    ci = ci,
    ci_method = ci_method,
    verbose = verbose
  )

  class(out) <- c("parameters_model", "see_parameters_model", class(out))
  out
}


#' @export
p_value.BFBayesFactor <- function(model, ...) {
  p <- bayestestR::p_direction(model, ...)
  .data_frame(
    Parameter = .remove_backticks_from_string(p$Parameter),
    p = sapply(p$pd, bayestestR::convert_pd_to_p, simplify = TRUE)
  )
}


# helper -------


.classify_BFBayesFactor <- function(x) {
  insight::check_if_installed("BayesFactor")

  if (inherits(x@denominator, "BFcorrelation")) {
    "correlation"
  } else if (inherits(x@denominator, "BFoneSample")) {
    "ttest1"
  } else if (inherits(x@denominator, "BFindepSample")) {
    "ttest2"
  } else if (inherits(x@denominator, "BFmetat")) {
    "meta"
  } else if (inherits(x@denominator, "BFlinearModel")) {
    "linear"
  } else if (inherits(x@denominator, "BFcontingencyTable")) {
    "xtable"
  } else if (inherits(x@denominator, "BFproportion")) {
    "proptest"
  } else {
    class(x@denominator)
  }
}

.method_BFBayesFactor <- function(x) {
  if (inherits(x@denominator, "BFcorrelation")) {
    "Bayesian correlation analysis"
  } else if (inherits(x@denominator, c("BFoneSample", "BFindepSample"))) {
    "Bayesian t-test"
  } else if (inherits(x@denominator, "BFmetat")) {
    "Meta-analytic Bayes factors"
  } else if (inherits(x@denominator, "BFlinearModel")) {
    "Bayes factors for linear models"
  } else if (inherits(x@denominator, "BFcontingencyTable")) {
    "Bayesian contingency table analysis"
  } else if (inherits(x@denominator, "BFproportion")) {
    "Bayesian proportion test"
  } else {
    NA_character_
  }
}

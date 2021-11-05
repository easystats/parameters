# classes: .BFBayesFactor


#' Parameters from BayesFactor objects
#'
#' Parameters from `BFBayesFactor` objects from `{BayesFactor}` package.
#'
#' @param model Object of class `BFBayesFactor`.
#' @param cohens_d If `TRUE`, compute Cohens' *d* as index of effect size. Only
#'   applies to objects from `ttestBF()`. See `effectsize::cohens_d()` for
#'   details.
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
#' \itemize{
#'   \item For [BayesFactor::ttestBF()]: `Difference` is the raw
#'   difference between the means. \item For
#'   [BayesFactor::correlationBF()]: `rho` is the linear
#'   correlation estimate (equivalent to Pearson's *r*). \item For
#'   [BayesFactor::lmBF()] / [BayesFactor::generalTestBF()]
#'   / [BayesFactor::regressionBF()] /
#'   [BayesFactor::anovaBF()]: in addition to parameters of the fixed
#'   and random effects, there are: `mu` is the (mean-centered) intercept;
#'   `sig2` is the model's sigma; `g` / `g_*` are the *g*
#'   parameters; See the *Bayes Factors for ANOVAs* paper
#'   (\doi{10.1016/j.jmp.2012.08.001}).
#' }
#'
#' @examples
#' \donttest{
#' if (require("BayesFactor")) {
#'   # Bayesian t-test
#'   model <- ttestBF(x = rnorm(100, 1, 1))
#'   model_parameters(model)
#'   model_parameters(model, cohens_d = TRUE, ci = .9)
#'
#'   # Bayesian contingency table analysis
#'   data(raceDolls)
#'   bf <- contingencyTableBF(raceDolls, sampleType = "indepMulti", fixedMargin = "cols")
#'   model_parameters(bf,
#'     centrality = "mean",
#'     dispersion = TRUE,
#'     verbose = FALSE,
#'     cramers_v = TRUE
#'   )
#' }
#' }
#' @return A data frame of indices related to the model's parameters.
#' @export
model_parameters.BFBayesFactor <- function(model,
                                           centrality = "median",
                                           dispersion = FALSE,
                                           ci = 0.95,
                                           ci_method = "hdi",
                                           test = c("pd", "rope"),
                                           rope_range = "default",
                                           rope_ci = 0.95,
                                           priors = TRUE,
                                           cohens_d = NULL,
                                           cramers_v = NULL,
                                           include_proportions = FALSE,
                                           verbose = TRUE,
                                           ...) {
  if (any(grepl("^Null", names(model@numerator)))) {
    if (isTRUE(verbose)) {
      insight::print_color("Nothing to compute for point-null models.\nSee github.com/easystats/parameters/issues/226\n", "red")
    }
    return(NULL)
  }

  if (is.null(insight::get_parameters(model, verbose = verbose))) {
    if (isTRUE(verbose)) {
      warning("Can't extract model parameters.", call. = FALSE)
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
  tryCatch(
    {
      cleaned_params <- insight::clean_parameters(model)
      out <- merge(out, cleaned_params[, c("Parameter", "Effects", "Component")], sort = FALSE)
    },
    error = function(e) {
      NULL
    }
  )

  # Extract BF
  tryCatch(
    {
      bfm <- as.data.frame(bayestestR::bayesfactor_models(model)[-1, ])
      if (!is.null(bfm$log_BF)) {
        out$BF <- exp(bfm$log_BF)
      } else {
        out$BF <- bfm$BF
      }
    },
    error = function(e) {
      NULL
    }
  )

  # Effect size?
  if (bf_type %in% c("ttest1", "ttest2") && !is.null(cohens_d) ||
    bf_type == "xtable" && !is.null(cramers_v)) {
    # needs {effectsize} to be installed
    insight::check_if_installed("effectsize")

    tryCatch(
      {
        effsize <- effectsize::effectsize(model,
          centrality = centrality,
          dispersion = dispersion,
          ci = ci,
          ci_method = ci_method,
          rope_ci = rope_ci
        )
        out <- merge(out, effsize, sort = FALSE, all = TRUE)
      },
      error = function(e) {
        NULL
      }
    )
  }

  # leave out redundant posterior cell proportions/counts
  if (bf_type == "xtable" && isFALSE(include_proportions)) {
    out <- out[which(!grepl("cell", out$Parameter)), , drop = FALSE]
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

  if (!is.null(out$Component) && .n_unique(out$Component) == 1) out$Component <- NULL
  if (!is.null(out$Effects) && .n_unique(out$Effects) == 1) out$Effects <- NULL


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

  attr(out, "title") <- unique(out$Method)
  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  attr(out, "pretty_names") <- pretty_names
  out <- .add_model_parameters_attributes(
    params = out,
    model = model,
    ci = ci,
    ci_method = ci_method,
    verbose = verbose
  )

  # reorder
  col_order <- c(
    "Parameter", "Mean", "Median", "MAD",
    "Cohens_d", "Cramers_v", "CI", "CI_low", "CI_high", "SD",
    "pd", "ROPE_Percentage", "Prior_Distribution", "Prior_Location", "Prior_Scale",
    "Effects", "Component", "BF", "Method"
  )
  out <- out[col_order[col_order %in% names(out)]]


  class(out) <- c("parameters_model", "see_parameters_model", class(out))
  out
}


#' p-values for Bayesian Models
#'
#' This function attempts to return, or compute, p-values of Bayesian models.
#'
#' @param model A statistical model.
#' @inheritParams p_value
#'
#' @details
#'
#' For Bayesian models, the p-values corresponds to the *probability of
#' direction* ([bayestestR::p_direction()]), which is converted to a p-value
#' using `bayestestR::convert_pd_to_p()`.
#'
#' @return The p-values.
#'
#' @examples
#' data(iris)
#' model <- lm(Petal.Length ~ Sepal.Length + Species, data = iris)
#' p_value(model)
#' @export
p_value.BFBayesFactor <- function(model, ...) {
  p <- bayestestR::p_direction(model)
  .data_frame(
    Parameter = .remove_backticks_from_string(p$Parameter),
    p = sapply(p$pd, bayestestR::convert_pd_to_p, simplify = TRUE)
  )
}




# helper -------


.classify_BFBayesFactor <- function(x) {
  if (!requireNamespace("BayesFactor", quietly = TRUE)) {
    stop("This function needs `BayesFactor` to be installed.")
  }

  if (any(class(x@denominator) %in% c("BFcorrelation"))) {
    "correlation"
  } else if (any(class(x@denominator) %in% c("BFoneSample"))) {
    "ttest1"
  } else if (any(class(x@denominator) %in% c("BFindepSample"))) {
    "ttest2"
  } else if (any(class(x@denominator) %in% c("BFmetat"))) {
    "meta"
  } else if (any(class(x@denominator) %in% c("BFlinearModel"))) {
    "linear"
  } else if (any(class(x@denominator) %in% c("BFcontingencyTable"))) {
    "xtable"
  } else if (any(class(x@denominator) %in% c("BFproportion"))) {
    "proptest"
  } else {
    class(x@denominator)
  }
}

.method_BFBayesFactor <- function(x) {
  if (!requireNamespace("BayesFactor", quietly = TRUE)) {
    stop("This function needs `BayesFactor` to be installed.")
  }

  if (any(class(x@denominator) %in% c("BFcorrelation"))) {
    "Bayesian correlation analysis"
  } else if (any(class(x@denominator) %in% c("BFoneSample", "BFindepSample"))) {
    "Bayesian t-test"
  } else if (any(class(x@denominator) %in% c("BFmetat"))) {
    "Meta-analytic Bayes factors"
  } else if (any(class(x@denominator) %in% c("BFlinearModel"))) {
    "Bayes factors for linear models"
  } else if (any(class(x@denominator) %in% c("BFcontingencyTable"))) {
    "Bayesian contingency table analysis"
  } else if (any(class(x@denominator) %in% c("BFproportion"))) {
    "Bayesian proportion test"
  } else {
    NA_character_
  }
}

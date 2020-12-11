#' Parameters from BayesFactor objects
#'
#' Parameters of BayesFactor objects.
#'
#' @param model Object of class \code{BFBayesFactor}.
#' @inheritParams bayestestR::describe_posterior
#' @inheritParams p_value
#'
#' @details
#' The meaning of the extracted parameters: For
#' \code{\link[BayesFactor]{ttestBF}}: \code{Difference} is the raw difference
#' between the means. For \code{\link[BayesFactor]{correlationBF}}: \code{rho}
#' is the linear correlation estimate (equivalent to Pearson's \emph{r}).
#' \code{\link[BayesFactor]{lmBF}} / \code{\link[BayesFactor]{generalTestBF}} /
#' For \code{\link[BayesFactor]{regressionBF}} /
#' \code{\link[BayesFactor]{anovaBF}}: in addition to parameters of the fixed
#' and random effects, there are: \code{mu} is the (mean-centered) intercept;
#' \code{sig2} is the model's sigma; \code{g} / \code{g_*} are the \emph{g}
#' parameters; See the
#' \href{https://doi.org/10.1016/j.jmp.2012.08.001}{\emph{Bayes Factors for
#' ANOVAs} paper}.
#'
#' @examples
#' \donttest{
#' if (require("BayesFactor")) {
#'   model <- ttestBF(x = rnorm(100, 1, 1))
#'   model_parameters(model)
#' }
#' }
#' @return A data frame of indices related to the model's parameters.
#' @importFrom stats na.omit
#' @importFrom bayestestR bayesfactor_models
#' @importFrom insight get_priors
#' @export
model_parameters.BFBayesFactor <- function(model,
                                           centrality = "median",
                                           dispersion = FALSE,
                                           ci = 0.89,
                                           ci_method = "hdi",
                                           test = c("pd", "rope"),
                                           rope_range = "default",
                                           rope_ci = 0.89,
                                           priors = TRUE,
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

  # Add components and effects columns
  tryCatch(
    {
      params <- insight::clean_parameters(model)[, c("Parameter", "Effects", "Component")]
      out <- merge(out, params, sort = FALSE)
    },
    error = function(e) {
      NULL
    }
  )

  # Extract BF
  tryCatch(
    {
      out$BF <- as.data.frame(bayestestR::bayesfactor_models(model)[-1, ])$BF
    },
    error = function(e) {
      NULL
    }
  )

  # Remove unnecessary columns
  if ("CI" %in% names(out) && length(stats::na.omit(unique(out$CI))) == 1) {
    out$CI <- NULL
  }
  if ("ROPE_CI" %in% names(out) && length(stats::na.omit(unique(out$ROPE_CI))) == 1) {
    out$ROPE_CI <- NULL
  }
  if ("ROPE_low" %in% names(out)) {
    out$ROPE_low <- NULL
    out$ROPE_high <- NULL
  }

  # ==== remove Component column if not needed

  if (.n_unique(out$Component) == 1) out$Component <- NULL
  if (.n_unique(out$Effects) == 1) out$Effects <- NULL

  attr(out, "ci") <- ci
  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(out) <- c("parameters_model", class(out))

  out
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
  } else {
    class(x@denominator)
  }
}

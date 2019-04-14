#' Describe posterior distributions.
#'
#' @param posteriors A dataframe of posterior draws.
#' @param ci Credible Interval (CI) level. Default to 0.90 (90\%).
#' @param ci_method The type of index used for Credible Interval. Can be \code{"hdi"} (default, see \link[bayestestR]{hdi}) or "quantile" (see \link[bayestestR]{ci}).
#' @param estimate The \href{https://easystats.github.io/bayestestR/articles/2_IndicesEstimationComparison.html}{point-estimate(s)} to compute. Can be a character or a list with "median", "mean" or "MAP".
#' @param test What \href{https://easystats.github.io/bayestestR/articles/3_IndicesExistenceComparison.html}{indices of effect existence} to compute. Can be a character or a list with "p_direction", "rope" or "p_map".
#' @param rope_range \href{https://easystats.github.io/bayestestR/#rope}{ROPE's} lower and higher bounds. Should be a list of two values (e.g., \code{c(-0.1, 0.1)}) or \code{"default"}. If \code{"default"}, the bounds are set to \code{x +- 0.1*SD(response)}.
#' @param rope_full If TRUE, use the proportion of the entire posterior distribution for the equivalence test. Otherwise, use the proportion of HDI as indicated by the \code{ci} argument.
#'
#' @examples
#' describe_posterior(rnorm(1000))
#'
#' @importFrom stats mad median sd setNames
#' @export
describe_posterior <- function(posteriors, ci = .90, ci_method="hdi", estimate = "median", test = c("pd", "rope"), rope_range = "default", rope_full = TRUE) {
  UseMethod("describe_posterior")
}






#' @rdname describe_posterior
#' @export
describe_posterior.numeric <- function(posteriors, ci = .90, ci_method="hdi", estimate = "median", test = c("pd", "rope"), rope_range = "default", rope_full = TRUE) {
  x <- describe_posterior(as.data.frame(posteriors), ci=ci, ci_method=ci_method, estimate=estimate, test=test, rope_range=rope_range, rope_full=rope_full)
  x[names(x) != "Parameter"]
}







#' @rdname describe_posterior
#' @export
describe_posterior.double <- describe_posterior.numeric


#' @rdname describe_posterior
#' @method describe_posterior data.frame
#' @export
describe_posterior.data.frame <- function(posteriors, ci = .90, ci_method="hdi", estimate = "median", test = c("pd", "rope"), rope_range = "default", rope_full = TRUE) {
  # Point estimates
  out <- data.frame("Parameter" = colnames(posteriors))
  if ("median" %in% c(estimate)) {
    out$Median <- sapply(posteriors, median)
    out$MAD <- sapply(posteriors, mad)
  }
  if ("mean" %in% c(estimate)) {
    out$Mean <- sapply(posteriors, mean)
    out$SD <- sapply(posteriors, sd)
  }
  if ("map" %in% c(estimate)) {
    out$MAP <- unlist(sapply(posteriors, bayestestR::map_estimate))
  }

  # CI
  if (!is.null(ci)) {
    if (length(ci) > 1) {
      if(ci_method == "hdi"){
        hdi <- sapply(posteriors, bayestestR::hdi, ci = ci, simplify = FALSE)
      } else{
        hdi <- sapply(posteriors, bayestestR::ci, ci = ci, simplify = FALSE)
      }
      for (i in names(hdi)) {
        current_hdi <- hdi[[i]]

        hdi_low <- as.data.frame(t(setNames(current_hdi$CI_low, as.numeric(current_hdi$CI))), stringsAsFactors = FALSE)
        names(hdi_low) <- paste0("CI_low_", names(hdi_low))

        hdi_high <- as.data.frame(t(setNames(current_hdi$CI_high, as.numeric(current_hdi$CI))), stringsAsFactors = FALSE)
        names(hdi_high) <- paste0("CI_high_", names(hdi_high))

        hdi[[i]] <- cbind(hdi_low, hdi_high)
      }
      hdi <- .flatten_list(hdi)
      hdi <- hdi[names(hdi) != "name"]
    } else {
      if(ci_method == "hdi"){
        hdi <- as.data.frame(t(sapply(posteriors, bayestestR::hdi, ci = ci)), stringsAsFactors = FALSE)
      } else{
        hdi <- as.data.frame(t(sapply(posteriors, bayestestR::ci, ci = ci)), stringsAsFactors = FALSE)
      }
      hdi <- hdi[c("CI_low", "CI_high")]
    }
    hdi <- sapply(hdi, as.numeric)
    if (is.null(ncol(hdi))) hdi <- t(hdi) # Catch When nrow == 1
    out <- cbind(out, hdi)
  }


  # Effect Existence
  if (!is.null(test)) {
    test_list <- tolower(c(test))
    if ("pd" %in% test_list | "p_direction" %in% test_list | "pdir" %in% test_list | "mpe" %in% test_list) {
      out$pd <- sapply(posteriors, bayestestR::p_direction)
    }
    if ("rope" %in% test_list | "equivalence" %in% test_list | "equi" %in% test_list) {
      if (length(ci) == 1 | rope_full) {
        if (rope_full) {
          results_rope <- as.data.frame(t(sapply(posteriors, bayestestR::equivalence_test, range = rope_range, ci = 1)), stringsAsFactors = FALSE)
        } else {
          results_rope <- as.data.frame(t(sapply(posteriors, bayestestR::equivalence_test, range = rope_range, ci = ci)), stringsAsFactors = FALSE)
        }
        results_rope <- results_rope[c("ROPE_Percentage", "ROPE_Equivalence")]
        results_rope$ROPE_Percentage <- as.numeric(results_rope$ROPE_Percentage)
        results_rope$ROPE_Equivalence <- as.character(results_rope$ROPE_Equivalence)
      } else {
        results_rope <- sapply(posteriors, bayestestR::equivalence_test, range = rope_range, ci = ci, simplify = FALSE)
        for (i in names(results_rope)) {
          current_rope <- results_rope[[i]]

          rope_percentage <- as.data.frame(t(setNames(current_rope$ROPE_Percentage, as.numeric(current_rope$CI))), stringsAsFactors = FALSE)
          names(rope_percentage) <- paste0("CI_", names(rope_percentage), "_ROPE_Percentage")
          rope_percentage <- sapply(rope_percentage, as.numeric)

          rope_equivalence <- as.data.frame(t(setNames(current_rope$ROPE_Equivalence, as.numeric(current_rope$CI))), stringsAsFactors = FALSE)
          names(rope_equivalence) <- paste0("CI_", names(rope_equivalence), "_ROPE_Equivalence")
          rope_equivalence <- sapply(rope_equivalence, as.character)

          results_rope[[i]] <- cbind(rope_percentage, rope_equivalence)
        }
        results_rope <- .flatten_list(results_rope)
        results_rope <- results_rope[names(results_rope) != "name"]
      }
      out <- cbind(out, results_rope)
    }
    if ("p_map" %in% test_list | "pmap" %in% test_list) {
      out$p_MAP <- sapply(posteriors, bayestestR::p_map)
    }
    # TODO: add p_ROPE, but first must enhance its implementation in bayestestR
  }

  rownames(out) <- NULL
  return(out)
}

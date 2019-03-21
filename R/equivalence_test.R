#' @title Test for Practical Equivalence
#' @aliases equivalence_test
#'
#' @description Perform a \strong{Test for Practical Equivalence} based on a CI+ROPE decision rule.
#'
#' @param x A model.
#' @param range The range of practical equivalence of an effect. May be \code{"default"},
#'   to automatically define this range based on properties of the model's data.
#' @param ci bla
#' @param verbose blub
#' @param ... Currently not used
#'
#' @examples
#' m <- lm(mpg ~ gear + wt + cyl + hp, data = mtcars)
#' equivalence_test(m)
#'
#' @importFrom stats confint
#' @importFrom bayestestR equivalence_test rope_range
#' @export
equivalence_test.lm <- function(x, range = "default", ci = .95, verbose = TRUE, ...) {
  equivalence_test_models(x, range, ci, verbose, ...)
}


#' @export
equivalence_test.glm <- equivalence_test.lm

#' @export
equivalence_test.merMod <- equivalence_test.lm

#' @export
equivalence_test.glmmTMB <- equivalence_test.lm

#' @export
equivalence_test.MixMod <- equivalence_test.lm


equivalence_test_models <- function(x, range = "default", ci = .95, verbose = TRUE, ...) {
  if (all(range == "default")) {
    range <- bayestestR::rope_range(x)
  } else if (!all(is.numeric(range)) | length(range) != 2) {
    stop("`range` should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1)).")
  }

  conf_int <- .clean_confint(as.data.frame(t(stats::confint(x, level = ci, method = "Wald", estimate = FALSE))))

  l <- lapply(
    conf_int,
    eq_test_numeric,
    range_rope = range,
    verbose = verbose
  )

  dat <- do.call(rbind, l)
  out <- data.frame(
    parameter = names(l),
    ci = ci,
    dat,
    stringsAsFactors = FALSE
  )

  class(out) <- c("equivalence_test_lm", class(out))
  attr(out, "rope") <- range

  out
}


eq_test_numeric <- function(range_ci, range_rope, verbose) {
  if (min(range_ci) > max(range_rope) || max(range_ci) < min(range_rope)) {
    decision <- "rejected"
    coverage <- 0
  } else if (max(range_ci) <= max(range_rope) && min(range_ci) >= min(range_rope)) {
    decision <- "accepted"
    coverage <- 1
  } else {
    diff_rope <- abs(diff(range_rope))
    diff_ci <- abs(diff(range_ci))
    decision <- "undecided"

    if (min(range_rope) >= min(range_ci) && max(range_rope) <= max(range_ci)) {
      coverage <- diff_rope / diff_ci
    } else if (min(range_ci) <= min(range_rope)) {
      coverage <- abs(diff(c(min(range_rope), max(range_ci)))) / diff_ci
    } else {
      coverage <- abs(diff(c(min(range_ci), max(range_rope)))) / diff_ci
    }
  }

  data.frame(
    decision = decision,
    coverage = coverage,
    ci_low = range_ci[1],
    ci_high = range_ci[2],
    stringsAsFactors = FALSE
  )
}
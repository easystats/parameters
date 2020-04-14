#' @importFrom bayestestR equivalence_test
#' @export
bayestestR::equivalence_test



#' @title Equivalence test
#'
#' @description Compute the equivalence test for frequentist models.
#'
#' @param x A statistical model.
#' @param range The range of practical equivalence of an effect. May be \code{"default"},
#'   to automatically define this range based on properties of the model's data.
#' @param ci Confidence Interval (CI) level. Default to 0.95 (95\%).
#' @param verbose Toggle off warnings.
#' @param ... Arguments passed to or from other methods.
#'
#' @seealso For more details, see \code{\link[bayestestR:equivalence_test]{equivalence_test}}.
#'
#' @return A data frame.
#' @examples
#' m <- lm(mpg ~ gear + wt + cyl + hp, data = mtcars)
#' equivalence_test(m)
#' @export
equivalence_test.lm <- function(x, range = "default", ci = .95, verbose = TRUE, ...) {
  out <- .equivalence_test_frequentist(x, range, ci, verbose, ...)

  attr(out, "object_name") <- .safe_deparse(substitute(x))
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
equivalence_test.feis <- equivalence_test.lm

#' @export
equivalence_test.felm <- equivalence_test.lm

#' @export
equivalence_test.mixed <- equivalence_test.lm

#' @export
equivalence_test.hurdle <- equivalence_test.lm

#' @export
equivalence_test.zeroinfl <- equivalence_test.lm





# mixed models, also random effects ----------------------

#' @export
equivalence_test.merMod <- function(x, range = "default", ci = .95, effects = c("fixed", "random"), verbose = TRUE, ...) {
  effects <- match.arg(effects)
  if (effects == "fixed") {
    out <- .equivalence_test_frequentist(x, range, ci, verbose, ...)
  } else {
    out <- .equivalence_test_frequentist_random(x, range, ci, verbose, ...)
  }

  attr(out, "object_name") <- .safe_deparse(substitute(x))
  class(out) <- c("equivalence_test_lm", "see_equivalence_test_lm", class(out))
  out
}


#' @export
equivalence_test.glmmTMB <- equivalence_test.merMod

#' @export
equivalence_test.MixMod <- equivalence_test.merMod






# helper -------------------


#' @importFrom bayestestR equivalence_test rope_range
#' @keywords internal
.equivalence_test_frequentist <- function(x, range = "default", ci = .95, verbose = TRUE, ...) {
  if (all(range == "default")) {
    range <- bayestestR::rope_range(x)
  } else if (!all(is.numeric(range)) | length(range) != 2) {
    stop("`range` should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1)).")
  }

  if (length(ci) > 1) {
    warning("`ci` may only be of length 1. Using first ci-value now.", call. = FALSE)
    ci <- ci[1]
  }

  params <- conf_int <- ci_wald(x, ci = ci)
  conf_int <- as.data.frame(t(conf_int[, c("CI_low", "CI_high")]))

  l <- lapply(
    conf_int,
    .equivalence_test_numeric,
    range_rope = range,
    verbose = verbose
  )

  dat <- do.call(rbind, l)
  if ("Component" %in% colnames(params)) dat$Component <- params$Component

  out <- data.frame(
    Parameter = params$Parameter,
    CI = ci,
    dat,
    stringsAsFactors = FALSE
  )
  attr(out, "rope") <- range
  out
}



#' @importFrom stats pnorm
#' @importFrom insight get_parameters
#' @importFrom bayestestR rope_range
#' @keywords internal
.equivalence_test_frequentist_random <- function(x, range = "default", ci = .95, verbose = TRUE, ...) {
  if (all(range == "default")) {
    range <- bayestestR::rope_range(x)
  } else if (!all(is.numeric(range)) | length(range) != 2) {
    stop("`range` should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1)).")
  }

  if (length(ci) > 1) {
    warning("`ci` may only be of length 1. Using first ci-value now.", call. = FALSE)
    ci <- ci[1]
  }

  params <- insight::get_parameters(x, effects = "random", component = "conditional")
  se <- standard_error(x, effects = "random", component = "conditional")

  alpha <- (1 + ci) / 2
  fac <- stats::qnorm(alpha)

  out <- do.call(rbind, lapply(names(params), function(np) {
    est <- params[[np]][, "(Intercept)"]
    stderr <- se[[np]][, "(Intercept)"]

    d <- data.frame(
      Parameter = rownames(params[[np]]),
      Estimate = est,
      CI_low = est - stderr * fac,
      CI_high = est + stderr * fac,
      CI = ci,
      group = np,
      stringsAsFactors = FALSE
    )

    conf_int <- as.data.frame(t(d[, c("CI_low", "CI_high")]))

    l <- lapply(
      conf_int,
      .equivalence_test_numeric,
      range_rope = range,
      verbose = verbose
    )

    dat <- do.call(rbind, l)
    cbind(d, dat)
  }))

  attr(out, "rope") <- range
  out
}




#' @keywords internal
.equivalence_test_numeric <- function(range_ci, range_rope, verbose) {
  if (min(range_ci) > max(range_rope) || max(range_ci) < min(range_rope)) {
    decision <- "Rejected"
    coverage <- 0
  } else if (max(range_ci) <= max(range_rope) && min(range_ci) >= min(range_rope)) {
    decision <- "Accepted"
    coverage <- 1
  } else {
    diff_rope <- abs(diff(range_rope))
    diff_ci <- abs(diff(range_ci))
    decision <- "Undecided"

    if (min(range_rope) >= min(range_ci) && max(range_rope) <= max(range_ci)) {
      coverage <- diff_rope / diff_ci
    } else if (min(range_ci) <= min(range_rope)) {
      coverage <- abs(diff(c(min(range_rope), max(range_ci)))) / diff_ci
    } else {
      coverage <- abs(diff(c(min(range_ci), max(range_rope)))) / diff_ci
    }
  }

  data.frame(
    CI_low = range_ci[1],
    CI_high = range_ci[2],
    ROPE_low = range_rope[1],
    ROPE_high = range_rope[2],
    ROPE_Percentage = coverage,
    ROPE_Equivalence = decision,
    stringsAsFactors = FALSE
  )
}






# methods ----------------


#' @importFrom insight print_color
#' @export
print.equivalence_test_lm <- function(x, digits = 2, ...) {
  insight::print_color("# Test for Practical Equivalence\n\n", "blue")

  .rope <- attr(x, "rope", exact = TRUE)
  cat(sprintf("  ROPE: [%.*f %.*f]\n\n", digits, .rope[1], digits, .rope[2]))

  if ("Component" %in% colnames(x)) {
    x <- x[x$Component %in% c("conditional", "count"), ]
  }

  if ("group" %in% colnames(x)) {
    out <- split(x, x$group)
    for (i in names(out)) {
      insight::print_color(sprintf("Group: %s\n\n", i), "red")
      .print_equitest_freq(out[[i]], digits, ...)
    }
  } else {
    .print_equitest_freq(x, digits, ...)
  }
}



#' @export
plot.equivalence_test_lm <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot results from equivalence-test. Please install it by running `install.packages('see')`.")
  }
  NextMethod()
}




# method-helper ----------------------

.print_equitest_freq <- function(x, digits, ...) {
  # find the longest CI-value, so we can align the brackets in the ouput
  x$CI_low <- sprintf("%.*f", digits, x$CI_low)
  x$CI_high <- sprintf("%.*f", digits, x$CI_high)

  maxlen_low <- max(nchar(x$CI_low))
  maxlen_high <- max(nchar(x$CI_high))

  x$ROPE_Percentage <- sprintf("%.*f %%", digits, 100 * x$ROPE_Percentage)
  x$conf.int <- sprintf("[%*s %*s]", maxlen_low, x$CI_low, maxlen_high, x$CI_high)

  CI <- unique(x$CI)
  keep.columns <- c("CI", "Parameter", "ROPE_Equivalence", "ROPE_Percentage", "conf.int")

  x <- x[, intersect(keep.columns, colnames(x))]

  colnames(x)[which(colnames(x) == "ROPE_Equivalence")] <- "H0"
  colnames(x)[which(colnames(x) == "ROPE_Percentage")] <- "inside ROPE"

  for (i in CI) {
    xsub <- x[x$CI == i, -which(colnames(x) == "CI"), drop = FALSE]
    colnames(xsub)[ncol(xsub)] <- sprintf("%i%% CI", round(100 * i))
    print.data.frame(xsub, digits = digits, row.names = FALSE)
    cat("\n")
  }
}

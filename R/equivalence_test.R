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
#' @param p_values Logical, if \code{TRUE}, adjusted p-values for equivalence testing are calculated.
#' @param verbose Toggle off warnings.
#' @param ... Arguments passed to or from other methods.
#' @inheritParams model_parameters.merMod
#'
#' @seealso For more details, see \code{\link[bayestestR:equivalence_test]{equivalence_test}}.
#'   Further readings can be found in the references.
#'
#' @details The calculation of p-values is somewhat "experimental". For parameters, where H0...
#'   \itemize{
#'     \item ... is rejected, the p-value equals a NHST as if the upper / lower boundary of the ROPE (see \code{range}) would be the point-null to test against.
#'     \item ... is accepted, the p-value is set to 1.
#'     \item ... is undecided, the p-value equals a NHST against the point-null, however, the "uncertainty" (i.e. ROPE range) is added to the confidence intervals (so the upper confidence interval limit equals the regular upper confidence interval limit + half the ROPE range).
#'   }
#'   All p-values are then adjusted for multiple testing (using \code{\link[stats]{p.adjust}} with \code{method = "fdr"}).
#'
#' @references
#' \itemize{
#'   \item Campbell, H., & Gustafson, P. (2018). Conditional equivalence testing: An alternative remedy for publication bias. PLOS ONE, 13(4), e0195145. https://doi.org/10.1371/journal.pone.0195145
#'   \item Kruschke, J. K. (2018). Rejecting or accepting parameter values in Bayesian estimation. Advances in Methods and Practices in Psychological Science, 1(2), 270-280. doi: 10.1177/2515245918771304
#'   \item Lakens, D. (2017). Equivalence Tests: A Practical Primer for t Tests, Correlations, and Meta-Analyses. Social Psychological and Personality Science, 8(4), 355–362. https://doi.org/10.1177/1948550617697177
#' }
#'
#' @return A data frame.
#' @examples
#' m <- lm(mpg ~ gear + wt + cyl + hp, data = mtcars)
#' equivalence_test(m)
#' @export
equivalence_test.lm <- function(x, range = "default", ci = .95, p_values = FALSE, verbose = TRUE, ...) {
  out <- .equivalence_test_frequentist(x, range, ci, p_values, verbose, ...)

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

#' @rdname equivalence_test.lm
#' @export
equivalence_test.merMod <- function(x, range = "default", ci = .95, effects = c("fixed", "random"), p_values = FALSE, verbose = TRUE, ...) {
  effects <- match.arg(effects)
  if (effects == "fixed") {
    out <- .equivalence_test_frequentist(x, range, ci, p_values, verbose, ...)
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







# Special classes -------------------------

#' @importFrom bayestestR rope_range
#' @export
equivalence_test.parameters_simulate_model <- function(x, range = "default", ci = .95, verbose = TRUE, ...) {
  model_name <- attr(x, "object_name", exact = TRUE)
  # retrieve model
  model <- tryCatch(
    {
      get(model_name, envir = parent.frame())
    },
    error = function(e) {
      NULL
    }
  )

  if (all(range == "default") && !is.null(model)) {
    range <- bayestestR::rope_range(model)
  } else if (!all(is.numeric(range)) | length(range) != 2) {
    stop("`range` should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1)).")
  }

  out <- equivalence_test(as.data.frame(x), range = range, ci = ci, verbose = verbose, ...)

  attr(out, "object_name") <- model_name
  attr(out, "data") <- x
  class(out) <- unique(c("equivalence_test", "see_equivalence_test", "equivalence_test_simulate_model", class(out)))
  out
}






# helper -------------------


#' @importFrom bayestestR equivalence_test rope_range
#' @keywords internal
.equivalence_test_frequentist <- function(x, range = "default", ci = .95, p_values = FALSE, verbose = TRUE, ...) {
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

  # adjusted p-value for tests
  if (isTRUE(p_values)) out$p <- .add_p_to_equitest(x, ci, range, out$ROPE_Equivalence)

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
      CI = ci,
      Group = np,
      stringsAsFactors = FALSE
    )

    conf_int <- as.data.frame(t(data.frame(
      CI_low = est - stderr * fac,
      CI_high = est + stderr * fac
    )))

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



#' @importFrom stats pt qnorm p.adjust
.add_p_to_equitest <- function(model, ci, range, decision) {
  tryCatch({
    fac <- stats::qnorm((1 + ci) / 2)
    interval <- ci_wald(model, ci = ci)
    se <- abs((interval$CI_high - interval$CI_low) / (2 * fac))
    est <- insight::get_parameters(model)$Estimate
    r <- range[2]
    if (any(decision == "Undecided")) se[decision == "Undecided"] <- se[decision == "Undecided"] + (r / fac)
    if (any(decision == "Rejected")) est[decision == "Rejected"] <- ifelse(est[decision == "Rejected"] < 0, est[decision == "Rejected"] + r, est[decision == "Rejected"] - r)
    stat <- abs(est / se)
    df <- degrees_of_freedom(model)
    out <- stats::p.adjust(2 * stats::pt(stat, df = df, lower.tail = FALSE), method = "fdr")
    if (any(decision == "Accepted")) out[decision == "Accepted"] <- 1
    out
  },
  error = function(e) {
    NULL
  })
}






# methods ----------------


#' @importFrom insight print_color
#' @export
print.equivalence_test_lm <- function(x, digits = 2, ...) {
  orig_x <- x
  insight::print_color("# Test for Practical Equivalence\n\n", "blue")

  .rope <- attr(x, "rope", exact = TRUE)
  cat(sprintf("  ROPE: [%.*f %.*f]\n\n", digits, .rope[1], digits, .rope[2]))

  if ("Component" %in% colnames(x)) {
    x <- x[x$Component %in% c("conditional", "count"), ]
  }

  if ("Group" %in% colnames(x)) {
    out <- split(x, x$Group)
    for (i in names(out)) {
      insight::print_color(sprintf("Group: %s\n\n", i), "red")
      .print_equitest_freq(out[[i]], digits, ...)
    }
  } else {
    .print_equitest_freq(x, digits, ...)
  }
  invisible(orig_x)
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
  if ("p" %in% colnames(x)) {
    x$p <- format_p(x$p, name = NULL)
  }

  CI <- unique(x$CI)
  keep.columns <- c("CI", "Parameter", "ROPE_Equivalence", "ROPE_Percentage", "conf.int", "p")

  x <- x[, intersect(keep.columns, colnames(x))]

  colnames(x)[which(colnames(x) == "ROPE_Equivalence")] <- "H0"
  colnames(x)[which(colnames(x) == "ROPE_Percentage")] <- "inside ROPE"

  for (i in CI) {
    xsub <- x[x$CI == i, -which(colnames(x) == "CI"), drop = FALSE]
    if ("p" %in% colnames(x)) {
      ci_col <- ncol(xsub) - 1
    } else {
      ci_col <- ncol(xsub)
    }
    colnames(xsub)[ci_col] <- sprintf("%i%% CI", round(100 * i))
    print.data.frame(xsub, digits = digits, row.names = FALSE)
    cat("\n")
  }
}

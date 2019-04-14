#' @importFrom insight print_color
#' @export
print.equivalence_test_lm <- function(x, digits = 2, ...) {
  insight::print_color("# Test for Practical Equivalence\n\n", "blue")

  .rope <- attr(x, "rope", exact = TRUE)
  cat(sprintf("  ROPE: [%.*f %.*f]\n\n", digits, .rope[1], digits, .rope[2]))

  # find the longest CI-value, so we can align the brackets in the ouput
  x$ci_low <- sprintf("%.*f", digits, x$ci_low)
  x$ci_high <- sprintf("%.*f", digits, x$ci_high)

  maxlen_low <- max(nchar(x$ci_low))
  maxlen_high <- max(nchar(x$ci_high))

  x$coverage <- sprintf("%.*f %%", digits, 100 * x$coverage)
  x$conf.int <- sprintf("[%*s %*s]", maxlen_low, x$ci_low, maxlen_high, x$ci_high)

  ci <- unique(x$ci)
  keep.columns <- c("ci", "parameter", "decision", "coverage", "conf.int")

  x <- x[, intersect(keep.columns, colnames(x))]

  colnames(x)[which(colnames(x) == "decision")] <- "H0"
  colnames(x)[which(colnames(x) == "coverage")] <- "inside ROPE"

  for (i in ci) {
    xsub <- x[x$ci == i, -which(colnames(x) == "ci"), drop = FALSE]
    colnames(xsub)[ncol(xsub)] <- sprintf("%i%% CI", round(100 * i))
    print.data.frame(xsub, digits = digits, row.names = FALSE)
    cat("\n")
  }
}


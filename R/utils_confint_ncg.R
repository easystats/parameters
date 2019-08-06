# This function is a modified version from package MBESS
# copied from https://github.com/cran/MBESS/blob/master/R/conf.limits.ncf.R
# Author: Ken Kelley
# License: GPL-3

#' @importFrom stats pf qf
#' @keywords internal
.confint_ncg <- function(F.value = NULL, conf.level = 0.95, df.1 = NULL, df.2 = NULL) {
  alpha.lower <- alpha.upper <- (1 - conf.level) / 2
  tol <- 1e-09
  Jumping.Prop <- 0.1
  FAILED <- NULL

  LL.0 <- stats::qf(p = alpha.lower * 5e-04, df1 = df.1, df2 = df.2)
  Diff <- stats::pf(q = F.value, df1 = df.1, df2 = df.2, ncp = LL.0) - (1 - alpha.lower)

  if (stats::pf(q = F.value, df1 = df.1, df2 = df.2, ncp = LL.0) < (1 - alpha.lower)) {
    FAILED <- if (stats::pf(q = F.value, df1 = df.1, df2 = df.2, ncp = 0) < 1 - alpha.lower) {
      LL.0 <- 1e-08
    }

    if (stats::pf(q = F.value, df1 = df.1, df2 = df.2, ncp = LL.0) < 1 - alpha.lower) {
      FAILED <- TRUE
    }
  }

  if (is.null(FAILED)) {
    LL.1 <- LL.2 <- LL.0

    while (Diff > tol) {
      LL.2 <- LL.1 * (1 + Jumping.Prop)
      Diff <- stats::pf(q = F.value, df1 = df.1, df2 = df.2, ncp = LL.2) - (1 - alpha.lower)
      LL.1 <- LL.2
    }

    LL.1 <- LL.2 / (1 + Jumping.Prop)
    LL.Bounds <- c(LL.1, (LL.1 + LL.2) / 2, LL.2)

    Diff <- stats::pf(q = F.value, df1 = df.1, df2 = df.2, ncp = LL.Bounds[2]) - (1 - alpha.lower)

    while (abs(Diff) > tol) {
      Diff.1 <- stats::pf(q = F.value, df1 = df.1, df2 = df.2, ncp = LL.Bounds[1]) - (1 - alpha.lower) > tol
      Diff.2 <- stats::pf(q = F.value, df1 = df.1, df2 = df.2, ncp = LL.Bounds[2]) - (1 - alpha.lower) > tol
      Diff.3 <- stats::pf(q = F.value, df1 = df.1, df2 = df.2, ncp = LL.Bounds[3]) - (1 - alpha.lower) > tol

      if (isTRUE(Diff.1) & isTRUE(Diff.2) & !isTRUE(Diff.3)) {
        LL.Bounds <-
          c(LL.Bounds[2], (LL.Bounds[2] + LL.Bounds[3]) / 2, LL.Bounds[3])
      }

      if (isTRUE(Diff.1) & !isTRUE(Diff.2) & !isTRUE(Diff.3)) {
        LL.Bounds <-
          c(LL.Bounds[1], (LL.Bounds[1] + LL.Bounds[2]) / 2, LL.Bounds[2])
      }

      Diff <- stats::pf(q = F.value, df1 = df.1, df2 = df.2, ncp = LL.Bounds[2]) - (1 - alpha.lower)
    }

    LL <- LL.Bounds[2]
  }

  if (!is.null(FAILED)) LL <- NA


  FAILED.Up <- NULL

  UL.0 <- stats::qf(p = 1 - alpha.upper * 5e-04, df1 = df.1, df2 = df.2)
  Diff <- stats::pf(q = F.value, df1 = df.1, df2 = df.2, ncp = UL.0) - alpha.upper

  if (Diff < 0) UL.0 <- 1e-08

  Diff <- stats::pf(q = F.value, df1 = df.1, df2 = df.2, ncp = UL.0) - alpha.upper

  if (Diff < 0) FAILED.Up <- TRUE

  if (is.null(FAILED.Up)) {
    UL.1 <- UL.2 <- UL.0

    while (Diff > tol) {
      UL.2 <- UL.1 * (1 + Jumping.Prop)
      Diff <- stats::pf(q = F.value, df1 = df.1, df2 = df.2, ncp = UL.2) - alpha.upper
      UL.1 <- UL.2
    }

    UL.1 <- UL.2 / (1 + Jumping.Prop)
    UL.Bounds <- c(UL.1, (UL.1 + UL.2) / 2, UL.2)

    Diff <- stats::pf(q = F.value, df1 = df.1, df2 = df.2, ncp = UL.Bounds[2]) - alpha.upper

    while (abs(Diff) > tol) {
      Diff.1 <- stats::pf(q = F.value, df1 = df.1, df2 = df.2, ncp = UL.Bounds[1]) - alpha.upper > tol
      Diff.2 <- stats::pf(q = F.value, df1 = df.1, df2 = df.2, ncp = UL.Bounds[2]) - alpha.upper > tol
      Diff.3 <- stats::pf(q = F.value, df1 = df.1, df2 = df.2, ncp = UL.Bounds[3]) - alpha.upper > tol

      if (isTRUE(Diff.1) & isTRUE(Diff.2) & !isTRUE(Diff.3)) {
        UL.Bounds <-
          c(UL.Bounds[2], (UL.Bounds[2] + UL.Bounds[3]) / 2, UL.Bounds[3])
      }

      if (isTRUE(Diff.1) & !isTRUE(Diff.2) & !isTRUE(Diff.3)) {
        UL.Bounds <- c(UL.Bounds[1], (UL.Bounds[1] +
          UL.Bounds[2]) / 2, UL.Bounds[2])
      }

      Diff <- stats::pf(q = F.value, df1 = df.1, df2 = df.2, ncp = UL.Bounds[2]) - alpha.upper
    }

    UL <- UL.Bounds[2]
  }

  if (!is.null(FAILED.Up)) UL <- NA


  list(Lower.Limit = LL, Upper.Limit = UL)
}

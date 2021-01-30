
.extract_random_parameters <- function(model, ...) {
  UseMethod(".extract_random_parameters")
}


.extract_random_parameters.merMod <- function(model, ci = .95, effects = "random", ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' required. Please install it.", call. = FALSE)
  }
  out <- as.data.frame(lme4::ranef(model, condVar = TRUE), stringsAsFactors = FALSE)
  colnames(out) <- c('Group', 'Parameter', 'Level', 'Coefficient', 'SE')
  out$Parameter <- paste0(out$Parameter, " [", out$Level, "]")

  if (length(ci) == 1) {
    fac <- stats::qnorm((1 + ci) / 2)
    out$CI_low <- out$Coefficient - fac * out$SE
    out$CI_high <- out$Coefficient + fac * out$SE
    ci_cols <- c("CI_low", "CI_high")
  } else {
    ci_cols <- c()
    for (i in ci) {
      fac <- stats::qnorm((1 + i) / 2)
      ci_low <- paste0("CI_low_", i)
      ci_high <- paste0("CI_high_", i)
      out[[ci_low]] <- out$Coefficient - fac * out$SE
      out[[ci_high]] <- out$Coefficient + fac * out$SE
      ci_cols <- c(ci_cols, ci_low, ci_high)
    }
  }

  stat_column <- gsub("-statistic", "", insight::find_statistic(model), fixed = TRUE)

  # to match rbind
  out[[stat_column]] <- NA
  out$df_error <- NA
  out$p <- NA
  out$Effects <- "random"

  out <- out[c("Parameter", "Coefficient", "SE", ci_cols, stat_column, "df_error", "p", "Effects", "Group")]

  if (effects == "random") {
    out[c(stat_column, "df_error", "p", "Effects")] <- NULL
  }
  out
}
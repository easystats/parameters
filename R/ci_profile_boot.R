#' @importFrom insight get_parameters
#' @importFrom stats confint
.ci_profiled <- function(model, ci) {
  out <- as.data.frame(stats::confint(model, level = ci), stringsAsFactors = FALSE)
  names(out) <- c("CI_low", "CI_high")

  out$CI <- ci * 100
  ## TODO change to "$Parameter" once fixed in insight
  out$Parameter <- insight::get_parameters(model, effects = "fixed", component = "conditional")[[1]]

  out <- out[c("Parameter", "CI", "CI_low", "CI_high")]
  rownames(out) <- NULL

  out
}



# we need this function for models where confint and get_parameters return
# different length (e.g. as for "polr" models)
#' @importFrom stats confint
.ci_profiled2 <- function(model, ci) {
  out <- as.data.frame(stats::confint(model, level = ci), stringsAsFactors = FALSE)
  names(out) <- c("CI_low", "CI_high")

  out$CI <- ci * 100
  out$Parameter <- rownames(out)

  out <- out[c("Parameter", "CI", "CI_low", "CI_high")]
  rownames(out) <- NULL

  out
}


#' @keywords internal
.ci_boot_merMod <- function(x, ci, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' required for this function to work. Please install it by running `install.packages('lme4')`.")
  }

  # Compute
  out <- as.data.frame(lme4::confint.merMod(x, level = ci, method = "boot", ...))
  rownames(out) <- gsub("`", "", rownames(out), fixed = TRUE)
  out <- out[rownames(out) %in% insight::find_parameters(x)$conditional, ]
  names(out) <- c("CI_low", "CI_high")

  # Clean up
  out$Parameter <- row.names(out)
  out$CI <- ci
  out <- out[c("Parameter", "CI", "CI_low", "CI_high")]
  row.names(out) <- NULL
  out
}

#' @importFrom insight get_parameters
#' @importFrom stats confint
.ci_profiled <- function(model, ci) {
  glm_ci <- tryCatch(
    {
      out <- as.data.frame(stats::confint(model, level = ci), stringsAsFactors = FALSE)
      names(out) <- c("CI_low", "CI_high")

      out$CI <- ci * 100
      out$Parameter <- insight::get_parameters(model, effects = "fixed", component = "conditional")$Parameter

      out <- out[c("Parameter", "CI", "CI_low", "CI_high")]
      rownames(out) <- NULL

      out
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(glm_ci)) {
    glm_ci <- ci_wald(model, ci = ci)
  }

  glm_ci
}



# we need this function for models where confint and get_parameters return
# different length (e.g. as for "polr" models)
#' @importFrom stats confint
.ci_profiled2 <- function(model, ci) {
  glm_ci <- tryCatch(
    {
      out <- as.data.frame(stats::confint(model, level = ci), stringsAsFactors = FALSE)
      names(out) <- c("CI_low", "CI_high")

      out$CI <- ci * 100
      out$Parameter <- .remove_backticks_from_string(rownames(out))

      out <- out[c("Parameter", "CI", "CI_low", "CI_high")]
      rownames(out) <- NULL

      out
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(glm_ci)) {
    glm_ci <- ci_wald(model, ci = ci)
  }

  glm_ci
}


#' @importFrom stats confint
#' @importFrom insight find_parameters
#' @keywords internal
.ci_profile_merMod <- function(x, ci, profiled, ...) {
  out <- as.data.frame(stats::confint(profiled, level = ci, ...))
  rownames(out) <- gsub("`", "", rownames(out), fixed = TRUE)
  out <- out[rownames(out) %in% insight::find_parameters(x, effects = "fixed")$conditional, ]
  names(out) <- c("CI_low", "CI_high")

  # Clean up
  out$Parameter <- row.names(out)
  out$CI <- ci
  out <- out[c("Parameter", "CI", "CI_low", "CI_high")]
  row.names(out) <- NULL
  out
}


#' @importFrom stats confint
#' @keywords internal
.ci_profile_glmmTMB <- function(x, ci, profiled, component, ...) {
  out <- as.data.frame(stats::confint(profiled, level = ci, ...))
  .process_glmmTMB_CI(x, out, ci, component)
}



#' @importFrom stats confint
#' @keywords internal
.ci_uniroot_glmmTMB <- function(x, ci, component, ...) {
  out <- as.data.frame(stats::confint(x, level = ci, method = "uniroot", ...))
  .process_glmmTMB_CI(x, out, ci, component)
}



#' @importFrom insight get_parameters
.process_glmmTMB_CI <- function(x, out, ci, component) {
  rownames(out) <- gsub("`", "", rownames(out), fixed = TRUE)

  pars <- insight::get_parameters(x, effects = "fixed", component = component)
  param_names <- switch(
    component,
    "conditional" = pars$Parameter,
    "zi" = ,
    "zero_inflated" = paste0("zi~", pars$Parameter),
    c(pars$Parameter[pars$Component == "conditional"],
      paste0("zi~", pars$Parameter[pars$Component == "zero_inflated"]))
  )

  out <- out[rownames(out) %in% param_names, ]
  names(out) <- c("CI_low", "CI_high")

  # Clean up
  out$Parameter <- pars$Parameter
  out$CI <- ci
  out <- out[c("Parameter", "CI", "CI_low", "CI_high")]
  out$Component <- pars$Component
  row.names(out) <- NULL
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
  out <- out[rownames(out) %in% insight::find_parameters(x, effects = "fixed")$conditional, ]
  names(out) <- c("CI_low", "CI_high")

  # Clean up
  out$Parameter <- row.names(out)
  out$CI <- ci
  out <- out[c("Parameter", "CI", "CI_low", "CI_high")]
  row.names(out) <- NULL
  out
}

#' @title Summary information from random effects
#' @name random_parameters
#'
#' @description This function extracts the different variance components of a
#'   mixed model and returns the result as a data frame.
#'
#' @param model A mixed effects model (including `stanreg` models).
#' @param component Should all parameters, parameters for the conditional model,
#'   for the zero-inflation part of the model, or the dispersion model be returned?
#'   Applies to models with zero-inflation and/or dispersion component. `component`
#'   may be one of `"conditional"`, `"zi"`, `"zero-inflated"`, `"dispersion"` or
#'    `"all"` (default). May be abbreviated.
#'
#' @return A data frame with random effects statistics for the variance components,
#'   including number of levels per random effect group, as well as complete
#'   observations in the model.
#'
#' @details
#' The variance components are obtained from [`insight::get_variance()`] and
#' are denoted as following:
#'
#' ## Within-group (or residual) variance
#' The residual variance, \ifelse{html}{\out{&sigma;<sup>2</sup><sub>&epsilon;</sub>}}{\eqn{\sigma^2_\epsilon}},
#' is the sum of the distribution-specific variance and the variance due to additive dispersion.
#' It indicates the *within-group variance*.
#'
#' ## Between-group random intercept variance
#' The random intercept variance, or *between-group* variance
#' for the intercept (\ifelse{html}{\out{&tau;<sub>00</sub>}}{\eqn{\tau_{00}}}),
#' is obtained from `VarCorr()`. It indicates how much groups
#' or subjects differ from each other.
#'
#' ## Between-group random slope variance
#' The random slope variance, or *between-group* variance
#' for the slopes (\ifelse{html}{\out{&tau;<sub>11</sub>}}{\eqn{\tau_{11}}})
#' is obtained from `VarCorr()`. This measure is only available
#' for mixed models with random slopes. It indicates how much groups
#' or subjects differ from each other according to their slopes.
#'
#' ## Random slope-intercept correlation
#' The random slope-intercept correlation
#' (\ifelse{html}{\out{&rho;<sub>01</sub>}}{\eqn{\rho_{01}}})
#' is obtained from `VarCorr()`. This measure is only available
#' for mixed models with random intercepts and slopes.
#'
#' **Note:** For the within-group and between-group variance, variance
#' and standard deviations (which are simply the square root of the variance)
#' are shown.
#'
#' @examples
#' if (require("lme4")) {
#'   data(sleepstudy)
#'   model <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
#'   random_parameters(model)
#' }
#' @export
random_parameters <- function(model, component = "conditional") {
  component <- match.arg(component, choices = c("conditional", "zi", "zero_inflated"))
  out <- .randomeffects_summary(model, component)
  class(out) <- c("parameters_random", class(out))
  out
}


# helper -----------------------------------

.n_randomeffects <- function(model) {
  vapply(
    insight::get_data(model, verbose = FALSE)[insight::find_random(model, split_nested = TRUE, flatten = TRUE)],
    insight::n_unique,
    numeric(1)
  )
}


.randomeffects_summary <- function(model, component = "conditional") {
  out <- list()

  re_variances <- suppressWarnings(insight::get_variance(model, model_component = component))
  model_re <- insight::find_random(model, split_nested = FALSE, flatten = TRUE)
  model_rs <- unlist(insight::find_random_slopes(model))

  if (length(re_variances) && sum(!is.na(re_variances)) > 0 && !is.null(re_variances)) {
    # Residual Variance (Sigma^2)
    out$Sigma2 <- re_variances$var.residual

    # Random Intercept Variance
    if (!insight::is_empty_object(re_variances$var.intercept)) {
      var_intercept <- as.list(re_variances$var.intercept)
      names(var_intercept) <- paste0("tau00_", names(re_variances$var.intercept))
      out <- c(out, var_intercept)
    }

    # Random Slope Variance
    if (!insight::is_empty_object(re_variances$var.slope) && !insight::is_empty_object(model_rs)) {
      var_slope <- as.list(re_variances$var.slope)
      names(var_slope) <- paste0("tau11_", names(re_variances$var.slope))
      out <- c(out, var_slope)
    }

    # Slope-Intercept Correlation
    if (!insight::is_empty_object(re_variances$cor.slope_intercept) && !insight::is_empty_object(model_rs)) {
      cor_slope_intercept <- as.list(re_variances$cor.slope_intercept)
      csi_names <- gsub("(.*)(\\.\\d)(.*)", "\\1\\3", names(re_variances$var.slope))
      # csi_names <- names(re_variances$var.slope)
      names(cor_slope_intercept) <- paste0("rho01_", csi_names)
      out <- c(out, cor_slope_intercept)
    }

    # Slopes Correlation
    if (!insight::is_empty_object(re_variances$cor.slopes) && !insight::is_empty_object(model_rs)) {
      cor_slopes <- as.list(re_variances$cor.slopes)
      names(cor_slopes) <- paste0("rho00_", names(cor_slopes))
      out <- c(out, cor_slopes)
    }
  }

  # Number of levels per random-effect groups
  n_re <- as.list(.n_randomeffects(model))
  if (insight::is_empty_object(n_re)) {
    n_re <- stats::setNames(NA_real_, "N")
  } else {
    names(n_re) <- paste0("N_", names(n_re))
    out <- c(out, n_re)
  }

  # number of observations
  out$Observations <- insight::n_obs(model)

  # make nice data frame
  out <- as.data.frame(do.call(rbind, out), stringsAsFactors = FALSE)
  out$Description <- rownames(out)
  rownames(out) <- NULL
  colnames(out) <- c("Value", "Description")

  # Additional information
  out$Component <- ""
  out$Component[out$Description == "Sigma2"] <- "sigma2"
  out$Component[startsWith(out$Description, "tau00_")] <- "tau00"
  out$Component[startsWith(out$Description, "tau11_")] <- "tau11"
  out$Component[startsWith(out$Description, "rho01_")] <- "rho01"
  out$Component[startsWith(out$Description, "rho00_")] <- "rho00"

  # Additional information
  out$Term <- ""
  out$Term[out$Component == "tau00"] <- gsub("^tau00_(.*)", "\\1", out$Description[out$Component == "tau00"])
  out$Term[out$Component == "tau11"] <- gsub("^tau11_(.*)", "\\1", out$Description[out$Component == "tau11"])
  out$Term[out$Component == "rho01"] <- gsub("^rho01_(.*)", "\\1", out$Description[out$Component == "rho01"])
  out$Term[out$Component == "rho00"] <- gsub("^rho00_(.*)(\\.\\.\\.)(.*)", "\\3", out$Description[out$Component == "rho00"])

  # renaming
  out$Type <- ""

  # Within-Group Variance
  out$Type[out$Description == "Sigma2"] <- ""
  out$Description[out$Description == "Sigma2"] <- "Within-Group Variance"

  # Between-Group Variance
  out$Type[startsWith(out$Description, "tau00_")] <- "Random Intercept"
  out$Description <- gsub("^tau00_(.*)", "Between-Group Variance", out$Description)
  out$Type[startsWith(out$Description, "tau11_")] <- "Random Slope"
  out$Description <- gsub("^tau11_(.*)", "Between-Group Variance", out$Description)

  # correlations
  out$Type[startsWith(out$Description, "rho01_")] <- ""
  out$Description <- gsub("^rho01_(.*)", "Correlations", out$Description)
  out$Type[startsWith(out$Description, "rho00_")] <- ""
  out$Description <- gsub("^rho00_(.*)", "Correlations", out$Description)

  out$Type[grepl("N_(.*)", out$Description)] <- ""
  out$Term[grepl("N_(.*)", out$Description)] <- gsub("N_(.*)", "\\1", grep("N_(.*)", out$Description, value = TRUE))
  out$Description <- gsub("_(.*)", "", out$Description)

  out$Type[startsWith(out$Description, "X")] <- ""
  out$Description[startsWith(out$Description, "X")] <- NA
  out$Component[out$Component == ""] <- NA
  out$Term[out$Term == ""] <- NA

  out[c("Description", "Component", "Type", "Term", "Value")]
}

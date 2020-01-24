#' @title Summary information from random effects
#' @name random_parameters
#'
#' @description This function extracts the different variance components of a
#'   mixed model as well as further information related to random effects (ICC, R2)
#'   and returns the result as a data frame.
#'
#' @param model A mixed effects model.
#'
#' @return A data frame with random effects statistics, including number of levels per
#'   random effect group, as well as complete observations in the model.
#'
#' @details The variance components are obtained from \code{\link[insight]{get_variance}}
#'   and are denoted as following:
#'   \subsection{Within-subject (or residual) variance}{
#'     The residual variance, \ifelse{html}{\out{&sigma;<sup>2</sup><sub>&epsilon;</sub>}}{\eqn{\sigma^2_\epsilon}},
#'     is the sum of the distribution-specific variance and the variance due to additive dispersion.
#'     It indicates the \emph{within-subject variance}.
#'   }
#'   \subsection{Between-subject (or random intercept) variance}{
#'     The random intercept variance, or \emph{between-subject} variance
#'     (\ifelse{html}{\out{&tau;<sub>00</sub>}}{\eqn{\tau_{00}}}),
#'     is obtained from \code{VarCorr()}. It indicates how much groups
#'     or subjects differ from each other.
#'   }
#'   \subsection{Random slope variance}{
#'     The random slope variance (\ifelse{html}{\out{&tau;<sub>11</sub>}}{\eqn{\tau_{11}}})
#'     is obtained from \code{VarCorr()}. This measure is only available
#'     for mixed models with random slopes.
#'   }
#'   \subsection{Random slope-intercept correlation}{
#'     The random slope-intercept correlation
#'     (\ifelse{html}{\out{&rho;<sub>01</sub>}}{\eqn{\rho_{01}}})
#'     is obtained from \code{VarCorr()}. This measure is only available
#'     for mixed models with random intercepts and slopes.
#'   }
#'  For details regarding the calculation of R2 and ICC, see \code{\link[performance]{r2_nakagawa}}
#'  and \code{\link[performance]{icc}}.
#'
#' @examples
#' if (require("lme4")) {
#'   data(sleepstudy)
#'   model <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
#'   random_parameters(model)
#' }
#' @export
random_parameters <- function(model) {
  out <- .randomeffects_summary(model)
  class(out) <- c("parameters_random", class(out))
  out
}



# helper -----------------------------------

.n_randomeffects <- function(model) {
  sapply(insight::get_data(model)[insight::find_random(model, split_nested = TRUE, flatten = TRUE)], function(i) length(unique(i, na.rm = TRUE)))
}



#' @importFrom insight find_random get_variance find_random_slopes n_obs
.randomeffects_summary <- function(model) {
  out <- list()

  re_variances <- suppressWarnings(insight::get_variance(model))
  model_re <- insight::find_random(model, split_nested = FALSE, flatten = TRUE)
  model_rs <- unlist(insight::find_random_slopes(model))

  new_line <- FALSE

  if (length(re_variances) && !is.na(re_variances) && !is.null(re_variances)) {
    # Residual Variance (Sigma^2)
    out$Sigma2 <- re_variances$var.residual

    # Random Intercept Variance
    var_intercept <- as.list(re_variances$var.intercept)
    names(var_intercept) <- paste0("tau00_", names(re_variances$var.intercept))
    out <- c(out, var_intercept)

    # Random Slope Variance
    if (!.is_empty_object(re_variances$var.slope) && !.is_empty_object(model_rs)) {
      out <- c(out, as.list(NA))
      new_line <- TRUE
      var_slope <- as.list(re_variances$var.slope)
      names(var_slope) <- paste0("tau11_", names(re_variances$var.slope))
      out <- c(out, var_slope)
    }

    # Slope-Intercept Correlation
    if (!.is_empty_object(re_variances$cor.slope_intercept) && !.is_empty_object(model_rs)) {
      if (!new_line) {
        out <- c(out, as.list(NA))
        new_line <- TRUE
      }
      cor_slope_intercept <- as.list(re_variances$cor.slope_intercept)
      names(cor_slope_intercept) <- paste0("rho01_", model_re, ".", model_rs)
      out <- c(out, cor_slope_intercept)
    }

    out <- c(out, as.list(NA))

    # ICC & R2
    out$R2_marginal <- re_variances$var.fixed / (re_variances$var.fixed + re_variances$var.residual)
    if (!.is_empty_object(re_variances$var.random) && !is.na(re_variances$var.random)) {
      out$R2_conditional <- (re_variances$var.fixed + re_variances$var.random) / (re_variances$var.fixed + re_variances$var.random + re_variances$var.residual)
      out$ICC <- re_variances$var.random / (re_variances$var.random + re_variances$var.residual)
    }
    out <- c(out, as.list(NA))
  }

  # Number of levels per random-effect groups
  n_re <- as.list(.n_randomeffects(model))
  names(n_re) <- paste0("N_", names(n_re))
  out <- c(out, n_re)

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
  out$Component[grepl("^tau00_", out$Description)] <- "tau00"
  out$Component[grepl("^tau11_", out$Description)] <- "tau11"
  out$Component[grepl("^rho01_", out$Description)] <- "rho01"

  # Additional information
  out$Term <- ""
  out$Term[out$Component == "tau00"] <- gsub("^tau00_(.*)", "\\1", out$Description[out$Component == "tau00"])
  out$Term[out$Component == "tau11"] <- gsub("^tau11_(.*)", "\\1", out$Description[out$Component == "tau11"])
  out$Term[out$Component == "rho01"] <- gsub("^rho01_(.*)", "\\1", out$Description[out$Component == "rho01"])

  # renaming
  out$Description[out$Description == "Sigma2"] <- "Within-Subject Variance"
  out$Description <- gsub("^tau00_(.*)", "Between-Subject Variance", out$Description)
  out$Description <- gsub("^tau11_(.*)", "Random Slope Variance", out$Description)
  out$Description <- gsub("^rho01_(.*)", "Slope-Intercept Correlation", out$Description)

  out$Term[grepl("N_(.*)", out$Description)] <- gsub("N_(.*)", "\\1", out$Description[grepl("N_(.*)", out$Description)])
  out$Description <- gsub("R2_(.*)", "R2 \\(\\1\\)", out$Description)
  out$Description <- gsub("_(.*)", "", out$Description)

  out$Description[grepl("^X", out$Description)] <- NA
  out$Component[out$Component == ""] <- NA
  out$Term[out$Term == ""] <- NA

  out[c("Description", "Component", "Term", "Value")]
}

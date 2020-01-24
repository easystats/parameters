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

  if (length(re_variances) && !is.na(re_variances) && !is.null(re_variances)) {
    # Residual Variance (Sigma^2)
    out$Sigma2 <- re_variances$var.residual
    out <- c(out, as.list(NA))

    # Random Intercept Variance
    var_intercept <- as.list(re_variances$var.intercept)
    names(var_intercept) <- paste0("tau00_", names(re_variances$var.intercept))
    out <- c(out, var_intercept)

    # Random Slope Variance
    if (!.is_empty_object(re_variances$var.slope) && !.is_empty_object(model_rs)) {
      var_slope <- as.list(re_variances$var.slope)
      names(var_slope) <- paste0("tau11_", names(re_variances$var.slope))
      out <- c(out, var_slope)
      out <- c(out, as.list(NA))
    } else {
      out <- c(out, as.list(NA))
    }

    # Slope-Intercept Correlation
    if (!.is_empty_object(re_variances$cor.slope_intercept) && !.is_empty_object(model_rs)) {
      cor_slope_intercept <- as.list(re_variances$cor.slope_intercept)
      names(cor_slope_intercept) <- paste0("rho01_", model_re, ".", model_rs)
      out <- c(out, cor_slope_intercept)
      out <- c(out, as.list(NA))
    }

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
  out$Statistic <- rownames(out)
  rownames(out) <- NULL
  colnames(out) <- c("Value", "Statistic")

  out[c(2:1)]
}


.extract_random_variances <- function(model, ...) {
  UseMethod(".extract_random_variances")
}


.extract_random_variances.default <- function(model,
                                              ci = .95,
                                              effects = "random",
                                              component = "conditional",
                                              ci_method = NULL,
                                              ...) {
  suppressWarnings(
    .extract_random_variances_helper(
      model,
      ci = ci,
      effects = effects,
      component = component,
      ci_method = ci_method,
      ...
    )
  )
}







.extract_random_variances.glmmTMB <- function(model,
                                              ci = .95,
                                              effects = "random",
                                              component = "all",
                                              ci_method = NULL,
                                              ...) {
  component <- match.arg(component, choices = c("all", "conditional", "zero_inflated", "zi", "dispersion"))

  out <- suppressWarnings(
    .extract_random_variances_helper(
      model,
      ci = ci,
      effects = effects,
      component = "conditional",
      ci_method = ci_method,
      ...
    )
  )
  out$Component <- "conditional"

  if (insight::model_info(model)$is_zero_inflated) {
    zi_var <- suppressWarnings(
      .extract_random_variances_helper(
        model,
        ci = ci,
        effects = effects,
        component = "zi",
        ci_method = ci_method,
        ...
      )
    )
    zi_var$Component <- "zero_inflated"
    out <- rbind(out, zi_var)
  }

  # filter
  if (component != "all") {
    if (component == "zi") {
      component <- "zero_inflated"
    }
    out <- out[out$Component == component, ]
  }

  out
}


.extract_random_variances.MixMod <- .extract_random_variances.glmmTMB







# workhorse ------------------------


.extract_random_variances_helper <- function(model,
                                             ci = .95,
                                             effects = "random",
                                             component = "conditional",
                                             ci_method = NULL, ...) {
  ran_intercept <- tryCatch(
    {
      data.frame(
        insight::get_variance(
          model,
          component = "intercept",
          verbose = FALSE,
          model_component = component
        )
      )
    },
    error = function(e) {
      NULL
    }
  )

  ran_slope <- tryCatch(
    {
      data.frame(
        insight::get_variance(
          model,
          component = "slope",
          verbose = FALSE,
          model_component = component
        )
      )
    },
    error = function(e) {
      NULL
    }
  )

  ran_corr <- tryCatch(
    {
      data.frame(
        insight::get_variance(
          model,
          component = "rho01",
          verbose = FALSE,
          model_component = component
        )
      )
    },
    error = function(e) {
      NULL
    }
  )

  ran_sigma <- data.frame(insight::get_sigma(model, ci = NULL, verbose = FALSE))

  # random intercept - tau00
  if (!is.null(ran_intercept) && nrow(ran_intercept) > 0) {
    colnames(ran_intercept) <- "Coefficient"
    ran_intercept$Group <- rownames(ran_intercept)
    ran_intercept$Parameter <- "SD (Intercept)"
  }

  # random slope - tau11
  if (!is.null(ran_slope) && nrow(ran_slope) > 0) {
    colnames(ran_slope) <- "Coefficient"
    ran_slope$Group <- rownames(ran_slope)
    for (i in unique(ran_intercept$Group)) {
      slopes <- which(grepl(paste0("^\\Q", i, "\\E"), ran_slope$Group))
      if (length(slopes)) {
        ran_slope$Parameter[slopes] <- paste0(
          "SD (", gsub("^\\.", "", gsub(i, "", ran_slope$Group[slopes], fixed = TRUE)), ")"
        )
        ran_slope$Group[slopes] <- i
      }
    }
  }

  # random slope-intercept correlation - rho01
  if (!is.null(ran_corr) && nrow(ran_corr) > 0) {
    if (colnames(ran_corr)[1] == ran_intercept$Group[1]) {
      colnames(ran_corr)[1] <- "Coefficient"
      ran_corr$Parameter <- paste0("Cor (Intercept~", row.names(ran_corr), ")")
      ran_corr$Group <- ran_intercept$Group[1]
    } else {
      colnames(ran_corr) <- "Coefficient"
      ran_corr$Group <- rownames(ran_corr)
      for (i in unique(ran_intercept$Group)) {
        corrs <- which(grepl(paste0("^\\Q", i, "\\E"), ran_corr$Group))
        if (length(corrs)) {
          param_name <- i
          cor_slopes <- which(grepl(paste0("^\\Q", i, "\\E"), ran_slope$Group))
          if (length(cor_slopes)) {
            param_name <- paste0(
              gsub("SD \\((.*)\\)", "\\1", ran_slope$Parameter[cor_slopes]),
              ": ",
              i
            )
          }
          ran_corr$Parameter[corrs] <- paste0("Cor (Intercept~", param_name, ")")
          ran_corr$Group[corrs] <- i
        }
      }
    }
  }

  # residuals - sigma
  if (nrow(ran_sigma) > 0) {
    colnames(ran_sigma) <- "Coefficient"
    ran_sigma$Group <- "Residual"
    ran_sigma$Parameter <- "SD (Observations)"
  }

  out <- rbind(ran_intercept, ran_slope, ran_corr, ran_sigma)
  rownames(out) <- NULL

  # variances to SD (sqrt), except correlations and Sigma
  corr_param <- grepl("Cor (Intercept~", out$Parameter, fixed = TRUE)
  sigma_param <- out$Parameter == "SD (Observations)"
  out$Coefficient[!corr_param & !sigma_param] <- sqrt(out$Coefficient[!corr_param])

  stat_column <- gsub("-statistic", "", insight::find_statistic(model), fixed = TRUE)

  # to match rbind
  out[[stat_column]] <- NA
  out$SE <- NA
  out$df_error <- NA
  out$p <- NA
  out$Level <- NA
  out$CI <- NA

  out$Effects <- "random"

  if (length(ci) == 1) {
    ci_cols <- c("CI_low", "CI_high")
  } else {
    ci_cols <- c()
    for (i in ci) {
      ci_low <- paste0("CI_low_", i)
      ci_high <- paste0("CI_high_", i)
      ci_cols <- c(ci_cols, ci_low, ci_high)
    }
  }
  out[ci_cols] <- NA

  # add confidence intervals?
  if (!is.null(ci) && !all(is.na(ci)) && length(ci) == 1) {
    out <- .random_sd_ci(model, out, ci_method, ci, corr_param, sigma_param)
  }

  out <- out[c("Parameter", "Level", "Coefficient", "SE", ci_cols, stat_column, "df_error", "p", "Effects", "Group")]

  if (effects == "random") {
    out[c(stat_column, "df_error", "p", "CI")] <- NULL
  }

  rownames(out) <- NULL
  out
}





# extract CI for random SD ------------------------


.random_sd_ci <- function(model, out, ci_method, ci, corr_param, sigma_param) {
  add_ci <- FALSE
  if (inherits(model, c("merMod", "glmerMod", "lmerMod"))) {
    if (!is.null(ci_method) && ci_method %in% c("profile", "boot")) {
      var_ci <- as.data.frame(suppressWarnings(stats::confint(model, parm = "theta_", oldNames = FALSE, method = ci_method, level = ci)))
      colnames(var_ci) <- c("CI_low", "CI_high")

      rn <- row.names(var_ci)
      rn <- gsub("sd_(.*)(\\|)(.*)", "\\1: \\3", rn)
      rn <- gsub("|", ":", rn, fixed = TRUE)
      rn <- gsub("[\\(\\)]", "", rn)
      rn <- gsub("cor_(.*)\\.(.*)", "cor \\2", rn)

      var_ci_corr_param <- grepl("^cor ", rn)
      var_ci_sigma_param <- rn == "sigma"
      add_ci <- TRUE
    }
  } else if (inherits(model, "glmmTMB")) {

    return(out)

    ## TODO "profile" seems to be less stable, so only wald? Need to mention in docs!
    var_ci <- rbind(
      as.data.frame(suppressWarnings(stats::confint(model, parm = "theta_", method = "wald", level = ci))),
      as.data.frame(suppressWarnings(stats::confint(model, parm = "sigma", method = "wald", level = ci)))
    )
    colnames(var_ci) <- c("CI_low", "CI_high")

    rn <- row.names(var_ci)
    var_ci_corr_param <- grepl("^Cor\\.", rn)
    var_ci_sigma_param <- rn == "sigma"
    add_ci <- TRUE
  }

  # only add CI if we reliably found that information
  if (add_ci) {
    out$CI <- ci

    out$CI_low[!corr_param & !sigma_param] <- var_ci$CI_low[!var_ci_corr_param & !var_ci_sigma_param]
    out$CI_low[sigma_param] <- var_ci$CI_low[var_ci_sigma_param]
    out$CI_low[corr_param] <- var_ci$CI_low[var_ci_corr_param]

    out$CI_high[!corr_param & !sigma_param] <- var_ci$CI_high[!var_ci_corr_param & !var_ci_sigma_param]
    out$CI_high[sigma_param] <- var_ci$CI_high[var_ci_sigma_param]
    out$CI_high[corr_param] <- var_ci$CI_high[var_ci_corr_param]
  }

  out
}

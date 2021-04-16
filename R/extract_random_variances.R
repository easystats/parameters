
.extract_random_variances <- function(model, ...) {
  UseMethod(".extract_random_variances")
}


.extract_random_variances.default <- function(model,
                                              ci = .95,
                                              effects = "random",
                                              component = "conditional",
                                              ...) {
  suppressWarnings(
    .extract_random_variances_helper(
      model,
      ci = ci,
      effects = effects,
      component = component,
      ...
    )
  )
}







.extract_random_variances.glmmTMB <- function(model,
                                              ci = .95,
                                              effects = "random",
                                              component = "all",
                                              ...) {
  component <- match.arg(component, choices = c("all", "conditional", "zero_inflated", "zi", "dispersion"))

  out <- suppressWarnings(
    .extract_random_variances_helper(
      model,
      ci = ci,
      effects = effects,
      component = "conditional",
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


.extract_random_variances_helper <- function(model, ci = ci, effects = effects, component = "conditional", ...) {
  ran_intercept <- data.frame(
    insight::get_variance(
      model,
      component = "intercept",
      verbose = FALSE,
      model_component = component
    )
  )

  ran_slope <- data.frame(
    insight::get_variance(
      model,
      component = "slope",
      verbose = FALSE,
      model_component = component
    )
  )

  ran_corr <- data.frame(
    insight::get_variance(
      model,
      component = "rho01",
      verbose = FALSE,
      model_component = component
    )
  )

  ran_sigma <- data.frame(insight::get_sigma(model, verbose = FALSE))

  # random intercept - tau00
  if (nrow(ran_intercept) > 0) {
    colnames(ran_intercept) <- "Coefficient"
    ran_intercept$Group <- rownames(ran_intercept)
    ran_intercept$Parameter <- "SD (Intercept)"
  }

  # random slope - tau11
  if (nrow(ran_slope) > 0) {
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
  if (nrow(ran_corr) > 0) {
    colnames(ran_corr) <- "Coefficient"
    ran_corr$Group <- rownames(ran_corr)
    for (i in unique(ran_intercept$Group)) {
      corrs <- which(grepl(paste0("^\\Q", i, "\\E"), ran_corr$Group))
      if (length(corrs)) {
        ran_corr$Parameter[corrs] <- paste0("Cor (Intercept~", i, ")")
        ran_corr$Group[corrs] <- i
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

  # variances to SD (sqrt)
  corr_param <- grepl("Cor (Intercept~", out$Parameter, fixed = TRUE)
  out$Coefficient[!corr_param] <- sqrt(out$Coefficient[!corr_param])

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

  out <- out[c("Parameter", "Level", "Coefficient", "SE", ci_cols, stat_column, "df_error", "p", "Effects", "Group")]

  if (effects == "random") {
    out[c(stat_column, "df_error", "p", "CI")] <- NULL
  }

  rownames(out) <- NULL
  out
}

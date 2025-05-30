
.extract_random_variances <- function(model, ...) {
  UseMethod(".extract_random_variances")
}


# default method -------------------

.extract_random_variances.default <- function(model,
                                              ci = 0.95,
                                              effects = "random",
                                              component = "conditional",
                                              ci_method = NULL,
                                              verbose = FALSE,
                                              ...) {
  out <- suppressWarnings(
    .extract_random_variances_helper(
      model,
      ci = ci,
      effects = effects,
      component = component,
      ci_method = ci_method,
      verbose = verbose,
      ...
    )
  )

  # check for errors
  if (is.null(out) && isTRUE(verbose)) {
    insight::format_warning(
      "Something went wrong when calculating random effects parameters. Only showing model's fixed effects now. You may use `effects=\"fixed\"` to speed up the call to `model_parameters()`."
    )
  }

  out
}


# glmmTMB -------------------

.extract_random_variances.glmmTMB <- function(model,
                                              ci = 0.95,
                                              effects = "random",
                                              component = "all",
                                              ci_method = NULL,
                                              verbose = FALSE,
                                              ...) {
  component <- insight::validate_argument(
    component,
    c("all", "conditional", "zero_inflated", "zi", "dispersion")
  )

  out <- suppressWarnings(
    .extract_random_variances_helper(
      model,
      ci = ci,
      effects = effects,
      component = "conditional",
      ci_method = ci_method,
      verbose = verbose,
      ...
    )
  )

  # check for errors
  if (is.null(out)) {
    if (isTRUE(verbose)) {
      insight::format_warning(
        "Something went wrong when calculating random effects parameters. Only showing model's fixed effects now. You may use `effects=\"fixed\"` to speed up the call to `model_parameters()`."
      )
    }
    return(NULL)
  }

  out$Component <- "conditional"

  if (insight::model_info(model, verbose = FALSE)$is_zero_inflated &&
      !is.null(insight::find_random(model)$zero_inflated_random)) {
    zi_var <- suppressWarnings(
      .extract_random_variances_helper(
        model,
        ci = ci,
        effects = effects,
        component = "zi",
        ci_method = ci_method,
        verbose = FALSE,
        ...
      )
    )

    # bind if any zi-components could be extracted
    if (!is.null(zi_var)) {
      zi_var$Component <- "zero_inflated"
      out <- rbind(out, zi_var)
    }
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


# GLMMadpative -------------------

.extract_random_variances.MixMod <- .extract_random_variances.glmmTMB



# workhorse ------------------------

.extract_random_variances_helper <- function(model,
                                             ci = 0.95,
                                             effects = "random",
                                             component = "conditional",
                                             ci_method = NULL,
                                             verbose = FALSE,
                                             ...) {
  varcorr <- .get_variance_information(model, component)

  ran_intercept <- .safe(data.frame(.random_intercept_variance(varcorr)))
  ran_slope <- .safe(data.frame(.random_slope_variance(model, varcorr)))
  ran_corr <- .safe(data.frame(.random_slope_intercept_corr(model, varcorr)))
  ran_slopes_corr <- .safe(data.frame(.random_slopes_corr(model, varcorr)))

  # sigma/dispersion only once,
  if (component == "conditional") {
    ran_sigma <- data.frame(insight::get_sigma(model, ci = NULL, verbose = FALSE))
  } else {
    ran_sigma <- NULL
  }


  # random intercept - tau00
  if (!is.null(ran_intercept) && nrow(ran_intercept) > 0) {
    colnames(ran_intercept) <- "Coefficient"
    ran_intercept$Group <- rownames(ran_intercept)
    ran_intercept$Parameter <- "SD (Intercept)"
  }
  ran_groups_int <- ran_intercept$Group


  # random slope - tau11
  if (!is.null(ran_slope) && nrow(ran_slope) > 0) {
    colnames(ran_slope) <- "Coefficient"
    ran_slope$Group <- rownames(ran_slope)
    ran_groups_slp <- gsub("\\..*", "", ran_slope$Group)
    ran_slope$Parameter <- NA
    for (i in unique(ran_groups_slp)) {
      slopes <- which(grepl(paste0("^\\Q", i, "\\E"), ran_slope$Group))
      if (length(slopes)) {
        ran_slope$Parameter[slopes] <- paste0(
          "SD (", gsub("^\\.", "", gsub(i, "", ran_slope$Group[slopes], fixed = TRUE)), ")"
        )
        ran_slope$Group[slopes] <- i
      }
    }
  } else {
    ran_groups_slp <- NULL
  }

  ran_groups <- unique(c(ran_groups_int, ran_groups_slp))


  # random slope-intercept correlation - rho01
  if (!is.null(ran_corr) && nrow(ran_corr) > 0) {
    if (ncol(ran_corr) > 1 && all(colnames(ran_corr) %in% ran_groups)) {
      ran_corr <- datawizard::reshape_longer(
        ran_corr,
        colnames_to = "Group",
        values_to = "Coefficient",
        rows_to = "Slope"
      )
      ran_corr$Parameter <- paste0("Cor (Intercept~", ran_corr$Slope, ": ", ran_intercept$Group, ")")
      ran_corr <- datawizard::data_reorder(ran_corr, select = c("Parameter", "Coefficient", "Group"))
      ran_corr$Slope <- NULL
    } else if (!is.null(ran_intercept$Group) && colnames(ran_corr)[1] == ran_intercept$Group[1]) {
      colnames(ran_corr)[1] <- "Coefficient"
      ran_corr$Parameter <- paste0("Cor (Intercept~", row.names(ran_corr), ": ", ran_intercept$Group[1], ")")
      ran_corr$Group <- ran_intercept$Group[1]
    } else if (!is.null(ran_groups) && colnames(ran_corr)[1] == ran_groups[1]) {
      # colnames(ran_corr)[1] <- "Coefficient"
      # ran_corr$Parameter <- paste0("Cor (Reference~", row.names(ran_corr), ")")
      # ran_corr$Group <- ran_groups[1]
      # this occurs when model has no random intercept
      # will be captures by random slope-slope-correlation
      ran_corr <- NULL
    } else {
      colnames(ran_corr) <- "Coefficient"
      ran_corr$Group <- rownames(ran_corr)
      ran_corr$Parameter <- NA
      for (i in unique(ran_groups)) {
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


  # random slope correlation - rho00
  if (!is.null(ran_slopes_corr) && nrow(ran_slopes_corr) > 0) {
    term <- rownames(ran_slopes_corr)
    colnames(ran_slopes_corr) <- "Coefficient"
    rg <- paste0("(", paste0(c(ran_groups, paste0(ran_groups, "\\.\\d+")), collapse = "|"), ")")
    grp <- gsub(paste0(rg, "\\.(.*)-(.*)"), "\\1", term)
    slope1 <- gsub(paste0(rg, "\\.(.*)-(.*)"), "\\2", term)
    slope2 <- gsub(paste0(rg, "\\.(.*)-(.*)"), "\\3", term)
    ran_slopes_corr$Parameter <- paste0("Cor (", slope1, "~", slope2, ": ", grp, ")")
    ran_slopes_corr$Group <- grp
  }


  # residuals - sigma
  if (!is.null(ran_sigma) && nrow(ran_sigma) > 0) {
    colnames(ran_sigma) <- "Coefficient"
    ran_sigma$Group <- "Residual"
    ran_sigma$Parameter <- "SD (Observations)"
  }

  # row bind all random effect variances, if possible
  out <- tryCatch(
    {
      out_list <- insight::compact_list(list(
        ran_intercept,
        ran_slope,
        ran_corr,
        ran_slopes_corr,
        ran_sigma
      ))
      do.call(rbind, out_list)
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(out)) {
    return(NULL)
  }

  rownames(out) <- NULL

  # variances to SD (sqrt), except correlations and Sigma
  corr_param <- grepl("^Cor (.*)", out$Parameter)
  sigma_param <- out$Parameter == "SD (Observations)"
  not_cor_and_sigma <- !corr_param & !sigma_param
  if (any(not_cor_and_sigma)) {
    out$Coefficient[not_cor_and_sigma] <- sqrt(out$Coefficient[not_cor_and_sigma])
  }

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
    out <- .random_sd_ci(
      model,
      out,
      ci_method,
      ci,
      corr_param,
      sigma_param,
      component,
      verbose = verbose
    )
  }

  out <- out[c("Parameter", "Level", "Coefficient", "SE", ci_cols, stat_column, "df_error", "p", "Effects", "Group")]

  if (effects == "random") {
    out[c(stat_column, "df_error", "p", "CI")] <- NULL
  }

  rownames(out) <- NULL
  out
}



# extract CI for random SD ------------------------

.random_sd_ci <- function(model, out, ci_method, ci, corr_param, sigma_param, component = NULL, verbose = FALSE) {
  ## TODO needs to be removed once MCM > 0.1.5 is on CRAN
  if (startsWith(insight::safe_deparse(insight::get_call(model)), "mcm_lmer")) {
    return(out)
  }

  if (inherits(model, c("merMod", "glmerMod", "lmerMod"))) {
    if (!is.null(ci_method) && ci_method %in% c("profile", "boot")) {
      var_ci <- as.data.frame(suppressWarnings(stats::confint(
        model,
        parm = "theta_",
        oldNames = FALSE,
        method = ci_method,
        level = ci
      )))
      colnames(var_ci) <- c("CI_low", "CI_high")

      rn <- row.names(var_ci)
      rn <- gsub("sd_(.*)(\\|)(.*)", "\\1: \\3", rn)
      rn <- gsub("|", ":", rn, fixed = TRUE)
      rn <- gsub("[\\(\\)]", "", rn)
      rn <- gsub("cor_(.*)\\.(.*)", "cor \\2", rn)

      var_ci_corr_param <- startsWith(rn, "cor ")
      var_ci_sigma_param <- rn == "sigma"

      out$CI_low[!corr_param & !sigma_param] <- var_ci$CI_low[!var_ci_corr_param & !var_ci_sigma_param]
      out$CI_high[!corr_param & !sigma_param] <- var_ci$CI_high[!var_ci_corr_param & !var_ci_sigma_param]

      if (any(sigma_param) && any(var_ci_sigma_param)) {
        out$CI_low[sigma_param] <- var_ci$CI_low[var_ci_sigma_param]
        out$CI_high[sigma_param] <- var_ci$CI_high[var_ci_sigma_param]
      }

      if (any(corr_param) && any(var_ci_corr_param)) {
        out$CI_low[corr_param] <- var_ci$CI_low[var_ci_corr_param]
        out$CI_high[corr_param] <- var_ci$CI_high[var_ci_corr_param]
      }
    } else if (!is.null(ci_method)) {
      # Wald based CIs
      # see https://stat.ethz.ch/pipermail/r-sig-mixed-models/2022q1/029985.html
      if (all(insight::check_if_installed(c("merDeriv", "lme4"), quietly = TRUE))) {
        # this may fail, so wrap in try-catch
        tryCatch(
          {
            # vcov from full model. the parameters from vcov have a different
            # order, so we need to restore the "original" order of random effect
            # parameters using regex to match the naming patterns (of the column
            # names from the vcov)
            vv <- stats::vcov(model, full = TRUE, ranpar = "sd")

            # only keep random effect variances
            cov_columns <- grepl("(^cov_|residual)", colnames(vv))
            vv <- vv[cov_columns, cov_columns, drop = FALSE]

            # iterate random effect variables
            re_groups <- setdiff(unique(out$Group), "Residual")
            # create data frame with group and parameter names and SE
            var_ci <- do.call(rbind, lapply(re_groups, function(i) {
              pattern <- paste0("^cov_", i, "\\.(.*)")
              re_group_columns <- grepl(pattern, colnames(vv))
              vv_sub <- as.matrix(vv[re_group_columns, re_group_columns, drop = FALSE])
              cn <- gsub(pattern, "\\1", colnames(vv_sub))
              .data_frame(Group = i, Parameter = cn, SE = sqrt(diag(vv_sub)))
            }))

            # add residual variance
            res_column <- which(colnames(vv) == "residual")
            if (length(res_column)) {
              var_ci <- rbind(
                var_ci,
                .data_frame(
                  Group = "Residual",
                  Parameter = "SD (Observations)",
                  SE = sqrt(vv[res_column, res_column, drop = TRUE])
                )
              )
            }
            # renaming
            var_ci$Parameter[var_ci$Parameter == "(Intercept)"] <- "SD (Intercept)"
            # correlations
            var_ci_corr_param <- grepl("(.*)\\.\\(Intercept\\)", var_ci$Parameter)
            if (any(var_ci_corr_param)) {
              rnd_slope_terms <- gsub("(.*)\\.\\(Intercept\\)", "\\1", var_ci$Parameter[var_ci_corr_param])
              var_ci$Parameter[var_ci_corr_param] <- paste0(
                "Cor (Intercept~",
                rnd_slope_terms,
                ": ",
                var_ci$Group[var_ci_corr_param],
                ")"
              )
            }

            # correlations w/o intercept? usually only for factors
            # or: correlation among slopes. we need to recover the (categorical)
            # term names from our prepared data frame, then match vcov-names
            rnd_slope_corr <- grepl("^Cor \\((?!Intercept~)", out$Parameter, perl = TRUE)
            if (any(rnd_slope_corr)) {
              for (gr in setdiff(unique(out$Group), "Residual")) {
                rnd_slope_corr_grp <- rnd_slope_corr & out$Group == gr
                dummy <- gsub("Cor \\((.*)~(.*): (.*)\\)", "\\2.\\1", out$Parameter[rnd_slope_corr_grp])
                var_ci$Parameter[var_ci$Group == gr][match(dummy, var_ci$Parameter[var_ci$Group == gr])] <- out$Parameter[rnd_slope_corr_grp]
              }
            }

            # remaining
            var_ci_others <- !grepl("^(Cor|SD) (.*)", var_ci$Parameter)
            var_ci$Parameter[var_ci_others] <- gsub("(.*)", "SD (\\1)", var_ci$Parameter[var_ci_others])

            # merge with random effect coefficients
            out$.sort_id <- seq_len(nrow(out))
            tmp <- merge(
              datawizard::data_remove(out, "SE", verbose = FALSE),
              var_ci,
              all.x = TRUE,
              sort = FALSE
            )
            tmp <- tmp[order(tmp$.sort_id), ]
            out$SE <- tmp$SE
            out$.sort_id <- NULL

            # ensure correlation CI are within -1/1 bounds
            var_ci_corr_param <- startsWith(out$Parameter, "Cor ")
            if (any(var_ci_corr_param)) {
              coefs <- out$Coefficient[var_ci_corr_param]
              delta_se <- out$SE[var_ci_corr_param] / (1 - coefs^2)
              out$CI_low[var_ci_corr_param] <- tanh(atanh(coefs) - stats::qnorm(0.975) * delta_se)
              out$CI_high[var_ci_corr_param] <- tanh(atanh(coefs) + stats::qnorm(0.975) * delta_se)
            }

            # Wald CI, based on delta-method.
            # SD is chi square distributed. So it has a long tail. CIs should
            # therefore be asymmetrical. log(SD) is normally distributed.
            # Also, if the SD is small, then the CI might go negative
            coefs <- out$Coefficient[!var_ci_corr_param]
            delta_se <- out$SE[!var_ci_corr_param] / coefs
            out$CI_low[!var_ci_corr_param] <- exp(log(coefs) - stats::qnorm(0.975) * delta_se)
            out$CI_high[!var_ci_corr_param] <- exp(log(coefs) + stats::qnorm(0.975) * delta_se)

            # warn if singular fit
            if (isTRUE(verbose) && insight::check_if_installed("performance", quietly = TRUE) && isTRUE(performance::check_singularity(model))) {
              insight::format_alert(
                "Your model may suffer from singularity (see `?lme4::isSingular` and `?performance::check_singularity`).",
                "Some of the standard errors and confidence intervals of the random effects parameters are probably not meaningful!"
              )
            }
          },
          error = function(e) {
            if (isTRUE(verbose)) {
              if (grepl("nAGQ of at least 1 is required", e$message, fixed = TRUE)) {
                insight::format_alert("Argument `nAGQ` needs to be larger than 0 to compute confidence intervals for random effect parameters.")
              }
              if (grepl("exactly singular", e$message, fixed = TRUE) ||
                grepl("computationally singular", e$message, fixed = TRUE) ||
                grepl("Exact singular", e$message, fixed = TRUE)) {
                insight::format_alert(
                  "Cannot compute standard errors and confidence intervals for random effects parameters.",
                  "Your model may suffer from singularity (see `?lme4::isSingular` and `?performance::check_singularity`)."
                )
              }
            }
          }
        )
      } else if (isTRUE(verbose)) {
        insight::format_alert("Package {.pkg `merDeriv`} needs to be installed to compute confidence intervals for random effect parameters.")
      }
    }
  } else if (inherits(model, "glmmTMB")) {
    ## TODO "profile" seems to be less stable, so only wald?
    out <- tryCatch(
      {
        var_ci <- rbind(
          as.data.frame(suppressWarnings(stats::confint(
            model,
            parm = "theta_",
            method = "wald",
            level = ci
          ))),
          as.data.frame(suppressWarnings(stats::confint(
            model,
            parm = "sigma",
            method = "wald",
            level = ci
          )))
        )
        colnames(var_ci) <- c("CI_low", "CI_high", "not_used")
        var_ci$Component <- "conditional"
        # # regex-pattern to find conditional and ZI components
        group_factor <- insight::find_random(model, flatten = TRUE)
        group_factor2 <- paste0("(", paste(group_factor, collapse = "|"), ")")

        var_ci$Parameter <- row.names(var_ci)
        pattern <- paste0("^(zi\\.|", group_factor2, "\\.zi\\.)")
        zi_rows <- grepl(pattern, var_ci$Parameter)
        if (any(zi_rows)) {
          var_ci$Component[zi_rows] <- "zi"
        }

        # add Group
        var_ci$Group <- NA
        if (length(group_factor) > 1) {
          var_ci$Group[var_ci$Component == "conditional"] <- gsub(
            paste0("^", group_factor2, "\\.cond\\.(.*)"),
            "\\1",
            var_ci$Parameter[var_ci$Component == "conditional"]
          )
          var_ci$Group[var_ci$Component == "zi"] <- gsub(
            paste0("^", group_factor2, "\\.zi\\.(.*)"),
            "\\1",
            var_ci$Parameter[var_ci$Component == "zi"]
          )
        } else {
          var_ci$Group <- group_factor
          # check if sigma was properly identified
          if (!"sigma" %in% var_ci$Group && "sigma" %in% rownames(var_ci)) {
            var_ci$Group[rownames(var_ci) == "sigma"] <- "Residual"
          }
        }
        var_ci$Group[var_ci$Group == "sigma"] <- "Residual"

        # remove cond/zi prefix
        pattern <- paste0("^(cond\\.|zi\\.|", group_factor, "\\.cond\\.|", group_factor, "\\.zi\\.)(.*)")
        for (p in pattern) {
          var_ci$Parameter <- gsub(p, "\\2", var_ci$Parameter)
        }
        # fix SD and Cor names
        var_ci$Parameter <- gsub(".Intercept.", "(Intercept)", var_ci$Parameter, fixed = TRUE)
        var_ci$Parameter <- gsub("^(Std\\.Dev\\.)(.*)", "SD \\(\\2\\)", var_ci$Parameter)
        var_ci$Parameter <- gsub("^Cor\\.(.*)\\.(.*)", "Cor \\(\\2~\\1:", var_ci$Parameter)
        # minor cleaning
        var_ci$Parameter <- gsub("((", "(", var_ci$Parameter, fixed = TRUE)
        var_ci$Parameter <- gsub("))", ")", var_ci$Parameter, fixed = TRUE)
        var_ci$Parameter <- gsub(")~", "~", var_ci$Parameter, fixed = TRUE)
        # fix sigma
        var_ci$Parameter[var_ci$Parameter == "sigma"] <- "SD (Observations)"
        # add name of group factor to cor
        cor_params <- startsWith(var_ci$Parameter, "Cor ")
        if (any(cor_params)) {
          var_ci$Parameter[cor_params] <- paste0(var_ci$Parameter[cor_params], " ", group_factor, ")")
        }

        # remove unused columns (that are added back after merging)
        out$CI_low <- NULL
        out$CI_high <- NULL

        # filter component
        var_ci <- var_ci[var_ci$Component == component, ]
        var_ci$not_used <- NULL
        var_ci$Component <- NULL

        merge(out, var_ci, sort = FALSE, all.x = TRUE)
        #
        # groups <- utils::stack(insight::find_random(model, flatten = FALSE))
        # colnames(groups) <- c("Group", "Component")
        # groups$Component <- ifelse(groups$Component == "random", "conditional", "zi")
        #
        # # regex-pattern to find conditional and ZI components
        # group_factor <- insight::find_random(model, flatten = TRUE)
        # group_factor2 <- paste0("(", paste(group_factor, collapse = "|"), ")")
        #
        # thetas <- as.data.frame(suppressWarnings(stats::confint(model, parm = "theta_", method = "wald", level = ci)))
        # thetas$Parameter <- row.names(thetas)
        # thetas$Component <- "conditional"
        # # find zi-prefix, to set correct component value
        # pattern <- paste0("^(zi\\.|", group_factor2, "\\.zi\\.)")
        # thetas$Component[grepl(pattern, row.names(thetas))] <- "zi"
        #
        # if (nrow(thetas) == nrow(groups)) {
        #   thetas <- cbind(thetas, groups)
        # } else {
        #   thetas <- merge(thetas, groups, sort = FALSE)
        # }
        #
        # # reorder columns
        # thetas <- datawizard::data_relocate(thetas, cols = "Component", after = "Group")
        # thetas <- datawizard::data_relocate(thetas, cols = "Parameter")
        #
        # sigma <- as.data.frame(suppressWarnings(stats::confint(model, parm = "sigma", method = "wald", level = ci)))
        #
        # # check for sigma component
        # if (nrow(sigma) > 0) {
        #   sigma$Parameter <- row.names(sigma)
        #   sigma$Group <- "Residual"
        #   sigma$Component <- "conditional"
        #   sigma <- datawizard::data_relocate(sigma, cols = "Parameter")
        #   var_ci <- rbind(thetas, sigma)
        # } else {
        #   var_ci <- thetas
        # }
        #
        # colnames(var_ci) <- c("Parameter", "CI_low", "CI_high", "not_used", "Group", "Component")
        #
        # # remove cond/zi prefix
        # pattern <- paste0("^(cond\\.|zi\\.|", group_factor2, "\\.cond\\.|", group_factor2, "\\.zi\\.)")
        # var_ci$Parameter <- gsub(pattern, "", var_ci$Parameter)
        # # fix SD and Cor names
        # var_ci$Parameter <- gsub(".Intercept.", "(Intercept)", var_ci$Parameter, fixed = TRUE)
        # var_ci$Parameter <- gsub("^(Std\\.Dev\\.)(.*)", "SD \\(\\2\\)", var_ci$Parameter)
        # var_ci$Parameter <- gsub("^Cor\\.(.*)\\.(.*)", "Cor \\(\\2~\\1:", var_ci$Parameter)
        # # minor cleaning
        # var_ci$Parameter <- gsub("((", "(", var_ci$Parameter, fixed = TRUE)
        # var_ci$Parameter <- gsub("))", ")", var_ci$Parameter, fixed = TRUE)
        # var_ci$Parameter <- gsub(")~", "~", var_ci$Parameter, fixed = TRUE)
        # # fix sigma
        # var_ci$Parameter[var_ci$Parameter == "sigma"] <- "SD (Observations)"
        # # add name of group factor to cor
        # cor_params <- grepl("^Cor ", var_ci$Parameter)
        # if (any(cor_params)) {
        #   # this might break if length(group_factor) > 1; I don't have a test case handy
        #   var_ci$Parameter[cor_params] <- paste0(var_ci$Parameter[cor_params], " ", group_factor, ")")
        # }
        #
        # # remove unused columns (that are added back after merging)
        # out$CI_low <- NULL
        # out$CI_high <- NULL
        #
        # # filter component
        # var_ci <- var_ci[var_ci$Component == component, ]
        # var_ci$not_used <- NULL
        # var_ci$Component <- NULL
        #
        # merge(out, var_ci, sort = FALSE, all.x = TRUE)
      },
      error = function(e) {
        if (isTRUE(verbose)) {
          insight::format_alert(
            "Cannot compute standard errors and confidence intervals for random effects parameters.",
            "Your model may suffer from singularity (see '?lme4::isSingular' and '?performance::check_singularity')."
          )
        }
        out
      }
    )
  }

  out
}



# Extract Variance and Correlation Components ----

# store essential information about variance components...
# basically, this function should return lme4::VarCorr(x)
.get_variance_information <- function(model, model_component = "conditional") {
  # reason to be installed
  reason <- "to compute random effect variances for mixed models"

  # installed?
  insight::check_if_installed("lme4", reason = reason)

  if (inherits(model, "lme")) {
    insight::check_if_installed("nlme", reason = reason)
  }

  if (inherits(model, "clmm")) {
    insight::check_if_installed("ordinal", reason = reason)
  }

  if (inherits(model, "brmsfit")) {
    insight::check_if_installed("brms", reason = reason)
  }

  if (inherits(model, "cpglmm")) {
    insight::check_if_installed("cplm", reason = reason)
  }

  if (inherits(model, "rstanarm")) {
    insight::check_if_installed("rstanarm", reason = reason)
  }

  # stanreg
  # ---------------------------
  if (inherits(model, "stanreg")) {
    varcorr <- lme4::VarCorr(model)

    # GLMMapdative
    # ---------------------------
  } else if (inherits(model, "MixMod")) {
    vc1 <- vc2 <- NULL
    re_names <- insight::find_random(model)

    vc_cond <- !startsWith(colnames(model$D), "zi_")
    if (any(vc_cond)) {
      vc1 <- model$D[vc_cond, vc_cond, drop = FALSE]
      attr(vc1, "stddev") <- sqrt(diag(vc1))
      attr(vc1, "correlation") <- stats::cov2cor(model$D[vc_cond, vc_cond, drop = FALSE])
    }

    vc_zi <- startsWith(colnames(model$D), "zi_")
    if (any(vc_zi)) {
      colnames(model$D) <- gsub("^zi_(.*)", "\\1", colnames(model$D))
      rownames(model$D) <- colnames(model$D)
      vc2 <- model$D[vc_zi, vc_zi, drop = FALSE]
      attr(vc2, "stddev") <- sqrt(diag(vc2))
      attr(vc2, "correlation") <- stats::cov2cor(model$D[vc_zi, vc_zi, drop = FALSE])
    }

    model_deviance <- insight::get_deviance(model, verbose = FALSE)
    residual_df <- insight::get_df(model, type = "residual", verbose = FALSE)

    vc1 <- list(vc1)
    names(vc1) <- re_names[[1]]
    attr(vc1, "sc") <- sqrt(abs(model_deviance) / residual_df)

    if (!is.null(vc2)) {
      vc2 <- list(vc2)
      names(vc2) <- re_names[[2]]
      attr(vc2, "sc") <- sqrt(abs(model_deviance) / residual_df)
    }

    varcorr <- insight::compact_list(list(vc1, vc2))
    names(varcorr) <- c("cond", "zi")[seq_along(varcorr)]

    # joineRML
    # ---------------------------
  } else if (inherits(model, "mjoint")) {
    re_names <- insight::find_random(model, flatten = TRUE)
    varcorr <- summary(model)$D
    attr(varcorr, "stddev") <- sqrt(diag(varcorr))
    attr(varcorr, "correlation") <- stats::cov2cor(varcorr)
    varcorr <- list(varcorr)
    names(varcorr) <- re_names[1]
    attr(varcorr, "sc") <- model$coef$sigma2[[1]]

    # nlme / glmmPQL
    # ---------------------------
  } else if (inherits(model, "lme")) {
    re_names <- insight::find_random(model, split_nested = TRUE, flatten = TRUE)
    if (.is_nested_lme(model)) {
      varcorr <- .get_nested_lme_varcorr(model)
    } else {
      varcorr <- list(nlme::getVarCov(model))
    }
    names(varcorr) <- re_names

    # ordinal
    # ---------------------------
  } else if (inherits(model, "clmm")) {
    varcorr <- ordinal::VarCorr(model)

    # glmmadmb
    # ---------------------------
  } else if (inherits(model, "glmmadmb")) {
    varcorr <- lme4::VarCorr(model)

    # brms
    # ---------------------------
  } else if (inherits(model, "brmsfit")) {
    varcorr <- lapply(names(lme4::VarCorr(model)), function(i) {
      element <- lme4::VarCorr(model)[[i]]
      if (i != "residual__") {
        if (!is.null(element$cov)) {
          out <- as.matrix(drop(element$cov[, 1, ]))
          colnames(out) <- rownames(out) <- gsub("Intercept", "(Intercept)", rownames(element$cov), fixed = TRUE)
        } else {
          out <- as.matrix(drop(element$sd[, 1])^2)
          colnames(out) <- rownames(out) <- gsub("Intercept", "(Intercept)", rownames(element$sd), fixed = TRUE)
        }
        attr(out, "sttdev") <- element$sd[, 1]
      } else {
        out <- NULL
      }
      out
    })
    varcorr <- insight::compact_list(varcorr)
    names(varcorr) <- setdiff(names(lme4::VarCorr(model)), "residual__")
    attr(varcorr, "sc") <- lme4::VarCorr(model)$residual__$sd[1, 1]

    # cpglmm
    # ---------------------------
  } else if (inherits(model, "cpglmm")) {
    varcorr <- cplm::VarCorr(model)

    # lme4 / glmmTMB
    # ---------------------------
  } else {
    varcorr <- lme4::VarCorr(model)
  }


  # for glmmTMB, tell user that dispersion model is ignored

  if (inherits(model, c("glmmTMB", "MixMod"))) {
    if (is.null(model_component) || model_component == "conditional") {
      varcorr <- .collapse_cond(varcorr)
    } else {
      varcorr <- .collapse_zi(varcorr)
    }
  }

  varcorr
}



# Caution! this is somewhat experimental...
# It retrieves the variance-covariance matrix of random effects
# from nested lme-models.
.get_nested_lme_varcorr <- function(model) {
  # installed?
  insight::check_if_installed("lme4")

  vcor <- lme4::VarCorr(model)
  class(vcor) <- "matrix"

  re_index <- (which(rownames(vcor) == "(Intercept)") - 1)[-1]
  vc_list <- split(data.frame(vcor, stringsAsFactors = FALSE), findInterval(seq_len(nrow(vcor)), re_index))
  vc_rownames <- split(rownames(vcor), findInterval(seq_len(nrow(vcor)), re_index))
  re_pars <- unique(unlist(insight::find_parameters(model)["random"]))
  re_names <- insight::find_random(model, split_nested = TRUE, flatten = TRUE)

  names(vc_list) <- re_names

  Map(
    function(x, y) {
      if ("Corr" %in% colnames(x)) {
        g_cor <- suppressWarnings(stats::na.omit(as.numeric(x[, "Corr"])))
      } else {
        g_cor <- NULL
      }
      row.names(x) <- as.vector(y)
      vl <- rownames(x) %in% re_pars
      x <- suppressWarnings(apply(x[vl, vl, drop = FALSE], MARGIN = c(1, 2), FUN = as.numeric))
      m1 <- matrix(, nrow = nrow(x), ncol = ncol(x))
      m1[seq_len(nrow(m1)), seq_len(ncol(m1))] <- as.vector(x[, 1])
      rownames(m1) <- rownames(x)
      colnames(m1) <- rownames(x)

      if (!is.null(g_cor)) {
        m1_cov <- sqrt(prod(diag(m1))) * g_cor
        for (j in seq_len(ncol(m1))) {
          m1[j, nrow(m1) - j + 1] <- m1_cov[1]
        }
      }

      attr(m1, "cor_slope_intercept") <- g_cor
      m1
    },
    vc_list,
    vc_rownames
  )
}


.is_nested_lme <- function(model) {
  re <- insight::find_random(model)
  if (is.null(re)) {
    return(FALSE)
  }
  sapply(re, function(i) any(grepl(":", i, fixed = TRUE)))
}



# glmmTMB returns a list of model information, one for conditional
# and one for zero-inflation part, so here we "unlist" it, returning
# only the conditional part.
.collapse_cond <- function(x) {
  if (is.list(x) && "cond" %in% names(x)) {
    x[["cond"]]
  } else {
    x
  }
}


.collapse_zi <- function(x) {
  if (is.list(x) && "zi" %in% names(x)) {
    x[["zi"]]
  } else {
    x
  }
}




#### helper to extract various random effect variances -----------------------


# random slope-variances (tau 11) ----
# ----------------------------------------------
.random_slope_variance <- function(model, varcorr) {
  if (inherits(model, "lme")) {
    unlist(lapply(varcorr, function(x) diag(x)[-1]))
  } else {
    # random slopes for correlated slope-intercept
    out <- unlist(lapply(varcorr, function(x) diag(x)[-1]))
    # check for uncorrelated random slopes-intercept
    non_intercepts <- which(sapply(varcorr, function(i) !startsWith(dimnames(i)[[1]][1], "(Intercept)")))
    if (length(non_intercepts)) {
      if (length(non_intercepts) == length(varcorr)) {
        out <- unlist(lapply(varcorr, diag))
      } else {
        dn <- unlist(lapply(varcorr, function(i) dimnames(i)[1])[non_intercepts])
        rndslopes <- unlist(lapply(varcorr, function(i) {
          if (is.null(dim(i)) || identical(dim(i), c(1, 1))) {
            as.vector(i)
          } else {
            as.vector(diag(i))
          }
        })[non_intercepts])
        # random slopes for uncorrelated slope-intercept
        names(rndslopes) <- gsub("(.*)\\.\\d+$", "\\1", names(rndslopes))
        rndslopes <- stats::setNames(rndslopes, paste0(names(rndslopes), ".", dn))
        # anything missing? (i.e. correlated slope-intercept slopes)
        missig_rnd_slope <- setdiff(names(out), names(rndslopes))
        if (length(missig_rnd_slope)) {
          # validation check
          to_remove <- c()
          for (j in seq_along(out)) {
            # identical random slopes might have different names, so
            # we here check if random slopes from correlated and uncorrelated
            # are duplicated (i.e. their difference is 0 - including a tolerance)
            # and then remove duplicated elements
            the_same <- which(abs(outer(out[j], rndslopes, `-`)) < 0.0001)
            if (length(the_same) && grepl(dn[the_same], names(out[j]), fixed = TRUE)) {
              to_remove <- c(to_remove, j)
            }
          }
          if (length(to_remove)) {
            out <- out[-to_remove]
          }
          out <- c(out, rndslopes)
        } else {
          out <- rndslopes
        }
      }
    }
    out
  }
}



# random intercept-variances, i.e.
# between-subject-variance (tau 00) ----
# ----------------------------------------------
.random_intercept_variance <- function(varcorr) {
  vars <- lapply(varcorr, function(i) i[1])
  # check for uncorrelated random slopes-intercept
  non_intercepts <- which(sapply(varcorr, function(i) !startsWith(dimnames(i)[[1]][1], "(Intercept)")))
  if (length(non_intercepts)) {
    vars <- vars[-non_intercepts]
  }

  sapply(vars, function(i) i)
}



# slope-intercept-correlations (rho 01) ----
# ----------------------------------------------
.random_slope_intercept_corr <- function(model, varcorr) {
  if (inherits(model, "lme")) {
    rho01 <- unlist(sapply(varcorr, attr, which = "cor_slope_intercept"))
    if (is.null(rho01)) {
      vc <- lme4::VarCorr(model)
      if ("Corr" %in% colnames(vc)) {
        re_name <- insight::find_random(model, split_nested = FALSE, flatten = TRUE)
        rho01 <- as.vector(suppressWarnings(stats::na.omit(as.numeric(vc[, "Corr"]))))
        if (length(re_name) == length(rho01)) {
          names(rho01) <- re_name
        }
      }
    }
    rho01
  } else {
    corrs <- lapply(varcorr, attr, which = "correlation")
    rho01 <- sapply(corrs, function(i) {
      if (!is.null(i) && colnames(i)[1] == "(Intercept)") {
        i[-1, 1]
      } else {
        NULL
      }
    })
    unlist(rho01)
  }
}



# slope-slope-correlations (rho 00) ----
# ----------------------------------------------
.random_slopes_corr <- function(model, varcorr) {
  corrs <- lapply(varcorr, attr, "correlation")
  rnd_slopes <- unlist(insight::find_random_slopes(model))

  # check if any categorical random slopes. we then have
  # correlation among factor levels
  cat_random_slopes <- tryCatch(
    {
      d <- insight::get_data(model)[rnd_slopes]
      any(vapply(d, is.factor, TRUE))
    },
    error = function(e) {
      NULL
    }
  )

  # check if any polynomial / I term in random slopes.
  # we then have correlation among levels
  rs_names <- unique(unlist(lapply(corrs, colnames)))
  pattern <- paste0("(I|poly)(.*)(", paste0(rnd_slopes, collapse = "|"), ")")
  poly_random_slopes <- any(grepl(pattern, rs_names))

  if (length(rnd_slopes) < 2 && !isTRUE(cat_random_slopes) && !isTRUE(poly_random_slopes)) {
    return(NULL)
  }

  rho00 <- tryCatch(
    {
      insight::compact_list(lapply(corrs, function(d) {
        d[upper.tri(d, diag = TRUE)] <- NA
        d <- as.data.frame(d)

        d <- datawizard::reshape_longer(d, colnames_to = "Parameter1", rows_to = "Parameter2", verbose = FALSE)
        d <- d[stats::complete.cases(d), ]
        d <- d[!d$Parameter1 %in% c("Intercept", "(Intercept)"), ]

        if (nrow(d) == 0) {
          return(NULL)
        }

        d$Parameter <- paste0(d$Parameter1, "-", d$Parameter2)
        d$Parameter1 <- d$Parameter2 <- NULL
        stats::setNames(d$Value, d$Parameter)
      }))
    },
    error = function(e) {
      NULL
    }
  )

  # rho01 <- tryCatch(
  #   {
  #     sapply(corrs, function(i) {
  #       if (!is.null(i)) {
  #         slope_pairs <- utils::combn(x = rnd_slopes, m = 2, simplify = FALSE)
  #         lapply(slope_pairs, function(j) {
  #           stats::setNames(i[j[1], j[2]], paste0("..", paste0(j, collapse = "-")))
  #         })
  #       } else {
  #         NULL
  #       }
  #     })
  #   },
  #   error = function(e) {
  #     NULL
  #   }
  # )

  unlist(rho00)
}

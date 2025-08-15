.extract_random_variances <- function(model, ...) {
  UseMethod(".extract_random_variances")
}


# default method -------------------

.extract_random_variances.default <- function(model,
                                              ci = 0.95,
                                              effects = "random",
                                              component = "conditional",
                                              ci_method = NULL,
                                              ci_random = NULL,
                                              verbose = FALSE,
                                              ...) {
  out <- suppressWarnings(
    .extract_random_variances_helper(
      model,
      ci = ci,
      effects = effects,
      component = component,
      ci_method = ci_method,
      ci_random = ci_random,
      verbose = verbose,
      ...
    )
  )

  # check for errors
  if (is.null(out) && isTRUE(verbose)) {
    insight::format_warning("Something went wrong when calculating random effects parameters. Only showing model's fixed effects now. You may use `effects=\"fixed\"` to speed up the call to `model_parameters()`.") # nolint
  }

  out
}


# glmmTMB -------------------

.extract_random_variances.glmmTMB <- function(model,
                                              ci = 0.95,
                                              effects = "random",
                                              component = "all",
                                              ci_method = NULL,
                                              ci_random = NULL,
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
      ci_random = ci_random,
      verbose = verbose,
      ...
    )
  )

  # check for errors
  if (is.null(out)) {
    if (isTRUE(verbose)) {
      insight::format_warning("Something went wrong when calculating random effects parameters. Only showing model's fixed effects now. You may use `effects=\"fixed\"` to speed up the call to `model_parameters()`.") # nolint
    }
    return(NULL)
  }

  out$Component <- "conditional"

  if (insight::model_info(model, verbose = FALSE)$is_zero_inflated && !is.null(insight::find_random(model)$zero_inflated_random)) { # nolint
    zi_var <- suppressWarnings(
      .extract_random_variances_helper(
        model,
        ci = ci,
        effects = effects,
        component = "zi",
        ci_method = ci_method,
        ci_random = ci_random,
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


# svy2lme ------------------------

.extract_random_variances.svy2lme <- function(model, ci = 0.95, effects = "random", ...) {
  s <- sqrt(as.vector(model$s2))
  stdev <- matrix(s * sqrt(diag(model$L)), ncol = 1)
  vcnames <- c(paste0("SD (", model$znames, ")"), "SD (Observations)")
  grp_names <- names(model$znames)
  if (is.null(grp_names)) {
    grp_names <- model$znames
  }

  out <- data.frame(
    Parameter = vcnames,
    Level = NA,
    Coefficient = c(as.vector(stdev), s),
    SE = NA,
    CI_low = NA,
    CI_high = NA,
    t = NA,
    df_error = NA,
    p = NA,
    Effects = "random",
    Group = c(grp_names, "Residual"),
    stringsAsFactors = FALSE
  )

  # fix intercept names
  out$Parameter <- gsub("(Intercept)", "Intercept", out$Parameter, fixed = TRUE)

  if (effects == "random") {
    out[c("t", "df_error", "p")] <- NULL
  }

  rownames(out) <- NULL
  out
}


# workhorse ------------------------

.extract_random_variances_helper <- function(model,
                                             ci = 0.95,
                                             effects = "random",
                                             component = "conditional",
                                             ci_method = NULL,
                                             ci_random = NULL,
                                             verbose = FALSE,
                                             ...) {
  varcorr <- insight::get_mixed_info(model, component = component, verbose = FALSE)$vc
  if (!inherits(model, c("lme", "coxme"))) {
    class(varcorr) <- "VarCorr.merMod"
  }

  # return varcorr matrix
  re_data <- as.data.frame(varcorr, order = "lower.tri")

  # extract parameters from SD and COR separately, for sorting
  re_sd_intercept <- re_data$var1 == "(Intercept)" & is.na(re_data$var2) & re_data$grp != "Residual"
  re_sd_slope <- re_data$var1 != "(Intercept)" & is.na(re_data$var2) & re_data$grp != "Residual"
  re_cor_intercept <- re_data$var1 == "(Intercept)" & !is.na(re_data$var2) & re_data$grp != "Residual"
  re_cor_slope <- re_data$var1 != "(Intercept)" & !is.na(re_data$var2) & re_data$grp != "Residual"
  re_sigma <- re_data$grp == "Residual"

  # merge to sorted data frame
  out <- rbind(
    re_data[re_sd_intercept, ],
    re_data[re_sd_slope, ],
    re_data[re_cor_intercept, ],
    re_data[re_cor_slope, ],
    re_data[re_sigma, ]
  )
  out$Parameter <- NA

  # rename SD
  sds <- !is.na(out$var1) & is.na(out$var2)
  if (any(sds)) {
    out$Parameter[sds] <- paste0("SD (", out$var1[sds], ")")
  }

  # rename correlations
  corrs <- !is.na(out$var2)
  if (any(corrs)) {
    out$Parameter[corrs] <- paste0("Cor (", out$var1[corrs], "~", out$var2[corrs], ")")
  }

  # rename sigma
  sigma_res <- out$grp == "Residual"
  if (any(sigma_res)) {
    out$Parameter[sigma_res] <- "SD (Observations)"
  }

  # rename columns
  out <- datawizard::data_rename(
    out,
    select = c("grp", "sdcor"),
    replacement = c("Group", "Coefficient")
  )

  # fix names for uncorrelated slope-intercepts
  pattern <- paste0("(", paste(insight::find_random(model, flatten = TRUE), collapse = "|"), ")\\.\\d+$")
  out$Group <- gsub(pattern, "\\1", out$Group)

  # remove non-used columns
  out$var1 <- NULL
  out$var2 <- NULL
  out$grp <- NULL
  out$vcov <- NULL
  out$sdcor <- NULL

  # fix intercept names
  out$Parameter <- gsub("(Intercept)", "Intercept", out$Parameter, fixed = TRUE)

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
    ci_cols <- NULL
    for (i in ci) {
      ci_low <- paste0("CI_low_", i)
      ci_high <- paste0("CI_high_", i)
      ci_cols <- c(ci_cols, ci_low, ci_high)
    }
  }
  out[ci_cols] <- NA

  # variances to SD (sqrt), except correlations and Sigma
  corr_param <- startsWith(out$Parameter, "Cor ")
  sigma_param <- out$Parameter == "SD (Observations)"

  # add confidence intervals?
  if (!is.null(ci) && !all(is.na(ci)) && length(ci) == 1 && !isFALSE(ci_random)) {
    out <- .random_sd_ci(model, out, ci_method, ci, ci_random, corr_param, sigma_param, component, verbose = verbose)
  }

  out <- out[c("Parameter", "Level", "Coefficient", "SE", ci_cols, stat_column, "df_error", "p", "Effects", "Group")]

  if (effects == "random") {
    out[c(stat_column, "df_error", "p", "CI")] <- NULL
  }

  rownames(out) <- NULL
  out
}


#' @export
as.data.frame.VarCorr.lme <- function(x, row.names = NULL, optional = FALSE, ...) {
  # retrieve RE SD and COR
  stddevs <- sapply(x[, "StdDev"], as.numeric)
  if ("Corr" %in% colnames(x)) {
    corrs <- suppressWarnings(sapply(x[, "Corr"], as.numeric))
  } else {
    corrs <- NULL
  }
  grps <- endsWith(names(stddevs), " =")

  # for multiple grouping factors, split at each group
  if (any(grps)) {
    from <- which(grps)
    to <- c(which(grps) - 1, length(grps))[-1]
    out_sd <- do.call(rbind, lapply(seq_along(from), function(i) {
      values <- stddevs[from[i]:to[i]]
      .data_frame(
        grp = gsub("(.*) =$", "\\1", names(values[1])),
        var1 = names(values[-1]),
        var2 = NA_character_,
        sdcor = unname(values[-1])
      )
    }))
    if (is.null(corrs)) {
      out_cor <- NULL
    } else {
      out_cor <- do.call(rbind, lapply(seq_along(from), function(i) {
        values <- corrs[from[i]:to[i]]
        .data_frame(
          grp = gsub("(.*) =$", "\\1", names(values[1])),
          var1 = "(Intercept)",
          var2 = names(values[-1]),
          sdcor = unname(values[-1])
        )
      }))
    }
  } else {
    out_sd <- .data_frame(
      grp = gsub("(.*) =(.*)", "\\1", attributes(x)$title),
      var1 = names(stddevs),
      var2 = NA_character_,
      sdcor = unname(stddevs)
    )
    if (is.null(corrs)) {
      out_cor <- NULL
    } else {
      out_cor <- .data_frame(
        grp = gsub("(.*) =(.*)", "\\1", attributes(x)$title),
        var1 = "(Intercept)",
        var2 = names(corrs),
        sdcor = unname(corrs)
      )
    }
  }

  out_sd$grp[out_sd$var1 == "Residual"] <- "Residual"
  out_sd$var1[out_sd$grp == "Residual"] <- NA_character_
  out_sd$var2[out_sd$grp == "Residual"] <- NA_character_
  out_cor <- out_cor[!is.na(out_cor$sdcor), ]

  rbind(out_sd, out_cor)
}


#' @export
as.data.frame.VarCorr.coxme <- function(x, row.names = NULL, optional = FALSE, ...) {
  # extract variances from VarCorr object
  variances <- lapply(x, diag)
  # create data frame, similar to as.data.frame.VarCorr.merMod
  out <- do.call(rbind, lapply(names(variances), function(i) {
    # information on variances
    d <- data.frame(
      grp = i,
      var1 = names(variances[[i]]),
      var2 = NA_character_,
      vcov = as.numeric(variances[[i]]),
      sdcor = sqrt(as.numeric(variances[[i]])),
      stringsAsFactors = FALSE
    )
    # add correlations, if any
    if (nrow(x[[i]]) > 1) {
      d <- rbind(d, data.frame(
        grp = i,
        var1 = "(Intercept)",
        var2 = rownames(x[[i]])[2],
        vcov = NA_real_,
        sdcor = as.numeric(x[[i]][2, 1]),
        stringsAsFactors = FALSE
      ))
    }
  }))

  # bind residual variance
  rbind(out, data.frame(
    grp = "Residual",
    var1 = NA_character_,
    var2 = NA_character_,
    vcov = NA_real_,
    sdcor = NA_real_,
    stringsAsFactors = FALSE
  ))
}


# extract CI for random SD ------------------------

.random_sd_ci <- function(model,
                          out,
                          ci_method,
                          ci, ci_random,
                          corr_param,
                          sigma_param,
                          component = NULL,
                          verbose = FALSE) {
  ## TODO needs to be removed once MCM > 0.1.5 is on CRAN
  if (startsWith(insight::safe_deparse(insight::get_call(model)), "mcm_lmer")) {
    return(out)
  }

  # heuristic to check whether CIs for random effects should be computed or
  # not. If `ci_random=NULL`, we check model complexity and decide whether to
  # go on or not. For models with larger samples sized or more complex random
  # effects, this might be quite time consuming.

  if (is.null(ci_random)) {
    # check sample size, don't compute by default when larger than 1000
    n_obs <- insight::n_obs(model)
    if (n_obs >= 1000) {
      return(out)
    }

    # check complexity of random effects
    re <- insight::find_random(model, flatten = TRUE)
    rs <- insight::find_random_slopes(model)

    # quit if if random slopes and larger sample size or more than 1 grouping factor
    if (!is.null(rs) && (n_obs >= 500 || length(re) > 1)) {
      return(out)
    }

    # quit if if than two grouping factors
    if (length(re) > 2) {
      return(out)
    }
  }


  if (inherits(model, c("merMod", "glmerMod", "lmerMod"))) {
    # lme4 - boot and profile

    if (!is.null(ci_method) && ci_method %in% c("profile", "boot")) {
      out <- tryCatch(
        {
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
          out
        },
        error = function(e) {
          if (isTRUE(verbose)) {
            insight::format_alert(
              "Cannot compute profiled standard errors and confidence intervals for random effects parameters.",
              "Your model may suffer from singularity (see '?lme4::isSingular' and '?performance::check_singularity').",
              "You may try to impose a prior on the random effects parameters, e.g. using the {.pkg glmmTMB} package."
            )
          }
          out
        }
      )
    } else if (!is.null(ci_method)) {
      # lme4 - wald / normal CI

      merDeriv_loaded <- isNamespaceLoaded("merDeriv")
      # detach on exit
      on.exit(
        if (!merDeriv_loaded) {
          .unregister_vcov()
        },
        add = TRUE,
        after = FALSE
      )

      # Wald based CIs
      # see https://stat.ethz.ch/pipermail/r-sig-mixed-models/2022q1/029985.html
      if (all(suppressMessages(insight::check_if_installed(c("merDeriv", "lme4"), quietly = TRUE)))) {
        # this may fail, so wrap in try-catch
        out <- tryCatch(
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
              var_ci$Parameter[var_ci_corr_param] <- paste0("Cor (Intercept~", rnd_slope_terms, ")")
            }

            # correlations w/o intercept? usually only for factors
            # or: correlation among slopes. we need to recover the (categorical)
            # term names from our prepared data frame, then match vcov-names
            rnd_slope_corr <- grepl("^Cor \\((?!Intercept~)", out$Parameter, perl = TRUE)
            if (any(rnd_slope_corr)) {
              for (gr in setdiff(unique(out$Group), "Residual")) {
                rnd_slope_corr_grp <- rnd_slope_corr & out$Group == gr
                dummy <- gsub("Cor \\((.*)~(.*)\\)", "\\2.\\1", out$Parameter[rnd_slope_corr_grp])
                var_ci$Parameter[var_ci$Group == gr][match(dummy, var_ci$Parameter[var_ci$Group == gr])] <- out$Parameter[rnd_slope_corr_grp] # nolint
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
            if (isTRUE(verbose) && insight::check_if_installed("performance", quietly = TRUE) && isTRUE(performance::check_singularity(model))) { # nolint
              insight::format_alert(
                "Your model may suffer from singularity (see see `?lme4::isSingular` and `?performance::check_singularity`).", # nolint
                "Some of the standard errors and confidence intervals of the random effects parameters are probably not meaningful!", # nolint
                "You may try to impose a prior on the random effects parameters, e.g. using the {.pkg glmmTMB} package." # nolint
              )
            }
            out
          },
          error = function(e) {
            if (isTRUE(verbose)) {
              if (grepl("nAGQ of at least 1 is required", e$message, fixed = TRUE)) {
                insight::format_alert("Argument `nAGQ` needs to be larger than 0 to compute confidence intervals for random effect parameters.") # nolint
              }
              if (grepl("Multiple cluster variables detected.", e$message, fixed = TRUE)) {
                insight::format_alert("Confidence intervals for random effect parameters are currently not supported for multiple grouping variables.") # nolint
              }
              if (grepl("exactly singular", e$message, fixed = TRUE) ||
                grepl("computationally singular", e$message, fixed = TRUE) ||
                grepl("Exact singular", e$message, fixed = TRUE)) {
                insight::format_alert(
                  "Cannot compute standard errors and confidence intervals for random effects parameters.",
                  "Your model may suffer from singularity (see see `?lme4::isSingular` and `?performance::check_singularity`).", # nolint
                  "You may try to impose a prior on the random effects parameters, e.g. using the {.pkg glmmTMB} package." # nolint
                )
              }
            }
            out
          }
        )
      } else if (isTRUE(verbose)) {
        insight::format_alert("Package 'merDeriv' needs to be installed to compute confidence intervals for random effect parameters.") # nolint
      }
    }
  } else if (inherits(model, "glmmTMB")) {
    # glmmTMB random-effects-CI

    ## TODO "profile" seems to be less stable, so only wald?
    out <- tryCatch(
      {
        var_ci <- rbind(
          as.data.frame(suppressWarnings(stats::confint(model, parm = "theta_", method = "wald", level = ci))),
          as.data.frame(suppressWarnings(stats::confint(model, parm = "sigma", method = "wald", level = ci)))
        )

        colnames(var_ci) <- c("CI_low", "CI_high", "not_used")
        var_ci$Component <- "conditional"
        var_ci$Parameter <- row.names(var_ci)

        if (utils::packageVersion("glmmTMB") > "1.1.3") {
          var_ci$Component[startsWith(var_ci$Parameter, "zi.")] <- "zi"
          # remove cond/zi prefix
          var_ci$Parameter <- gsub("^(cond\\.|zi\\.)(.*)", "\\2", var_ci$Parameter)
          # copy RE group
          var_ci$Group <- gsub("(.*)\\|(.*)$", "\\2", var_ci$Parameter)
          var_ci$Parameter <- gsub("(.*)\\|(.*)$", "\\1", var_ci$Parameter)
          var_ci$Group[rownames(var_ci) == "sigma"] <- "Residual"
        } else {
          # regex-pattern to find conditional and ZI components
          group_factor <- insight::find_random(model, flatten = TRUE)
          group_factor2 <- paste0("(", paste(group_factor, collapse = "|"), ")")

          pattern <- paste0("^(zi\\.|", group_factor2, "\\.zi\\.)")
          zi_rows <- grepl(pattern, var_ci$Parameter)
          if (any(zi_rows)) {
            var_ci$Component[zi_rows] <- "zi"
          }

          # add Group
          var_ci$Group <- NA
          if (length(group_factor) > 1) {
            var_ci$Group[var_ci$Component == "conditional"] <- gsub(paste0("^", group_factor2, "\\.cond\\.(.*)"), "\\1", var_ci$Parameter[var_ci$Component == "conditional"]) # nolint
            var_ci$Group[var_ci$Component == "zi"] <- gsub(paste0("^", group_factor2, "\\.zi\\.(.*)"), "\\1", var_ci$Parameter[var_ci$Component == "zi"]) # nolint
          } else {
            var_ci$Group <- group_factor
            # check if sigma was properly identified
            if (!"sigma" %in% var_ci$Group && "sigma" %in% rownames(var_ci)) {
              var_ci$Group[rownames(var_ci) == "sigma"] <- "Residual"
            }
          }

          # remove cond/zi prefix
          pattern <- paste0("^(cond\\.|zi\\.|", group_factor, "\\.cond\\.|", group_factor, "\\.zi\\.)(.*)")
          for (p in pattern) {
            var_ci$Parameter <- gsub(p, "\\2", var_ci$Parameter)
          }
        }

        # fix SD and Cor names
        var_ci$Parameter <- gsub(".Intercept.", "(Intercept)", var_ci$Parameter, fixed = TRUE)
        var_ci$Parameter <- gsub("^(Std\\.Dev\\.)(.*)", "SD \\(\\2\\)", var_ci$Parameter)
        var_ci$Parameter <- gsub("^Cor\\.(.*)\\.(.*)", "Cor \\(\\2~\\1\\)", var_ci$Parameter)
        # minor cleaning
        var_ci$Parameter <- gsub("((", "(", var_ci$Parameter, fixed = TRUE)
        var_ci$Parameter <- gsub("))", ")", var_ci$Parameter, fixed = TRUE)
        var_ci$Parameter <- gsub(")~", "~", var_ci$Parameter, fixed = TRUE)
        # fix sigma
        var_ci$Parameter[var_ci$Parameter == "sigma"] <- "SD (Observations)"
        var_ci$Group[var_ci$Group == "sigma"] <- "Residual"

        # remove unused columns (that are added back after merging)
        out$CI_low <- NULL
        out$CI_high <- NULL

        # filter component
        var_ci <- var_ci[var_ci$Component == component, ]
        var_ci$not_used <- NULL
        var_ci$Component <- NULL

        # check results - warn user
        if (isTRUE(verbose)) {
          missing_ci <- any(is.na(var_ci$CI_low) | is.na(var_ci$CI_high))
          singular_fit <- insight::check_if_installed("performance", quietly = TRUE) & isTRUE(performance::check_singularity(model)) # nolint

          if (singular_fit) {
            insight::format_alert(
              "Your model may suffer from singularity (see `?lme4::isSingular` and `?performance::check_singularity`).",
              "Some of the confidence intervals of the random effects parameters are probably not meaningful!",
              "You may try to impose a prior on the random effects parameters, e.g. using the {.pkg glmmTMB} package." # nolint
            )
          } else if (missing_ci) {
            insight::format_alert(
              "Your model may suffer from singularity (see `?lme4::isSingular` and `?performance::check_singularity`).",
              "Some of the confidence intervals of the random effects parameters could not be calculated or are probably not meaningful!", # nolint
              "You may try to impose a prior on the random effects parameters, e.g. using the {.pkg glmmTMB} package." # nolint
            )
          }
        }

        # merge and sort
        out$.sort_id <- seq_len(nrow(out))
        out <- merge(out, var_ci, sort = FALSE, all.x = TRUE)
        out <- out[order(out$.sort_id), ]
        out$.sort_id <- NULL
        out
      },
      error = function(e) {
        if (isTRUE(verbose)) {
          insight::format_alert(
            "Cannot compute confidence intervals for random effects parameters.",
            "Your model may suffer from singularity (see `?lme4::isSingular` and `?performance::check_singularity`)."
          )
        }
        out
      }
    )
  }

  out
}


# this is used to only temporarily load merDeriv and to point registered
# methods from merDeriv to lme4-methods. if merDeriv was loaded before,
# nothing will be changed. If merDeriv was not loaded, vcov-methods registered
# by merDeriv will be re-registered to use lme4::vcov.merMod. This is no problem,
# because *if* useres load merDeriv later manually, merDeriv-vcov-methods will
# be registered again.

.unregister_vcov <- function() {
  unloadNamespace("merDeriv")
  suppressWarnings(suppressMessages(registerS3method("vcov", "lmerMod", method = lme4::vcov.merMod)))
  suppressWarnings(suppressMessages(registerS3method("vcov", "glmerMod", method = lme4::vcov.merMod)))
}

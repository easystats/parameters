.extract_random_variances <- function(model, ci = .95, effects = "random_variances", ...) {
  out <- random_parameters(model)

  out <- out[!out$Description %in% c("N", "Observations"), ]
  out$Value <- sqrt(out$Value)
  out$Type[out$Component == "tau00"] <- "SD (Intercept)"
  out$Type[out$Component == "sigma2"] <- "SD (Observations)"

  out$Group <- NA
  out$Group[out$Component == "tau00"] <- out$Term[out$Component == "tau00"]
  out$Group[out$Component == "sigma2"] <- "Residual"

  ran_slope <- which(out$Component == "tau11")
  ran_cor <- which(out$Component == "rho01")
  group_names <- out$Term[out$Component == "tau00"]

  for (i in group_names) {
    if (length(ran_slope)) {
      slope_groups <- which(grepl(paste0("^\\Q", i, "\\E"), out$Term[ran_slope]))
      if (length(slope_groups)) {
        out$Group[ran_slope[slope_groups]] <- i
        out$Type[ran_slope[slope_groups]] <- paste0("SD (",
                                                      gsub("^\\.", "", gsub(i, "", out$Term[ran_slope[slope_groups]], fixed = TRUE)),
                                                      ")")
      }
    }
    if (length(ran_cor)) {
      cor_groups <- which(grepl(paste0("^\\Q", i, "\\E"), out$Term[ran_cor]))
      if (length(cor_groups)) {
        out$Group[ran_cor[cor_groups]] <- i
        out$Type[ran_cor[cor_groups]] <- paste0("Rho (Intercept~",
                                                  gsub("^\\.", "", gsub(i, "", out$Term[ran_slope[slope_groups]], fixed = TRUE)),
                                                  ")")
      }
    }
  }

  out$Description <- NULL
  out$Component <- NULL
  out$Term <- NULL

  colnames(out) <- c("Parameter", "Coefficient", "Group")

  stat_column <- gsub("-statistic", "", insight::find_statistic(model), fixed = TRUE)

  # to match rbind
  out[[stat_column]] <- NA
  out$SE <- NA
  out$df_error <- NA
  out$p <- NA
  out$Level <- NA

  out$Effects <- "random_variances"

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

  if (effects != "fixed") {
    out[c(stat_column, "df_error", "p")] <- NULL
  }
  out
}

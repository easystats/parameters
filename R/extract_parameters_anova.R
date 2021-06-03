#' @keywords internal
.extract_parameters_anova <- function(model, test = "multivariate") {

  # Processing
  if ("manova" %in% class(model)) {
    parameters <- .extract_anova_manova(model)
  } else if ("maov" %in% class(model)) {
    parameters <- .extract_anova_maov(model)
  } else if ("aov" %in% class(model)) {
    parameters <- .extract_anova_aov(model)
  } else if ("anova" %in% class(model)) {
    parameters <- .extract_anova_anova(model)
  } else if ("Anova.mlm" %in% class(model)) {
    parameters <- .extract_anova_mlm(model, test)
  } else if ("aovlist" %in% class(model)) {
    parameters <- .extract_anova_aovlist(model)
  } else if ("anova.rms" %in% class(model)) {
    parameters <- .extract_anova_aov_rms(model)
  }

  # Rename

  # p-values
  names(parameters) <- gsub("(Pr|P)\\(>.*\\)", "p", names(parameters))
  names(parameters) <- gsub("Pr..Chisq.", "p", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Pr..Chi.", "p", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("p.value", "p", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("^P$", "p", names(parameters))
  # squares
  names(parameters) <- gsub("Sum Sq", "Sum_Squares", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Error SS", "Sum_Squares_Error", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Partial.SS", "Sum_Squares_Partial", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Sum of Sq", "Sum_Squares", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Mean Sq", "Mean_Square", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("MSE", "Mean_Square", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("MS", "Mean_Square", names(parameters), fixed = TRUE)
  # statistic
  names(parameters) <- gsub("approx F", "F", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("F values", "F", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("F value", "F", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("LR.Chisq", "Chi2", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("LR Chisq", "Chi2", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Chisq", "Chi2", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Chi.sq", "Chi2", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Chi-Square", "Chi2", names(parameters), fixed = TRUE)
  # other
  names(parameters) <- gsub("logLik", "Log_Likelihood", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("deviance", "Deviance", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Resid. Dev", "Deviance_error", names(parameters), fixed = TRUE)
  # error-df
  if (!"df_error" %in% names(parameters)) {
    names(parameters) <- gsub("den Df", "df_error", names(parameters), fixed = TRUE)
    names(parameters) <- gsub("Res.Df", "df_error", names(parameters), fixed = TRUE)
    names(parameters) <- gsub("Resid. Df", "df_error", names(parameters), fixed = TRUE)
    names(parameters) <- gsub("Res.DoF", "df_error", names(parameters), fixed = TRUE)
  }
  # df
  if (!"df" %in% names(parameters)) {
    names(parameters) <- gsub("npar", "df", names(parameters), fixed = TRUE)
    names(parameters) <- gsub("NumDF", "df", names(parameters), fixed = TRUE)
    names(parameters) <- gsub("num Df", "df", names(parameters), fixed = TRUE)
    names(parameters) <- gsub("d.f.", "df", names(parameters), fixed = TRUE)
    names(parameters) <- gsub("Df", "df", names(parameters), fixed = TRUE)
  }
  # other df
  names(parameters) <- gsub("Chi.Df", "Chi2_df", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Chi DoF", "Chi2_df", names(parameters), fixed = TRUE)

  # Reorder
  row.names(parameters) <- NULL
  order <- c("Response", "Group", "Parameter", "Pillai", "AIC", "BIC", "Log_Likelihood", "Chi2", "Chi2_df", "RSS", "Sum_Squares", "Sum_Squares_Partial", "Sum_Squares_Error", "df", "Deviance", "Statistic", "df_num", "df_error", "Deviance_error", "Mean_Square", "F", "Rao", "p")
  parameters <- parameters[order[order %in% names(parameters)]]

  .remove_backticks_from_parameter_names(parameters)
}



# helpers -----


# aov -----

.extract_anova_aov <- function(model) {
  parameters <- as.data.frame(summary(model)[[1]])
  parameters$Parameter <- trimws(row.names(parameters))
  parameters
}


# manova -----

.extract_anova_manova <- function(model) {
  parameters <- as.data.frame(summary(model)$stats)
  parameters$Parameter <- trimws(row.names(parameters))
  parameters[["den Df"]] <- NULL
  parameters[["num Df"]] <- NULL
  parameters
}


# maov -----

.extract_anova_maov <- function(model) {
  s <- summary(model)
  out <- do.call(rbind, lapply(names(s), function(i) {
    parameters <- as.data.frame(s[[i]])
    parameters$Parameter <- trimws(row.names(parameters))
    parameters$Response <- gsub("\\s*Response ", "", i)
    parameters
  }))
  out
}


# aov.rms -----

.extract_anova_aov_rms <- function(model) {
  parameters <- data.frame(model)
  parameters$Parameter <- rownames(parameters)
  parameters$Parameter[parameters$Parameter == "ERROR"] <- "Residuals"
  parameters$Parameter[parameters$Parameter == "TOTAL"] <- "Total"
  parameters
}


# aovlist -----

.extract_anova_aovlist <- function(model) {
  if (names(model)[1L] == "(Intercept)") {
    model <- model[-1L]
  }
  parameters <- Reduce(function(x, y) merge(x, y, all = TRUE, sort = FALSE), lapply(names(model), function(i) {
    aov_summary <- summary(model[[i]])
    if (inherits(aov_summary, "summary.manova")) {
      temp <- as.data.frame(aov_summary$stats)
    } else {
      temp <- as.data.frame(aov_summary[[1]])
    }
    temp$Parameter <- trimws(row.names(temp))
    temp$Group <- i
    temp
  }))
  # parameters <- parameters[order(parameters$Group), ]
  parameters
}


# anova -----

.extract_anova_anova <- function(model) {
  parameters <- as.data.frame(model)
  parameters$Parameter <- trimws(row.names(parameters))
  # Deal with anovas of models
  if (length(attributes(model)$heading) == 2) {
    info <- attributes(model)$heading[[2]]
    if (grepl("Model", info)) {
      parameters$Parameter <- unlist(strsplit(info, "\n", fixed = TRUE))
    }
  } else if (length(attributes(model)$heading) > 2) {
    p_names <- attributes(model)$heading[-1:-2]
    if (nrow(parameters) == length(p_names)) {
      parameters$Parameter <- p_names
    }
  }

  # If mixed models...
  sumsq <- names(parameters)[names(parameters) %in% c("Sum Sq", "Sum of Sq")]
  df_num <- names(parameters)[names(parameters) %in% c("npar", "Df", "NumDF", "num Df")]
  mean_sq <- names(parameters)[names(parameters) %in% c("Mean Sq", "MSE")]

  if (length(sumsq) != 0 && length(df_num) != 0) {
    parameters$Mean_Square <- parameters[[sumsq]] / parameters[[df_num]]
  } else if (length(mean_sq) != 0) {
    parameters$Mean_Square <- parameters[[mean_sq]]
  }

  if (length(df_num) == 0 && length(sumsq) != 0 && "Mean_Square" %in% colnames(parameters) && !("Df" %in% colnames(parameters))) {
    parameters$Df <- round(parameters[[sumsq]] / parameters$Mean_Square)
  }
  parameters
}


# Anova.mlm -------------

.extract_anova_mlm <- function(model, test = NULL) {
  if (identical(test, "univariate")) {
    ut <- unclass(summary(model)$univariate.tests)
    out <- data.frame(Parameter = rownames(ut), stringsAsFactors = FALSE)
    out <- cbind(out, as.data.frame(ut))
  } else {
    out <- lapply(1:length(model$terms), function(i) {
      if (model$repeated) {
        qr_value <- qr(model$SSPE[[i]])
      } else {
        qr_value <- qr(model$SSPE)
      }
      eigs <- Re(eigen(qr.coef(qr_value, model$SSP[[i]]), symmetric = FALSE)$values)
      test <- switch(model$test,
        "Pillai" = .pillai_test(eigs, model$df[i], model$error.df),
        "Wilks" = .wilks_test(eigs, model$df[i], model$error.df),
        "Hotelling-Lawley" = .hl_test(eigs, model$df[i], model$error.df),
        "Roy" = .roy_test(eigs, model$df[i], model$error.df)
      )
      data.frame(
        Parameter = model$terms[i],
        df = model$df[i],
        Statistic = test[1],
        `F` = test[2],
        df_num = test[3],
        df_error = test[4],
        p = stats::pf(test[2], test[3], test[4], lower.tail = FALSE),
        stringsAsFactors = FALSE
      )
    })

    out <- do.call(rbind, out)
  }
  out
}




# test helper -------------


.pillai_test <- function(eig, q, df.res) {
  test <- sum(eig / (1 + eig))
  p <- length(eig)
  s <- min(p, q)
  n <- 0.5 * (df.res - p - 1)
  m <- 0.5 * (abs(p - q) - 1)
  tmp1 <- 2 * m + s + 1
  tmp2 <- 2 * n + s + 1
  c(test, (tmp2 / tmp1 * test) / (s - test), s * tmp1, s * tmp2)
}


.roy_test <- function(eig, q, df.res) {
  p <- length(eig)
  test <- max(eig)
  tmp1 <- max(p, q)
  tmp2 <- df.res - tmp1 + q
  c(test, (tmp2 * test) / tmp1, tmp1, tmp2)
}


.hl_test <- function(eig, q, df.res) {
  test <- sum(eig)
  p <- length(eig)
  m <- 0.5 * (abs(p - q) - 1)
  n <- 0.5 * (df.res - p - 1)
  s <- min(p, q)
  tmp1 <- 2 * m + s + 1
  tmp2 <- 2 * (s * n + 1)
  c(test, (tmp2 * test) / s / s / tmp1, s * tmp1, tmp2)
}


.wilks_test <- function(eig, q, df.res) {
  test <- prod(1 / (1 + eig))
  p <- length(eig)
  tmp1 <- df.res - 0.5 * (p - q + 1)
  tmp2 <- (p * q - 2) / 4
  tmp3 <- p^2 + q^2 - 5
  tmp3 <- if (tmp3 > 0) {
    sqrt(((p * q)^2 - 4) / tmp3)
  } else {
    1
  }
  c(
    test, ((test^(-1 / tmp3) - 1) * (tmp1 * tmp3 - 2 * tmp2)) / p / q,
    p * q, tmp1 * tmp3 - 2 * tmp2
  )
}



# parameter-power ----------------


.power_for_aov <- function(model, params) {
  if (requireNamespace("effectsize", quietly = TRUE)) {
    power <- tryCatch(
      {
        cohens_f2 <- effectsize::cohens_f_squared(model, partial = TRUE)

        f2 <- cohens_f2$Cohens_f2[match(cohens_f2$Parameter, params$Parameter)]
        u <- params$df[params$Parameter != "Residuals"]
        v <- params$df[params$Parameter == "Residuals"]

        lambda <- f2 * (u + v + 1)
        cohens_f2$Power <- stats::pf(stats::qf(0.05, u, v, lower = FALSE), u, v, lambda, lower = FALSE)
        cohens_f2
      },
      error = function(e) {
        NULL
      }
    )
  }

  if (!is.null(power)) {
    params <- merge(params, cohens_f2[c("Parameter", "Power")], sort = FALSE, all = TRUE)
  }

  params
}

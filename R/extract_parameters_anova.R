#' @keywords internal
.extract_parameters_anova <- function(model) {

  # Processing
  if ("manova" %in% class(model)) {
    parameters <- .extract_anova_manova(model)
  } else if ("maov" %in% class(model)) {
    parameters <- .extract_anova_maov(model)
  } else if ("aov" %in% class(model)) {
    parameters <- .extract_anova_aov(model)
  } else if ("anova" %in% class(model)) {
    parameters <- .extract_anova_anova(model)
  } else if ("aovlist" %in% class(model)) {
    parameters <- .extract_anova_aovlist(model)
  } else if ("anova.rms" %in% class(model)) {
    parameters <- .extract_anova_aov_rms(model)
  }

  # Rename
  names(parameters) <- gsub("(Pr|P)\\(>.*\\)", "p", names(parameters))
  names(parameters) <- gsub("npar", "df", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("NumDF", "df", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("d.f.", "df", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Chi.Df", "Chi2_df", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Chi DoF", "Chi2_df", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Sum Sq", "Sum_Squares", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Partial.SS", "Sum_Squares_Partial", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Sum of Sq", "Sum_Squares", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Mean Sq", "Mean_Square", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("MS", "Mean_Square", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("approx F", "F", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("F value", "F", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Res.Df", "df_residual", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Res.DoF", "df_residual", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Chisq", "Chi2", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Pr..Chisq.", "p", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Pr..Chi.", "p", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Chi.sq", "Chi2", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("Chi-Square", "Chi2", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("LR.Chisq", "Chi2", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("LR Chisq", "Chi2", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("p.value", "p", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("logLik", "Log_Likelihood", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("deviance", "Deviance", names(parameters), fixed = TRUE)
  names(parameters) <- gsub("^P$", "p", names(parameters))
  names(parameters) <- gsub("Df", "df", names(parameters), fixed = TRUE)

  # Reorder
  row.names(parameters) <- NULL
  order <- c("Response", "Group", "Parameter", "Pillai", "AIC", "BIC", "Log_Likelihood", "Deviance", "Chi2", "Chi2_df", "RSS", "Sum_Squares", "Sum_Squares_Partial", "df", "df_residual", "Mean_Square", "F", "p")
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
    parameters$Parameter <- attributes(model)$heading[-1:-2]
  }

  # If mixed models...
  sumsq <- names(parameters)[names(parameters) %in% c("Sum Sq", "Sum of Sq")]
  df_num <- names(parameters)[names(parameters) %in% c("npar", "Df", "NumDF")]
  mean_sq <- names(parameters)[names(parameters) %in% c("Mean Sq")]

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

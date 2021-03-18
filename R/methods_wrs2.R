#' Parameters from robust statistical objects in \code{WRS2}
#'
#' @param model Object from \code{WRS2} package.
#' @param ... Arguments passed to or from other methods.
#' @inheritParams model_parameters.default
#'
#' @examples
#' if (require("WRS2")) {
#'   model <- t1way(libido ~ dose, data = viagra)
#'   model_parameters(model)
#' }
#' @return A data frame of indices related to the model's parameters.
#' @export
model_parameters.t1way <- function(model, verbose = TRUE, ...) {
  parameters <- .extract_wrs2_t1way(model)
  parameters <- .add_htest_parameters_attributes(parameters, model, ...)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}


# WRS2 anova ----------------------

.extract_wrs2_t1way <- function(model) {
  fcall <- .safe_deparse(model$call)
  # effect sizes are by default contained for `t1way` but not `rmanova`
  if (grepl("^(t1way|WRS2::t1way)", fcall)) {
    data.frame(
      "F" = model$test,
      "df" = model$df1,
      "df_error" = model$df2,
      "p" = model$p.value,
      "Estimate" = model$effsize,
      "CI" = .95,
      "CI_low" =  model$effsize_ci[1],
      "CI_high" = model$effsize_ci[2],
      "Effectsize" = "Explanatory measure of effect size",
      "Method" = "A heteroscedastic one-way ANOVA for trimmed means",
      stringsAsFactors = FALSE
    )
  } else if (grepl("^(rmanova|WRS2::rmanova)", fcall)) {
    data.frame(
      "F" = model$test,
      "df" = model$df1,
      "df_error" = model$df2,
      "p" = model$p.value,
      "Method" = "A heteroscedastic one-way repeated measures ANOVA for trimmed means",
      stringsAsFactors = FALSE
    )
  }
}


#' @export
model_parameters.yuen <- function(model, verbose = TRUE, ...) {
  parameters <- .extract_wrs2_yuen(model)
  parameters <- .add_htest_parameters_attributes(parameters, model, ...)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}


# WRS2 ttest ----------------------

.extract_wrs2_yuen <- function(model) {
  fcall <- .safe_deparse(model$call)

  if (grepl("^(yuen\\(|WRS2::yuen\\()", fcall)) {
    data.frame(
      "Difference" = model$diff,
      "CI" = .95,
      "Difference_CI_low" =  model$conf.int[1],
      "Difference_CI_high" =  model$conf.int[2],
      "t" = model$test,
      "df_error" = model$df,
      "p" = model$p.value,
      "Estimate" = model$effsize,
      "Effectsize" = "Explanatory measure of effect size",
      "Method" = "Yuen's test on trimmed means for independent samples",
      stringsAsFactors = FALSE
    )
  } else if (grepl("^(yuend|WRS2::yuend)", fcall)) {
    data.frame(
      "Difference" = model$diff,
      "CI" = .95,
      "Difference_CI_low" =  model$conf.int[1],
      "Difference_CI_high" =  model$conf.int[2],
      "t" = model$test,
      "df_error" = model$df,
      "p" = model$p.value,
      "Estimate" = model$effsize,
      "Effectsize" = "Explanatory measure of effect size",
      "Method" = "Yuen's test on trimmed means for dependent samples",
      stringsAsFactors = FALSE
    )
  }
}


#' @export
model_parameters.mcp1 <- function(model, verbose = TRUE, ...) {
  parameters <- .extract_wrs2_mcp12(model)
  parameters <- .add_htest_parameters_attributes(parameters, model, ...)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}


#' @export
model_parameters.mcp2 <- model_parameters.mcp1

#' @export
model_parameters.robtab <- function(model, verbose = TRUE, ...) {
  parameters <- .extract_wrs2_robtab(model)
  parameters <- .add_htest_parameters_attributes(parameters, model, ...)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}

# WRS2 post hoc comparisons ----------------------

.extract_wrs2_mcp12 <- function(model) {
  # component of the object containing results from multiple comparisons
  out <- as.data.frame(model$comp)

  # rename to `eaystats` conventions
  names(out)[1:6] <- c("Group1", "Group2", "Psihat", "CI_low", "CI_high", "p")

  # convert names to character
  out$Group1 <- model$fnames[model$comp[, 1]]
  out$Group2 <- model$fnames[model$comp[, 2]]

  # CI column
  out$CI <- .95

  # reorder
  col_order <- c("Group1", "Group2", "Psihat", "CI", "CI_low", "CI_high", "p", "p.crit")
  out <- out[col_order[col_order %in% names(out)]]
  out

  out
}

.extract_wrs2_robtab <- function(model) {
  fcall <- .safe_deparse(model$call)

  # dataframe
  out <- as.data.frame(model$partable)

  # rename to `eaystats` conventions
  if (grepl("^(discmcp\\(|WRS2::discmcp\\()", fcall)) {
    names(out)[1:3] <- c("Group1", "Group2", "p")
  }

  if (grepl("^(discstep\\(|WRS2::discstep\\()", fcall)) {
    names(out)[1:2] <- c("Groups", "p")
  }

  if (grepl("^(binband\\(|WRS2::binband\\()", fcall)) {
    names(out)[1:4] <- c("Value", "Probability1", "Probability2", "Difference")
    if ("p.value" %in% names(out)) {
      out$p <- out$p.value
      out <- subset(out, select = -c(p.value))
    }
  }

  out
}


#' @export
model_parameters.onesampb <- function(model, verbose = TRUE, ...) {
  parameters <- .extract_wrs2_onesampb(model)
  parameters <- .add_htest_parameters_attributes(parameters, model, ...)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}


# WRS2 one-sample percentile bootstrap ----------------------

.extract_wrs2_onesampb <- function(model) {
  data.frame(
    "Estimate" = model$estimate,
    "CI" = .95,
    "CI_low" =  model$ci[1],
    "CI_high" =  model$ci[2],
    "p" = model$p.value,
    "n_Obs" = model$n,
    "Effectsize" = "Robust location measure",
    "Method" = "One-sample percentile bootstrap",
    stringsAsFactors = FALSE
  )
}

#' @export
model_parameters.med1way <- function(model, verbose = TRUE, ...) {
  parameters <- .extract_wrs2_med1way(model)
  parameters <- .add_htest_parameters_attributes(parameters, model, ...)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}

.extract_wrs2_med1way <- function(model) {
  data.frame(
    "F" = model$test,
    "Critical value" = model$crit.val,
    "p" = model$p.value,
    "Method" = "Heteroscedastic one-way ANOVA for medians",
    stringsAsFactors = FALSE
  )
}

# WRS2 one-sample percentile bootstrap ----------------------

#' @export
model_parameters.AKP <- function(model, verbose = TRUE, ...) {
  parameters <- .extract_wrs2_AKP(model)
  parameters <- .add_htest_parameters_attributes(parameters, model, ...)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}


# AKP effect size ----------------------

.extract_wrs2_AKP <- function(model) {
  data.frame(
    "Estimate" = model$AKPeffect,
    "CI" = .95,
    "CI_low" =  model$AKPci[1],
    "CI_high" =  model$AKPci[2],
    "Effectsize" = "Algina-Keselman-Penfield robust standardized difference",
    stringsAsFactors = FALSE
  )
}

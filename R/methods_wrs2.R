#' Parameters from \code{WRS2} objects
#'
#' @param model Object of class \code{t1way}, \code{yuen}, \code{mcp1} or \code{mcp2}.
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


# extract WRS2 anova ----------------------

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


# extract WRS2 ttest ----------------------

.extract_wrs2_yuen <- function(model) {
  fcall <- .safe_deparse(model$call)

  if (grepl("^(yuen\\(|WRS2::yuen\\()", fcall)) {
    data.frame(
      "Difference" = model$diff,
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
  parameters <- .extract_wrs2_mcp(model)
  parameters <- .add_htest_parameters_attributes(parameters, model, ...)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}


#' @export
model_parameters.mcp2 <- model_parameters.mcp1


# extract WRS2 post hoc comparisons ----------------------

.extract_wrs2_mcp <- function(model) {
  # component of the object containing results from multiple comparisons
  out <- as.data.frame(model$comp)

  # rename to `eaystats` conventions
  names(out)[1:5] <- c("Group1", "Group2", "Psihat", "CI_low", "CI_high")

  # convert names to character
  out$Group1 <- model$fnames[model$comp[, 1]]
  out$Group2 <- model$fnames[model$comp[, 2]]

  out
}


#' @export
model_parameters.onesampb <- function(model, verbose = TRUE, ...) {
  parameters <- .extract_wrs2_onesampb(model)
  parameters <- .add_htest_parameters_attributes(parameters, model, ...)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}


# extract WRS2 one-sample percentile bootstrap ----------------------

.extract_wrs2_onesampb <- function(model) {
  data.frame(
    "Estimate" = model$estimate,
    "CI_low" =  model$ci[1],
    "CI_high" =  model$ci[2],
    "p" = model$p.value,
    "n_Obs" = model$n,
    "Effectsize" = "Robust location measure",
    "Method" = "One-sample percentile bootstrap",
    stringsAsFactors = FALSE
  )
}

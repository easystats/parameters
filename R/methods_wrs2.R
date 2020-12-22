#' Parameters from \code{WRS2} objects
#'
#' @param model Object of class \code{t1way}.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' if (require("WRS2")) {
#'   model1 <- t1way(libido ~ dose, data = viagra)
#'   model_parameters(model1)
#'
#'   model2 <- rmanova(WineTasting$Taste, WineTasting$Wine, WineTasting$Taster)
#'   model_parameters(model2)
#' }
#' @return A data frame of indices related to the model's parameters.
#' @export

model_parameters.t1way <- function(model, verbose = TRUE, ...) {
  parameters <- .extract_wrs2_t1way(model)
  parameters <- .add_htest_parameters_attributes(parameters, model, ...)
  class(parameters) <- c("parameters_model", class(parameters))
  parameters
}

# extract WRS2 anova ----------------------

.extract_wrs2_t1way <- function(model) {
  # effect sizes are by default contained for `t1way` but not `rmanova`
  if ("t1way" %in% as.character(model$call)) {
    out <- data.frame(
      "F" = model$test,
      "df" = model$df1,
      "df_error" = model$df2,
      "p" = model$p.value,
      "Effsize" = model$effsize,
      "CI_low" =  model$effsize_ci[1],
      "CI_high" = model$effsize_ci[2],
      "Method" = "A heteroscedastic one-way ANOVA for trimmed means",
      stringsAsFactors = FALSE
    )
  } else if ("rmanova" %in% as.character(model$call)) {
    out <- data.frame(
      "F" = model$test,
      "df" = model$df1,
      "df_error" = model$df2,
      "p" = model$p.value,
      "Method" = "A heteroscedastic one-way repeated measures ANOVA for trimmed means",
      stringsAsFactors = FALSE
    )
  }
}

#' Parameters from \code{WRS2} objects
#'
#' @param model Object of class \code{yuen}.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' if (require("WRS2")) {
#'   library(WRS2)
#'
#'   model1 <- yuen(Anxiety ~ Group, data = spider)
#'   model_parameters(model1)
#'
#'   before <- c(190, 210, 300, 240, 280, 170, 280, 250, 240, 220)
#'   after <- c(210, 210, 340, 190, 260, 180, 200, 220, 230, 200)
#'   model2 <- yuend(before, after)
#'   model_parameters(model2)
#' }
#' @return A data frame of indices related to the model's parameters.
#' @export

model_parameters.yuen <- function(model, verbose = TRUE, ...) {
  parameters <- .extract_wrs2_yuen(model)
  parameters <- .add_htest_parameters_attributes(parameters, model, ...)
  class(parameters) <- c("parameters_model", class(parameters))
  parameters
}


# extract WRS2 ttest ----------------------

.extract_wrs2_yuen <- function(model) {
  if ("yuen" %in% as.character(model$call)) {
    out <- data.frame(
      "Difference" = model$diff,
      "Difference_CI_low" =  model$conf.int[1],
      "Difference_CI_high" =  model$conf.int[2],
      "t" = model$test,
      "df_error" = model$df,
      "p" = model$p.value,
      "Effsize" = model$effsize,
      "Method" = "Yuen's test on trimmed means for independent samples",
      stringsAsFactors = FALSE
    )
  } else if ("yuend" %in% as.character(model$call)) {
    out <- data.frame(
      "Difference" = model$diff,
      "Difference_CI_low" =  model$conf.int[1],
      "Difference_CI_high" =  model$conf.int[2],
      "t" = model$test,
      "df_error" = model$df,
      "p" = model$p.value,
      "Effsize" = model$effsize,
      "Method" = "Yuen's test on trimmed means for dependent samples",
      stringsAsFactors = FALSE
    )
  }
}

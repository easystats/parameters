#' Parameters from \code{WRS2} objects
#'
#' @param model Object of class \code{t1way}.
#' @param ... Arguments passed to or from other methods.
#' @inheritParams model_parameters.default
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
    data.frame(
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

#' Parameters from \code{WRS2} objects
#'
#' @param model Object of class \code{yuen}.
#' @param ... Arguments passed to or from other methods.
#' @inheritParams model_parameters.default
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
    data.frame(
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
    data.frame(
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


#' Parameters from \code{WRS2} objects
#'
#' @param model Object of class \code{mcp1} or \code{mcp2}.
#' @param ... Arguments passed to or from other methods.
#' @inheritParams model_parameters.default
#'
#' @examples
#' if (require("WRS2")) {
#'   library(WRS2)
#'
#'   model1 <- lincon(libido ~ dose, data = viagra)
#'   model_parameters(model1)
#'
#'   model2 <- rmmcp(WineTasting$Taste, WineTasting$Wine, WineTasting$Taster)
#'   model_parameters(model2)
#' }
#' @return A data frame of indices related to the model's parameters.
#' @export

model_parameters.mcp1 <- function(model, verbose = TRUE, ...) {
  parameters <- .extract_wrs2_mcp(model)
  parameters <- .add_htest_parameters_attributes(parameters, model, ...)
  class(parameters) <- c("parameters_model", class(parameters))
  parameters
}

#' @rdname model_parameters.mcp1
#' @export

model_parameters.mcp2 <- model_parameters.mcp1

# extract WRS2 post hoc comparisons ----------------------

.extract_wrs2_mcp <- function(model) {
  # component of the object containing results from multiple comparisons
  out <- as.data.frame(model$comp)

  # rename to `eaystats` conventions
  names(out)[1:5] <- c("Group1", "Group2", "Psihat", "CI_low", "CI_high")

  # convert names to character
  out$Group1 <- as.character(out$Group1)
  out$Group2 <- as.character(out$Group2)

  out
}

#' Parameters from \code{WRS2} objects
#'
#' @param model Object of class \code{t1way}.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' if (require("WRS2")) {
#'   library(WRS2)
#'
#'   set.seed(123)
#'   model <- t1way(libido ~ dose, data = viagra)
#'   model_parameters(model)
#'
#'   set.seed(123)
#'   model <- rmanova(WineTasting$Taste, WineTasting$Wine, WineTasting$Taster)
#'   model_parameters(model)
#' }
#' @return A data frame of indices related to the model's parameters.
#' @export

model_parameters.t1way <- function(model, verbose = TRUE, ...) {
  parameters <- .extract_wrs2_t1way(model, ...)
  class(parameters) <- c("parameters_model", class(parameters))
  parameters
}

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

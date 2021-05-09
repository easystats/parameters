#' Extract group-specific parameters of mixed models
#'
#' Extract parameters of each individual group in the context of mixed models.
#'
#' @inheritParams model_parameters.merMod
#'
#' @examples
#' library(parameters)
#' if (require("lme4")) {
#'   data <- lme4::sleepstudy
#'
#'   # Random intercept
#'   model <- lmer(Reaction ~ Days + (1 | Subject), data = data)
#'   parameters_groupspecific(model)
#'
#'   # Random slope and intercept
#'   model <- lmer(Reaction ~ Days + (1 + Days  | Subject), data = data)
#'   parameters_groupspecific(model)
#'
#'   # Nested random factors
#'   set.seed(12345)
#'   data$grp <- sample(1:5, size = 180, replace = TRUE)
#'   data$subgrp <- NA
#'   for (i in 1:5) {
#'     filter_group <- data$grp == i
#'     data$subgrp[filter_group] <- sample(1:30, size = sum(filter_group), replace = TRUE)
#'   }
#'   model <- lmer(Reaction ~ Days + (1 | grp / subgrp) + (1 | Subject), data = data)
#'   parameters_groupspecific(model)
#'
#' }
#' @export
parameters_groupspecific <- function(model, ...) {
  # Extract params
  out <- model_parameters(model, effects = "random", group_level = TRUE)

  # Assign new class
  class(out) <- c("parmaeters_groupspecific", class(out))
  out
}
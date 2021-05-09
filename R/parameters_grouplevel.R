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
#'   set.seed(33)
#'   data$grp <- sample(letters[1:5], size = 180, replace = TRUE)
#'   data$subgrp <- NA
#'   for (i in letters[1:5]) {
#'     filter_group <- data$grp == i
#'     data$subgrp[filter_group] <- sample(LETTERS, size = sum(filter_group), replace = TRUE)
#'   }
#'   model <- lmer(Reaction ~ Days + (1 | grp / subgrp) + (1 | Subject), data = data)
#'   parameters_groupspecific(model)
#'
#' }
#' @export
parameters_groupspecific <- function(model, ...) {
  # Extract params
  params <- model_parameters(model, effects = "random", group_level = TRUE)
  params <- as.data.frame(params)  # TODO: improve / add new printing that groups by group/level?

  # Reorganize columns
  params$Effects <- NULL
  params <- params[c("Group", "Level", names(params)[!names(params) %in% c("Group", "Level")])]
  params <- params[order(params$Group, params$Level, params$Parameter), ]

  # Clean
  row.names(params) <- NULL

  # Assign new class
  class(params) <- c("parmaeters_groupspecific", class(params))
  params
}
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
#'   random <- parameters_groupspecific(model)
#'   head(random)
#'   summary(random)
#'
#'   # Random slope and intercept
#'   model <- lmer(Reaction ~ Days + (1 + Days  | Subject), data = data)
#'   random <- parameters_groupspecific(model)
#'   head(random)
#'
#'   # Multiple random groups
#'   model <- lmer(mpg ~ wt + (1 | gear) + (1 | carb), data = mtcars)
#'   random <- parameters_groupspecific(model)
#'   head(random)
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
#'   # parameters_groupspecific(model)
#'
#' }
#' @export
parameters_groupspecific <- function(model, indices = c("Coefficient", "SE"), ...) {
  # Extract params
  params <- model_parameters(model, effects = "random", group_level = TRUE)
  params <- as.data.frame(params)  # TODO: improve / add new printing that groups by group/level?

  # Find info
  params_vars <- names(params)[names(params) %in% c("Coefficient", "SE", "SD", "CI_low", "CI_high")]

  # Get original dataframe of random
  df_random <- insight::get_data(model)[insight::find_random(model, flatten = TRUE)]
  for(random_group in names(df_random)) {
    params_subset <- params[params$Group == random_group, ]
    if(nrow(params_subset) == 0) next

    # Clean
    params_subset[[random_group]] <- params_subset$Level
    params_newvars <- paste0(random_group, "_", params_vars)
    names(params_subset)[names(params_subset) %in% params_vars] <- params_newvars

    # Reshape
    params_subset$Parameter <- ifelse(params_subset$Parameter == "(Intercept)",
                                      "Intercept",
                                      params_subset$Parameter)

    wide <- insight::data_to_wide(params_subset[c(random_group, params_newvars, "Parameter")],
                                  rows_from = random_group,
                                  values_from = params_newvars,
                                  colnames_from = "Parameter",
                                  sep = "_")

    df_random <- merge(df_random, wide, by = random_group)
  }


  # Reorganize columns
  params$Effects <- NULL
  params <- params[c("Group", "Level", names(params)[!names(params) %in% c("Group", "Level")])]
  params <- params[order(params$Group, params$Level, params$Parameter), ]

  # Clean
  row.names(params) <- NULL



  # Assign new class
  attr(df_random, "summary") <- params
  class(df_random) <- c("parameters_groupspecific", class(df_random))
  df_random
}


#' @export
summary.parameters_groupspecific <- function(object, ...) {
  attributes(object)$summary
}
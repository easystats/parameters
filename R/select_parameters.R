#' Automated selection of model parameters
#'
#' This function performs an automated selection of the 'best' parameters,
#' updating and returning the "best" model.
#'
#' @param model A statistical model (of class `lm`, `glm`,
#'   `merMod`, `stanreg` or `brmsfit`).
#' @param ... Arguments passed to or from other methods.
#'
#' @details
#'   \subsection{Classical lm and glm}{
#'     For frequentist GLMs, `select_parameters()` performs an AIC-based
#'     stepwise selection.
#'   }
#'
#'   \subsection{Mixed models}{
#'     For mixed-effects models of class `merMod`, stepwise selection is
#'     based on [cAIC4::stepcAIC()]. This step function
#'     only searches the "best" model based on the random-effects structure,
#'     i.e. `select_parameters()` adds or excludes random-effects until
#'     the cAIC can't be improved further.
#'   }
#'
#'   \subsection{Bayesian models}{
#'     For Bayesian models, it uses the \pkg{projpred} package.
#'   }
#'
#' @examples
#' model <- lm(mpg ~ ., data = mtcars)
#' select_parameters(model)
#'
#' model <- lm(mpg ~ cyl * disp * hp * wt, data = mtcars)
#' select_parameters(model)
#' \donttest{
#' # lme4 -------------------------------------------
#' if (require("lme4")) {
#'   model <- lmer(
#'     Sepal.Width ~ Sepal.Length * Petal.Width * Petal.Length + (1 | Species),
#'     data = iris
#'   )
#'   select_parameters(model)
#' }
#' }
#'
#' \dontrun{
#' # rstanarm -------------------------------------------
#' if (require("rstanarm") && require("projpred")) {
#'   model <- stan_glm(
#'     mpg ~ .,
#'     data = mtcars,
#'     iter = 500, refresh = 0, verbose = FALSE
#'   )
#'   select_parameters(model, cross_validation = TRUE)
#'
#'   model <- stan_glm(
#'     mpg ~ cyl * disp * hp,
#'     data = mtcars,
#'     iter = 500, refresh = 0, verbose = FALSE
#'   )
#'   select_parameters(model, cross_validation = FALSE)
#' }
#' }
#'
#' @return The model refitted with optimal number of parameters.
#' @export
select_parameters <- function(model, ...) {
  UseMethod("select_parameters")
}


#' @rdname select_parameters
#' @inheritParams stats::step
#' @export
select_parameters.lm <- function(model,
                                 direction = "both",
                                 steps = 1000,
                                 k = 2,
                                 ...) {
  junk <- utils::capture.output(best <- stats::step(model,
    trace = 0,
    direction = direction,
    steps = steps,
    k = k,
    ...
  ))

  best
}



#' @rdname select_parameters
#' @export
select_parameters.merMod <- function(model,
                                     direction = "backward",
                                     steps = 1000,
                                     ...) {
  insight::check_if_installed("cAIC4")

  # Find slope and group candidates
  # data <- insight::get_data(model)
  # factors <- names(data[sapply(data, is.factor)])
  # if(length(factors) == 0){
  #   factors <- NULL
  # }
  # nums <- names(data[sapply(data, is.numeric)])
  # if(length(nums) == 0){
  #   nums <- NULL
  # }

  factors <- unique(c(
    insight::find_random(model, split_nested = FALSE, flatten = TRUE),
    insight::find_random(model, split_nested = TRUE, flatten = TRUE)
  ))
  factors <- gsub(":", "/", factors, fixed = TRUE)


  best <- suppressMessages(suppressWarnings(cAIC4::stepcAIC(model,
    # slopeCandidates = nums,
    groupCandidates = factors,
    direction = direction,
    steps = steps,
    allowUseAcross = TRUE
  )$finalModel))


  # Using MuMIn's dredge(): works nicely BUT throws unnecessary warnings and requires to set global options for na.action even tho no NaNs.
  # The code is here: https://github.com/cran/MuMIn/blob/master/R/dredge.R Maybe it could be reimplemented?
  # insight::check_if_installed("MuMIn")
  # model <- lmer(Sepal.Width ~ Sepal.Length * Petal.Width * Petal.Length + (1 | Species), data = iris, na.action = na.fail)
  # summary(MuMIn::get.models(MuMIn::dredge(model), 1)[[1]])

  best
}

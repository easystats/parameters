#' Automated selection of model parameters
#'
#' This function performs an automated selection of the 'best' parameters, updating and returning the "best" model. For frequentist simple GLMs, it performs an AIC-based stepwise selection. For Bayesian models, it uses the \code{projpred} package.
#'
#' @param model A statistical model.
#' @param ... Arguments passed to or from other methods.
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
#'
#' # rstanarm -------------------------------------------
#' if (require("rstanarm")) {
#'   model <- stan_glm(
#'     mpg ~ ., data = mtcars,
#'     iter = 500, refresh = 0, verbose = FALSE
#'   )
#'   select_parameters(model, cross_validation = TRUE)
#'
#'   model <- stan_glm(
#'     mpg ~ cyl * disp * hp, data = mtcars,
#'     iter = 500, refresh = 0, verbose = FALSE
#'   )
#'   select_parameters(model, cross_validation = FALSE)
#' }
#' }
#' @return The model refitted with optimal number of parameters.
#' @export
select_parameters <- function(model, ...) {
  UseMethod("select_parameters")
}

#' @rdname select_parameters
#' @export
parameters_selection <- select_parameters



#' @rdname select_parameters
#' @inheritParams stats::step
#' @importFrom stats step
#' @export
select_parameters.lm <- function(model, direction = "both", steps = 1000, k = 2, ...) {
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
select_parameters.merMod <- function(model, direction = "backward", steps = 1000, ...) {

  # Using cAIC4's stepcAIC()
  if (!requireNamespace("cAIC4", quietly = TRUE)) {
    stop("Package 'cAIC4' required for this function to work. Please install it by running `install.packages('cAIC4')`.")
  }

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

  best <- cAIC4::stepcAIC(model,
    # groupCandidates = factors,
    # slopeCandidates = nums,
    direction = direction,
    steps = steps,
    allowUseAcross = TRUE
  )$finalModel


  # Using MuMIn's dredge(): works nicely BUT throws unnecessary warnings and requires to set global options for na.action even tho no NaNs.
  # The code is here: https://github.com/cran/MuMIn/blob/master/R/dredge.R Maybe it could be reimplemented?
  # if (!requireNamespace("MuMIn", quietly = TRUE)) {
  #   stop("Package 'MuMIn' required for this function to work. Please install it by running `install.packages('MuMIn')`.")
  # }
  # model <- lmer(Sepal.Width ~ Sepal.Length * Petal.Width * Petal.Length + (1 | Species), data = iris, na.action = na.fail)
  # summary(MuMIn::get.models(MuMIn::dredge(model), 1)[[1]])

  best
}

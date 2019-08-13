#' Parameters Selection
#'
#' This function performs an automated selection of the 'best' parameters, udpating and returning the "best" model. For frequentist simple GLMs, it performs an AIC-based stepwise selection. For Bayesian models, it uses the \code{projpred} package.
#'
#' @param model A statistical model.
#' @param ... Arguments passed to or from other methods.
#'
#'
#' @examples
#' model <- lm(mpg ~ ., data = mtcars)
#' parameters_selection(model)
#'
#' model <- lm(mpg ~ cyl * disp * hp * wt, data = mtcars)
#' parameters_selection(model)
#' \donttest{
#' # lme4 -------------------------------------------
#' library(lme4)
#' model <- lmer(Sepal.Width ~ Sepal.Length * Petal.Width * Petal.Length + (1 | Species), data = iris)
#' parameters_selection(model)
#'
#' # rstanarm -------------------------------------------
#' library(rstanarm)
#' model <- stan_glm(mpg ~ ., data = mtcars, iter = 500, refresh = 0)
#' parameters_selection(model, cross_validation = TRUE)
#'
#' model <- stan_glm(mpg ~ cyl * disp * hp, data = mtcars)
#' parameters_selection(model, cross_validation = FALSE)
#' }
#'
#' @return The model refitted with optimal number of parameters.
#'
#' @export
parameters_selection <- function(model, ...) {
  UseMethod("parameters_selection")
}



#' @rdname parameters_selection
#' @inheritParams stats::step
#' @importFrom stats step
#' @export
parameters_selection.lm <- function(model, direction = "both", steps = 1000, k = 2, ...) {
  junk <- capture.output(best <- step(model,
    trace = 0,
    direction = direction,
    steps = steps,
    k = k,
    ...
  ))

  best
}









#' @rdname parameters_selection
#' @export
parameters_selection.merMod <- function(model, direction = "backward", steps = 1000, ...) {

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








#' @param method The method used in the variable selection. Can be NULL (default), "forward" or "L1". See \code{projpred::varsel}.
#' @param cross_validation Select with cross-validation.
#' @rdname parameters_selection
#' @export
parameters_selection.stanreg <- function(model, method = NULL, cross_validation = FALSE, ...) {
  if (!requireNamespace("projpred", quietly = TRUE)) {
    stop("Package 'projpred' required for this function to work. Please install it by running `install.packages('projpred')`.")
  }

  if (cross_validation) {
    message("Cross-validating best parameters...")
    junk <- capture.output(selection <- projpred::cv_varsel(model, method = method), ...)
  } else {
    selection <- projpred::varsel(model, method = method, ...)
  }

  # Visualise
  # varsel_plot(selection, stats = c('elpd', 'rmse'), deltas=T)

  # Extract parameters
  projection <- projpred::project(selection, nv = projpred::suggest_size(selection), ...)
  parameters <- row.names(projection$beta)

  # Reconstruct formula
  formula <- .reconstruct_formula(parameters, model)

  # Update model
  junk <- capture.output(best <- update(model, formula = formula, ...))
  best
}






#' @keywords internal
.reconstruct_formula <- function(parameters, model) {

  # # Clean
  # if (tail(parameters, 1) == "sigma") {
  #   parameters <- parameters[1:length(parameters) - 1]
  # }
  # if (parameters[1] == "(Intercept)") {
  #   parameters <- parameters[2:length(parameters)]
  # }
  #
  # # Detect interactions
  # interactions <- parameters[grepl(":", parameters)]
  # if (length(interactions) > 0) {
  #   for (interaction in interactions) {
  #     terms <- unlist(strsplit(interaction, ":", fixed = TRUE))
  #     if (length(terms) == 2) {
  #       if (all(terms %in% parameters)) {
  #         # replace interactions components by interactions
  #         parameters <- parameters[!parameters %in% c(terms, interaction)]
  #         parameters <- c(parameters, paste0(terms, collapse = " * "))
  #       }
  #     }
  #   }
  # }


  formula <- paste(parameters, collapse = " + ")
  formula <- paste(insight::find_response(model), "~", formula)
  formula
}

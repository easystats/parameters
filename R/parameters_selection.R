#' Parameters Selection
#'
#' Automated selection of the 'best' parameters. For frequentist simple GLMs, it performes an AIC-based stepwise selection. For Bayesian models, it uses the \code{projpred} package.
#'
#' @param model A statistical model.
#' @param ... Arguments passed to or from other methods.
#'
#'
#' @examples
#' model <- lm(mpg ~ ., data = mtcars)
#' parameters_selection(model)
#'
#' model <- lm(mpg ~ cyl * disp * hp * wt , data = mtcars)
#' parameters_selection(model)
#'
#' \dontrun{
#' # lme4 -------------------------------------------
#' library(lme4)
#' model <- lmer(Sepal.Width ~ Sepal.Length * Petal.Width * Petal.Length + (1|Species), data = iris)
#' parameters_selection(model)
#'
#' # rstanarm -------------------------------------------
#' library(rstanarm)
#' model <- stan_glm(mpg ~ ., data = mtcars)
#' parameters_selection(model, cross_validation = TRUE)
#'
#' model <- stan_glm(mpg ~ cyl * disp * hp, data = mtcars)
#' parameters_selection(model, cross_validation = FALSE)
#' }
#'
#'
#' @export
parameters_selection <- function(model, ...){
  UseMethod("parameters_selection")
}



#' @rdname parameters_selection
#' @importFrom stats step
#' @export
parameters_selection.lm <- function(model, ...){
  junk <- capture.output(best <- step(model, ...))

  parameters <- names(best$coefficients)
  formula <- .reconstruct_formula(parameters)
  formula
}









#' @rdname parameters_selection
#' @export
parameters_selection.merMod <- function(model, ...){
  if (!requireNamespace("cAIC4", quietly = TRUE)) {
    stop("Package `cAIC4` required. Please install it by running `install.packages(cAIC4)`.", call. = FALSE)
  }

  best <- cAIC4::stepcAIC(model)
  # TODO: Need to improve reconstruct formula to also account for random effects and all
  parameters <- insight::find_parameters(best$finalModel, effects = "fixed", flatten = TRUE)

  formula <- .reconstruct_formula(parameters)
  formula
}








#' @param method The method used in the variable selection. Can be NULL (default), "forward" or "L1". See \code{projpred::varsel}.
#' @param cross_validation Select with cross-validation.
#' @rdname parameters_selection
#' @export
parameters_selection.stanreg <- function(model, method=NULL, cross_validation = FALSE, ...){
  if (!requireNamespace("projpred", quietly = TRUE)) {
    stop("Package `projpred` required. Please install it by running `install.packages(projpred)`.", call. = FALSE)
  }

  if(cross_validation){
    message("Cross-validating best parameters...")
    junk <- capture.output(selection <- projpred::cv_varsel(model, method=method), ...)
  } else{
    selection <- projpred::varsel(model, method=method, ...)
  }

  # Visualise
  # varsel_plot(selection, stats = c('elpd', 'rmse'), deltas=T)

  # Extract parameters
  projection <- projpred::project(selection, nv = projpred::suggest_size(selection), ...)
  parameters <- names(as.data.frame(as.matrix(projection)))

  formula <- .reconstruct_formula(parameters)
  formula
}






#' @keywords internal
.reconstruct_formula <- function(parameters){

  # Clean
  if(tail(parameters, 1) == "sigma"){
    parameters <- parameters[1:length(parameters)-1]
  }
  if(parameters[1] == "(Intercept)"){
    parameters <- parameters[2:length(parameters)]
  }

  # Detect interactions
  interactions <- parameters[grepl(":", parameters)]
  if(length(interactions) > 0){
    for(interaction in interactions){
      terms <- unlist(strsplit(interaction, ":", fixed = TRUE))
      if(length(terms) == 2){
        if(all(terms %in% parameters)){
          # replace interactions components by interactions
          parameters <- parameters[!parameters %in% c(terms, interaction)]
          parameters <- c(parameters, paste0(terms, collapse = " * "))
        }
      }
    }
  }


  formula <- paste(parameters, collapse = " + ")
  formula
}
#' Parameters Selection
#'
#' Automated selection of the 'best' parameters.
#'
#' @param model A statistical model
#' @param method The method used in the variable selection. Can be NULL (default), "forward" or "L1". See \code{projpred::varsel}.
#' @param cross_validation Select with cross-validation.
#'
#' @examples
#' \dontrun{
#' library(rstanarm)
#' model <- stan_glm(mpg ~ ., data = mtcars, chains = 2, iter = 200)
#' parameters_selection(model)
#'
#' model <- stan_glm(mpg ~ cyl * disp * hp * drat, data = mtcars, chains = 2, iter = 200)
#' parameters_selection(model, cross_validation = TRUE)
#' }
#'
#'
#'
#' @export
parameters_selection <- function(model, method=NULL, cross_validation = FALSE, ...){
  if (!requireNamespace("projpred", quietly = TRUE)) {
    stop("Package `projpred` required. Please install it by running `install.packages(projpred)`.", call. = FALSE)
  }

  if(cross_validation){
    message("Cross-validating best parameters...")
    junk <- capture.output(selection <- projpred::cv_varsel(model, method=method))
  } else{
    selection <- projpred::varsel(model, method=method)
  }

  # Visualise
  # varsel_plot(selection, stats = c('elpd', 'rmse'), deltas=T)

  # Extract parameters
  projection <- projpred::project(selection, nv = projpred::suggest_size(selection))
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


  formula <- paste(parameters, collapse = " + ")
  formula
}
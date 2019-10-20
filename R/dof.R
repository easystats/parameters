#' Degrees of Freedom (DoF)
#'
#' Estimate or extract degrees of freedom of models.
#'
#' @param model A statistical model.
#' @param method Can be 'analytical' (default, DoFs are estimated based on the model type) or 'fit'. In which case they are directly taken from the model if available (for Bayesian models, the goal (looking for help to make it happen) would be to refit the model as a frequentist one before extracting the DoFs).
#' @param data Data used by the model. If \code{NULL}, will try to extract it from the model.
#'
#' @examples
#' model <- lm(Sepal.Length ~ Petal.Length * Species, data = iris)
#' dof(model)
#' model <- glm(vs ~ mpg * cyl, data = mtcars, family = "binomial")
#' dof(model)
#'
#' library(lme4)
#' model <- lme4::lmer(Sepal.Length ~ Petal.Length + (1|Species), data = iris)
#' dof(model)
#'
#' \donttest{
#' library(rstanarm)
#' model <- stan_glm(
#'   Sepal.Length ~ Petal.Length * Species,
#'   data = iris,
#'   chains = 2,
#'   refresh = 0
#' )
#' dof(model)
#' }
#'
#'
#'
#'
#' @export
degrees_of_freedom <- function(model, method = "analytical", data = NULL){

  if (method == "analytical") {
    dof <- .degrees_of_freedom_analytical(model, data = NULL)
  } else{
    dof <- .degrees_of_freedom_fit(model)
  }
  dof
}

#' @rdname degrees_of_freedom
#' @export
dof <- degrees_of_freedom









#' @keywords internal
.degrees_of_freedom_analytical <- function(model, data = NULL){
  if (is.null(data)) {
    data <- insight::get_data(model)
  }

  info <- insight::model_info(model)
  nparam <- n_parameters(model)
  n <- nrow(data)

  if (info$is_mixed) {
    if (info$is_bayesian) {
      stop("Cannot estimate DoFs for Bayesian mixed models yet.")
    } else{
      dof <- as.numeric(t(dof_kenward(model)))
    }
  } else{
    dof <- rep(n - nparam, nparam)
  }

  dof
}





#' @keywords internal
.degrees_of_freedom_fit <- function(model){
  info <- insight::model_info(model)

  if (info$is_bayesian) {
    # model <- bayestestR::refit_as_frequentist(model)
    stop("Method 'fit' is not yet available.")
  }

  dof <- model_parameters(model)$df_residual
  dof
}

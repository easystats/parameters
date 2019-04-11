#' Model Standardization
#'
#' Standardize the model's parameters.
#'
#' @inheritParams standardize
#' @param method The method used for standardizing the parameters. Can be "refit" (default).
#'
#' @details Methods:
#' \itemize{
#'  \item \emph{refit}: This method is based on a complete model re-fit using the standardized data. It is the most accurate, especially for parameters related to interactions, but it is also the most computationnaly costly.
#'  }
#'
#' @examples
#' model <- lm(Sepal.Length ~ Species * Petal.Width, data = iris)
#' coef(standardize(model))
#' @importFrom stats update
#' @importFrom insight get_data model_info find_response
#' @importFrom utils capture.output
#' @export
standardize.lm <- function(x, robust = FALSE, method = "refit", ...) {
  # TODO: add other methods
  if (method == "refit") {
    data <- insight::get_data(x)
    if (insight::model_info(x)$is_binomial) {
      data[insight::find_response(x)] <- as.factor(insight::get_response(x))
    }
    if (inherits(x, c("brmsfit"))) {
      text <- capture.output(model_std <- update(x, newdata = standardize(data, robust = robust)))
    } else {
      text <- capture.output(model_std <- update(x, data = standardize(data, robust = robust)))
    }

    return(model_std)
  }
}

#' @export
standardize.merMod <- standardize.lm

#' @export
standardize.stanreg <- standardize.lm

#' @export
standardize.brmsfit <- standardize.lm








#' #' @keywords
#' .standardize_priors <- function(x, robust = FALSE){
#'
#'   priors <- insight::get_priors(x)
#'
#'   if(insight::model_info(x)$is_linear){
#'
#'     if(robust == FALSE){
#'       disp_y <- sd(insight::get_response(x))
#'       loc_y <- mean(insight::get_response(x))
#'
#'       disp_x <- sapply(insight::get_predictors(x), sd)
#'       loc_x <- sapply(insight::get_predictors(x), mean)
#'     } else{
#'       disp_y <- mad(insight::get_response(x))
#'       loc_y <- median(insight::get_response(x))
#'
#'       disp_x <- sapply(insight::get_predictors(x), mad)
#'       loc_x <- sapply(insight::get_predictors(x), median)
#'     }
#'
#'     # Intercept
#'     prior_intercept <- priors[priors$parameter=="(Intercept)",]
#'     prior_intercept$scale <- prior_intercept$scale/disp_y
#'     prior_intercept <- .format_priors(prior_intercept)
#'
#'     # Effects
#'     prior_params <- priors[priors$parameter!="(Intercept)",]
#'     prior_params <- merge(prior_params,
#'                           data.frame("parameter" = names(disp_x),
#'                                      "Disp_x" = disp_x))
#'     prior_params <- merge(prior_params,
#'                           data.frame("parameter" = names(loc_x),
#'                                      "Loc_x" = loc_x))
#'     prior_params$scale <- prior_params$scale*prior_params$Disp_x/disp_y
#'     prior_params$location <- prior_params$location-prior_params$Loc_x/loc_y
#'     prior_params <- .format_priors(prior_params, force_autoscale=TRUE)
#'   }
#'
#'   return(list("prior_intercept" = prior_intercept,
#'               "prior" = prior_params))
#' }
#'
#'
#' .format_priors <- function(priors, force_autoscale=FALSE){
#'   if(force_autoscale == FALSE){
#'     priors$autoscale <- ifelse(!is.na(priors$adjusted_scale), TRUE, FALSE)
#'   } else{
#'     priors$autoscale <- TRUE
#'   }
#'
#'   priors <- list(
#'     "dist" = as.character(priors$distribution),
#'     "location" = priors$location,
#'     "scale" = priors$scale,
#'     "autoscale" = priors$autoscale)
#'   priors
#' }
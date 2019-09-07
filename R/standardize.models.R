#' Model Standardization
#'
#' Refit the model after standardizing the data.
#'
#' @inheritParams standardize
#'
#'
#' @examples
#' model <- lm(Sepal.Length ~ Species * Petal.Width, data = iris)
#' coef(standardize(model))
#' @return The model fitted on standardized data.
#'
#' @importFrom stats update
#' @importFrom insight get_data model_info find_response
#' @importFrom utils capture.output
#' @export
standardize.lm <- function(x, robust = FALSE, method = "default", verbose = TRUE, ...) {
  data <- insight::get_data(x)

  if (insight::model_info(x)$is_binomial) {
    data[insight::find_response(x)] <- as.factor(insight::get_response(x))
  }

  if (inherits(x, c("brmsfit"))) {
    text <- utils::capture.output(model_std <- stats::update(x, newdata = standardize(data, robust = robust, method = method, verbose = verbose)))
  } else {
    text <- utils::capture.output(model_std <- stats::update(x, data = standardize(data, robust = robust, method = method, verbose = verbose)))
  }

  model_std
}

#' @export
standardize.merMod <- standardize.lm

#' @export
standardize.stanreg <- standardize.lm

#' @export
standardize.brmsfit <- standardize.lm

#' @export
standardize.lme <- standardize.lm

#' @export
standardize.gls <- standardize.lm

#' @export
standardize.plm <- standardize.lm

#' @export
standardize.feis <- standardize.lm

#' @export
standardize.wbm <- function(x, ...) {
  warning("Standardization of parameters not possible for models from package 'panelr'.", call. = FALSE)
  x
}

#' @export
standardize.clm <- standardize.wbm

#' @export
standardize.clm2 <- standardize.wbm


#' @export
standardize.zeroinfl <- function(x, robust = FALSE, method = "default", verbose = TRUE, ...) {
  # dont standardize outcome!
  d <- standardize(insight::get_data(x), robust = robust, method = method, verbose = verbose)
  d[[insight::find_response(x)]] <- insight::get_response(x)

  text <- utils::capture.output(model_std <- stats::update(x, data = d))

  model_std
}

#' @export
standardize.hurdle <- standardize.zeroinfl

#' @export
standardize.zerocount <- standardize.zeroinfl







# models with special handling of response variables ---------------------------


#' @export
standardize.coxph <- function(x, robust = FALSE, method = "default", verbose = TRUE, ...) {

  # for some models, the DV cannot be standardized when using
  # "update()", so we only standardize model predictors

  pred <- insight::find_predictors(x, flatten = TRUE)
  data <- insight::get_data(x)

  data_std <- standardize(data[, pred, drop = FALSE], robust = robust, method = method, verbose = verbose)
  data[pred] <- data_std

  text <- utils::capture.output(model_std <- stats::update(x, data = data))

  model_std
}



#' @importFrom insight find_response get_response get_data
#' @export
standardize.betareg <- function(x, robust = FALSE, method = "default", verbose = TRUE, ...) {

  # for some models, the DV cannot be standardized when using
  # "update()", so we only standardize model predictors

  resp <- unique(c(insight::find_response(x), insight::find_response(x, combine = FALSE)))
  data <- insight::get_data(x)

  data_std <- standardize(data, robust = robust, method = method, verbose = verbose)
  data_std[resp] <- data[resp]

  text <- utils::capture.output(model_std <- stats::update(x, data = data_std))

  model_std
}

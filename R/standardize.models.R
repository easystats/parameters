#' Model Standardization
#'
#' Refit the model after standardizing the data.
#'
#' @param include_response Logical, if \code{TRUE} (default), the response value
#'   will also be standardized. For certain models (logistic regression,
#'   count models, ...), the response value will never be standardized, to make
#'   re-fitting the model work.
#' @inheritParams standardize
#'
#'
#' @examples
#' model <- lm(Sepal.Length ~ Species * Petal.Width, data = iris)
#' coef(standardize(model))
#' @return The model fitted on standardized data.
#'
#' @importFrom stats update
#' @importFrom insight get_data model_info find_response get_response
#' @importFrom utils capture.output
#' @export
standardize.lm <- function(x, robust = FALSE, method = "default", include_response = TRUE, verbose = TRUE, ...) {
  m_info <- insight::model_info(x)
  data <- insight::get_data(x)
  resp <- NULL

  # for models with binary outcome, make sure response is factor,
  # so it's not standardized.

  if (m_info$is_binomial) {
    data[insight::find_response(x)] <- as.factor(insight::get_response(x))
  }


  # for models with specific scale of the response value (e.g. count models
  # with positive integers, or beta with ratio between 0 and 1), we need to
  # make sure that the original response value will be restored after
  # standardizing, as these models also require a non-standardized reponse.

  if (m_info$is_count || m_info$is_beta || m_info$is_censored || !include_response) {
    resp <- unique(c(insight::find_response(x), insight::find_response(x, combine = FALSE)))
  }


  # if we standardize log-terms, standardization will fail (because log of
  # negative value is NaN)

  log_terms <- .log_terms(x)

  # standardize data

  data_std <- standardize(data, robust = robust, method = method, verbose = verbose)


  # restore data that should not be standardized

  if (!is.null(resp)) data_std[resp] <- data[resp]
  if (length(log_terms)) data_std[log_terms] <- data[log_terms]


  # update model with standardized data

  if (inherits(x, c("brmsfit"))) {
    text <- utils::capture.output(model_std <- stats::update(x, newdata = data_std))
  } else {
    text <- utils::capture.output(model_std <- stats::update(x, data = data_std))
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
standardize.negbin <- standardize.lm

#' @export
standardize.betareg <- standardize.lm

#' @export
standardize.ivreg <- standardize.lm

#' @export
standardize.truncreg <- standardize.lm

#' @export
standardize.tobit <- standardize.lm

#' @export
standardize.censReg <- standardize.lm

#' @export
standardize.lrm <- standardize.lm

#' @export
standardize.psm <- standardize.lm

#' @export
standardize.ols <- standardize.lm

#' @export
standardize.geeglm <- standardize.lm

#' @export
standardize.gee <- standardize.lm

#' @export
standardize.rms <- standardize.lm

#' @export
standardize.logistf <- standardize.lm

#' @export
standardize.vglm <- standardize.lm

#' @export
standardize.wbm <- function(x, ...) {
  warning("Standardization of parameters not possible for models from package 'panelr'.", call. = FALSE)
  x
}

#' @export
standardize.clm <- standardize.wbm

#' @export
standardize.clm2 <- standardize.wbm







# Zero-Inflated models -------------------------------------------------------


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


#' @export
standardize.coxme <- standardize.coxph

#' @rdname standardize
#' @importFrom stats update
#' @importFrom insight get_data model_info find_response get_response find_weights
#' @importFrom utils capture.output
#' @export
standardize.lm <- function(x, robust = FALSE, method = "default", include_response = TRUE, verbose = TRUE, ...) {
  m_info <- insight::model_info(x)
  data <- insight::get_data(x)
  resp <- NULL

  # for models with specific scale of the response value (e.g. count models
  # with positive integers, or beta with ratio between 0 and 1), we need to
  # make sure that the original response value will be restored after
  # standardizing, as these models also require a non-standardized reponse.

  if (.no_response_standardize(m_info) || !include_response) {
    resp <- unique(c(insight::find_response(x), insight::find_response(x, combine = FALSE)))
  }


  # if we standardize log-terms, standardization will fail (because log of
  # negative value is NaN)

  log_terms <- .log_terms(x)

  # Do not standardize weighting-variable, because negative weights will
  # cause errors in "update()"

  weight_variable <- insight::find_weights(x)

  # standardize data

  dont_standardize <- c(resp, log_terms, weight_variable)
  do_standardize <- setdiff(colnames(data), dont_standardize)

  if (length(do_standardize)) {
    data_std <- standardize(data[do_standardize], robust = robust, method = method, verbose = verbose)
  } else {
    if (verbose) {
      insight::print_color("No variables could be standardized.\n", "red")
    }
    return(x)
  }


  # restore data that should not be standardized

  if (length(dont_standardize)) {
    data_std <- cbind(data[, dont_standardize, drop = FALSE], data_std)
  }

  # update model with standardized data

  if (inherits(x, "brmsfit")) {
    text <- utils::capture.output(model_std <- stats::update(x, newdata = data_std))
  } else if (inherits(x, "biglm")) {
    text <- utils::capture.output(model_std <- stats::update(x, moredata = data_std))
  } else {
    text <- utils::capture.output(model_std <- stats::update(x, data = data_std))
  }

  model_std
}




#' @keywords internal
.no_response_standardize <- function(info) {
  # check if model has a response variable that should not be standardized.
  info$is_count | info$is_ordinal | info$is_beta | info$is_censored | info$is_binomial | info$is_survival
}




#' @export
standardize.mlm <- function(x, robust = FALSE, method = "default", verbose = TRUE, ...) {
  standardize.lm(x = x, robust = robust, method = method, include_response = FALSE, verbose = verbose, ...)
}

#' @export
standardize.merMod <- standardize.lm

#' @export
standardize.rq <- standardize.lm

#' @export
standardize.crq <- standardize.lm

#' @export
standardize.nlrq <- standardize.lm

#' @export
standardize.speedglm <- standardize.lm

#' @export
standardize.speedlm <- standardize.lm

#' @export
standardize.iv_robust <- standardize.lm

#' @export
standardize.lmrob <- standardize.lm

#' @export
standardize.glmrob <- standardize.lm

#' @export
standardize.glmRob <- standardize.lm

#' @export
standardize.lmRob <- standardize.lm

#' @export
standardize.MixMod <- standardize.lm

#' @export
standardize.glmmTMB <- standardize.lm

#' @export
standardize.stanreg <- standardize.lm

#' @export
standardize.brmsfit <- standardize.lm

#' @export
standardize.flexsurvreg <- standardize.lm

#' @export
standardize.lme <- standardize.lm

#' @export
standardize.biglm <- standardize.lm

#' @export
standardize.LORgee <- standardize.lm

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
standardize.lm_robust <- standardize.lm

#' @export
standardize.tobit <- standardize.lm

#' @export
standardize.censReg <- standardize.lm

#' @export
standardize.crch <- standardize.lm

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
standardize.clm <- standardize.lm

#' @export
standardize.wbm <- function(x, ...) {
  warning("Standardization of parameters not possible for models from package 'panelr'.", call. = FALSE)
  x
}

#' @export
standardize.clm2 <- standardize.wbm







# Zero-Inflated models -------------------------------------------------------


#' @export
standardize.zeroinfl <- standardize.lm

#' @export
standardize.hurdle <- standardize.lm

#' @export
standardize.zerocount <- standardize.lm







# models with special handling of response variables ---------------------------


#' @export
standardize.coxph <- function(x, robust = FALSE, method = "default", verbose = TRUE, ...) {

  # for some models, the DV cannot be standardized when using
  # "update()", so we only standardize model predictors
  #
  # survival models have some strange format for the response variable,
  # so we don't use the default standardize.lm function here, but
  # use a different approach that only retrieves predictors that should
  # be standardized.

  pred <- insight::find_predictors(x, flatten = TRUE)
  data <- insight::get_data(x)

  # if we standardize log-terms, standardization will fail (because log of
  # negative value is NaN)

  log_terms <- .log_terms(x)
  if (length(log_terms)) pred <- setdiff(pred, log_terms)

  weight_variable <- insight::find_weights(x)
  if (length(weight_variable)) pred <- setdiff(pred, weight_variable)

  # standardize data, if we have anything left to standardize

  if (length(pred)) {
    data_std <- standardize(data[, pred, drop = FALSE], robust = robust, method = method, verbose = verbose)
    data[pred] <- data_std
  }

  text <- utils::capture.output(model_std <- stats::update(x, data = data))

  model_std
}


#' @export
standardize.coxme <- standardize.coxph

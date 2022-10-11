#' @rdname standardize_parameters
#' @export
#' @aliases standardise_posteriors
standardize_posteriors <- function(model,
                                   method = "refit",
                                   robust = FALSE,
                                   two_sd = FALSE,
                                   include_response = TRUE,
                                   verbose = TRUE,
                                   ...) {
  object_name <- insight::safe_deparse_substitute(model)

  m_info <- .get_model_info(model, ...)
  include_response <- include_response && .safe_to_standardize_response(m_info, verbose = verbose)

  if (method == "refit") {
    model <- datawizard::standardize(
      model,
      robust = robust,
      two_sd = two_sd,
      include_response = include_response,
      verbose = verbose,
      m_info = m_info
    )
  }

  pars <- insight::get_parameters(model)


  if (method %in% c("posthoc", "smart", "basic", "classic", "pseudo")) {
    pars <- .standardize_posteriors_posthoc(pars, method, model, m_info, robust, two_sd, include_response, verbose)

    method <- attr(pars, "std_method")
    robust <- attr(pars, "robust")
  }

  ## attributes
  attr(pars, "std_method") <- method
  attr(pars, "two_sd") <- two_sd
  attr(pars, "robust") <- robust
  attr(pars, "include_response") <- include_response
  attr(pars, "object_name") <- object_name
  class(pars) <- c("parameters_standardized", class(pars))

  pars
}

#' @export
standardise_posteriors <- standardize_posteriors


#' @keywords internal
.standardize_posteriors_posthoc <- function(pars, method, model, mi, robust, two_sd, include_response, verbose) {
  # Sanity Check for "pseudo"
  method <- .should_pseudo(method, model)

  method <- .cant_smart_or_posthoc(method, model, mi, pars$Parameter)

  if (robust && method == "pseudo") {
    warning("`robust` standardization not available for `pseudo` method.",
      call. = FALSE
    )
    robust <- FALSE
  }

  ## Get scaling factors
  deviations <- standardize_info(
    model,
    robust = robust,
    include_pseudo = method == "pseudo",
    two_sd = two_sd,
    model_info = mi
  )
  i <- match(deviations$Parameter, colnames(pars))
  pars <- pars[, i]

  if (method == "basic") {
    col_dev_resp <- "Deviation_Response_Basic"
    col_dev_pred <- "Deviation_Basic"
  } else if (method == "posthoc") {
    col_dev_resp <- "Deviation_Response_Basic"
    col_dev_pred <- "Deviation_Smart"
  } else if (method == "smart") {
    col_dev_resp <- "Deviation_Response_Smart"
    col_dev_pred <- "Deviation_Smart"
  } else if (method == "pseudo") {
    col_dev_resp <- "Deviation_Response_Pseudo"
    col_dev_pred <- "Deviation_Pseudo"
  } else {
    insight::format_error("`method` must be one of \"basic\", \"posthoc\", \"smart\" or \"pseudo\".")
  }

  .dev_pred <- deviations[[col_dev_pred]]
  .dev_resp <- deviations[[col_dev_resp]]
  if (!include_response) .dev_resp <- 1
  .dev_factor <- .dev_pred / .dev_resp

  # Sapply standardization
  pars <- t(t(pars) * .dev_factor)
  pars <- as.data.frame(pars)

  attr(pars, "std_method") <- method
  attr(pars, "two_sd") <- two_sd
  attr(pars, "robust") <- robust

  pars
}

#' Parameters from multiply imputed repeated analyses
#'
#' Format models of class \code{mira}, obtained from \code{mice::width.mids()}.
#'
#' @param model An object of class \code{mira}.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' library(parameters)
#' if (require("mice")) {
#'   data(nhanes2)
#'   imp <- mice(nhanes2)
#'   fit <- with(data = imp, exp = lm(bmi ~ age + hyp + chl))
#'   model_parameters(fit)
#' }
#' @importFrom stats var qt pt p.adjust.methods
#' @export
model_parameters.mira <- function(model, ci = .95, exponentiate = FALSE, p_adjust = NULL, ...) {
  pretty_names <- NULL

  # extract model parameters for each sub model
  all_models <- do.call(rbind, lapply(1:length(model$analyses), function(i) {
    params <- model_parameters(model$analyses[[i]], df_method = "wald")
    params$.id <- as.character(i)
    if (is.null(pretty_names)) pretty_names <- attr(params, "pretty_names", exact = TRUE)
    as.data.frame(params)
  }))

  # find where to split parameters
  grp <- intersect(colnames(all_models), c("Parameter", "Response", "Component"))
  params <- split(all_models, all_models[grp])


  # pool models

  params <- do.call(rbind, lapply(params, function(i) {

    # calculate pooled SE
    ubar <- mean(i$SE^2)
    tmp <- ubar + (1 + 1 / nrow(i)) * stats::var(i$Coefficient)
    i$SE <- sqrt(tmp)

    # pooled coefficient
    i$Coefficient <- mean(i$Coefficient)

    # find statistic column
    stat_column <- colnames(i)[grepl("(\\bz\\b|\\bt\\b|\\bF\\b)", colnames(i))][1]
    if (length(stat_column)) {
      i[[stat_column]] <- i$Coefficient / i$SE
    } else {
      stat_column <- "Statistic"
      i$Statistic <- i$Coefficient / i$SE
    }

    # pool degrees of freedom
    df_column <- colnames(i)[grepl("(\\bdf\\b|\\bdf_error\\b)", colnames(i))][1]
    if (length(df_column)) {
      i[[df_column]] <- .barnad_rubin(m = nrow(i), b = stats::var(i$Coefficient), t = tmp, dfcom = i[[df_column]])
    } else {
      df_column <- "df"
      i$df <- Inf
    }

    # calculate CI
    alpha <- (1 + ci) / 2
    i$CI_low <- i$Coefficient - stats::qt(alpha, df = i[[df_column]]) * i$SE
    i$CI_high <- i$Coefficient + stats::qt(alpha, df = i[[df_column]]) * i$SE

    # and p-value
    i$p <- 2 * stats::pt(abs(i[[stat_column]]), df = i[[df_column]], lower.tail = FALSE)

    # filter results
    i[i$.id == 1, ]
  }))

  # remove ID
  params$.id <- NULL

  # adjust p-values?
  if (!is.null(p_adjust) && tolower(p_adjust) %in% stats::p.adjust.methods && "p" %in% colnames(parameters)) {
    parameters$p <- stats::p.adjust(parameters$p, method = p_adjust)
  }

  # final preparation
  if (exponentiate) params <- .exponentiate_parameters(params)
  params <- .add_model_parameters_attributes(params, model, ci, exponentiate, ...)
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}



.barnad_rubin <- function(m, b, t, dfcom = 999999) {
  # fix for z-statistic
  if (all(is.na(dfcom)) || all(is.infinite(dfcom))) {
    return(Inf)
  }
  lambda <- (1 + 1 / m) * b / t
  lambda[lambda < 1e-04] <- 1e-04
  dfold <- (m - 1) / lambda ^ 2
  dfobs <- (dfcom + 1) / (dfcom + 3) * dfcom * (1 - lambda)
  dfold * dfobs / (dfold + dfobs)
}

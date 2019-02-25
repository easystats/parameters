#' BayesFactor objects Parameters
#'
#' Parameters of BayesFactor objects.
#'
#' @param model Object of class \code{BFBayesFactor}.
#' @param ci Credible Interval (CI) level. Default to 0.90 (90\%).
#' @param iterations Number of posterior draws.
#' @param ... Arguments passed to or from other methods.
#'
#'
#' @export
model_parameters.BFBayesFactor <- function(model, ci = 0.90, iterations=4000, ...) {

  if (!requireNamespace("BayesFactor")) {
    warning("This function needs `BayesFactor` to be installed... installing now.")
    install.packages("BayesFactor")
    requireNamespace("BayesFactor")
  }

  if(any(class(model@denominator) %in% c("BFcorrelation"))){
    numerator <- model@numerator[[names(model@numerator)]]
    posteriors <- as.data.frame(suppressMessages(BayesFactor::posterior(model, iterations=iterations, progress = FALSE, ...)))

    hdi <- bayestestR::hdi(posteriors$rho, ci=ci)
    out <- data.frame("Median"=median(posteriors$rho),
                      "MAD"=mad(posteriors$rho),
                      "CI_low" = hdi$CI_low,
                      "CI_high" = hdi$CI_high,
                      "BF" = exp(numerator@analysis$bf),
                      "Prior" = numerator@prior$rscale)

    comment(out) <- "Bayesian correlation"


  } else if(any(class(model@denominator) %in% c("BFoneSample", "BFindepSample"))){

    numerator <- model@numerator[[names(model@numerator)]]
    posteriors <- as.data.frame(suppressMessages(BayesFactor::posterior(model, iterations=iterations, progress = FALSE, ...)))

    hdi <- bayestestR::hdi(posteriors$mu, ci=ci)
    out <- data.frame("Median"=median(posteriors$mu),
                      "MAD"=mad(posteriors$mu),
                      "CI_low" = hdi$CI_low,
                      "CI_high" = hdi$CI_high,
                      "BF" = exp(numerator@analysis$bf),
                      "Prior" = numerator@prior$rscale)
    if(any(class(model@denominator) %in% c("BFoneSample"))){
      comment(out) <- "one sample Bayesian t-test"
    } else {
      comment(out) <- "two samples Bayesian t-test"
    }

  } else{
    stop(paste0("BayesFactor objects of type ", class(model@denominator)[1], " not supported yet."))
  }

  return(out)
}



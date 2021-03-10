#' @importFrom insight get_parameters format_value format_p
#' @importFrom stats setNames
#' @export
model_parameters.epi.2by2 <- function(model, verbose = TRUE, ...) {
  # get parameter estimates
  params <- insight::get_parameters(model)
  colnames(params)[2] <- "Coefficient"

  # get coefficients including CI
  coef_names <- grepl(".strata.wald", names(model$massoc), fixed = TRUE)
  cf <- model$massoc[coef_names]
  names(cf) <- gsub(".strata.wald", "", names(cf), fixed = TRUE)

  # extract CI
  cis <- do.call(rbind, cf)
  cis$Parameter <- rownames(cis)
  cis$est <- NULL
  colnames(cis) <- c("CI_low", "CI_high", "Parameter")

  # merge
  params <- merge(params, cis, sort = FALSE)

  # find fraction estimates, multiply by 100 to get percentages
  fractions <- params$Parameter %in% c("AFRisk", "PAFRisk")
  params[fractions, c("Coefficient", "CI_low", "CI_high")] <- 100 * params[fractions, c("Coefficient", "CI_low", "CI_high")]

  # pretty names
  pretty_names <- params$Parameter
  pretty_names[pretty_names == "PR"] <- "Prevalence Ratio"
  pretty_names[pretty_names == "OR"] <- "Odds Ratio"
  pretty_names[pretty_names == "ARisk"] <- "Attributable Risk"
  pretty_names[pretty_names == "PARisk"] <- "Attributable Risk in Population"
  pretty_names[pretty_names == "AFRisk"] <- "Attributable Fraction in Exposed (%)"
  pretty_names[pretty_names == "PAFRisk"] <- "Attributable Fraction in Population (%)"

  stats <- model$massoc$chisq.strata
  attr(params, "footer_text") <- paste0("Test that Odds Ratio = 1: Chi2(", stats[["df"]], ") = ", insight::format_value(stats[["test.statistic"]]), ", ", insight::format_p(stats[["p.value"]]))
  attr(params, "pretty_names") <- stats::setNames(pretty_names, params$Parameter)
  class(params) <- c("parameters_model", "see_parameters_model", class(params))
  params
}

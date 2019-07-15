#' Conversion between EFA results and CFA structure
#'
#' Enables a conversion betwen Exploratory Factor Analysis (EFA) and Confirmatory Factor Analysis (CFA) \code{lavaan}-ready structure.
#'
#' @param model An EFA model (e.g., a \code{psych::fa} object).
#' @inheritParams principal_components
#' @examples
#' \dontrun{
#' library(psych)
#' library(lavaan)
#' library(parameters)
#'
#' efa <- psych::fa(attitude, nfactors = 3)
#'
#' model1 <- efa_to_cfa(efa)
#' model2 <- efa_to_cfa(efa, threshold = 0.3)
#'
#' anova(
#'   lavaan::cfa(model1, data = attitude),
#'   lavaan::cfa(model2, data = attitude)
#' )
#' }
#'
#' @export
convert_efa_to_cfa <- function(model, ...) {
  UseMethod("convert_efa_to_cfa")
}




#' @export
convert_efa_to_cfa.fa <- function(model, threshold = "max", ...) {
  .efa_to_cfa(model_parameters(model, threshold = threshold, ...))
}

#' @export
convert_efa_to_cfa.factor_structure <- function(model, ...) {
  .efa_to_cfa(model, ...)
}

#' @rdname convert_efa_to_cfa
#' @export
efa_to_cfa <- convert_efa_to_cfa




#' @keywords internal
.efa_to_cfa <- function(loadings, ...) {
  loadings <- attributes(loadings)$loadings_long

  cfa <- c()
  for (comp in unique(loadings$Component)) {
    cfa <- c(
      cfa,
      paste0(comp, " =~ ", paste(as.character(loadings[loadings$Component == comp, "Variable"]), collapse = " + "))
    )
  }

  cfa <- paste0(cfa, collapse = "\n")
  cfa <- paste0("# Latent variables\n", cfa)
  class(cfa) <- c("cfa_model", class(cfa))
  cfa
}

#' @export
print.cfa_model <- function(x, ...) {
  cat(x)
}

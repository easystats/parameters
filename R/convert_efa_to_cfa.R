#' Conversion between EFA results and CFA structure
#'
#' Enables a conversion betwen Exploratory Factor Analysis (EFA) and Confirmatory Factor Analysis (CFA) \code{lavaan}-ready structure.
#'
#' @param model An EFA model (e.g., a \code{psych::fa} object).
#' @inheritParams principal_components
#' @param names Vector containing dimension names.
#'
#' @examples
#' \donttest{
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
#' @return Converted index.
#' @export
convert_efa_to_cfa <- function(model, ...) {
  UseMethod("convert_efa_to_cfa")
}



#' @rdname convert_efa_to_cfa
#' @inheritParams model_parameters.principal
#' @export
convert_efa_to_cfa.fa <- function(model, threshold = "max", names = NULL, ...) {
  .efa_to_cfa(model_parameters(model, threshold = threshold, ...), names = names, ...)
}

#' @export
convert_efa_to_cfa.parameters_efa <- function(model, names = NULL, ...) {
  .efa_to_cfa(model, names = names, ...)
}

#' @export
convert_efa_to_cfa.parameters_pca <- convert_efa_to_cfa.parameters_efa


#' @rdname convert_efa_to_cfa
#' @export
efa_to_cfa <- convert_efa_to_cfa




#' @keywords internal
.efa_to_cfa <- function(loadings, names = NULL, ...) {
  loadings <- attributes(loadings)$loadings_long

  # Get dimension names
  if (is.null(names)) {
    names <- unique(loadings$Component)
  }

  # Catch error
  if (length(names) != length(unique(loadings$Component))) {
    stop(paste("The `names` vector must be of same length as the number of dimensions, in this case", length(unique(loadings$Component))))
  }

  cfa <- c()
  # Iterate over dimensions
  for (i in 1:length(names)) {
    cfa <- c(
      cfa,
      paste0(names[i], " =~ ", paste(as.character(loadings[loadings$Component == unique(loadings$Component)[i], "Variable"]), collapse = " + "))
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

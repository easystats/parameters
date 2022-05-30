#' Parameters from PCA, FA, CFA, SEM
#'
#' Format structural models from the **psych** or **FactoMineR** packages.
#'
#' @param standardize Return standardized parameters (standardized coefficients).
#'   Can be `TRUE` (or `"all"` or `"std.all"`) for standardized
#'   estimates based on both the variances of observed and latent variables;
#'   `"latent"` (or `"std.lv"`) for standardized estimates based
#'   on the variances of the latent variables only; or `"no_exogenous"`
#'   (or `"std.nox"`) for standardized estimates based on both the
#'   variances of observed and latent variables, but not the variances of
#'   exogenous covariates. See `lavaan::standardizedsolution` for details.
#' @param labels A character vector containing labels to be added to the
#'   loadings data. Usually, the question related to the item.
#' @param component What type of links to return. Can be `"all"` or some of
#' `c("regression", "correlation", "loading", "variance", "mean")`.
#' @param ... Arguments passed to or from other methods.
#' @inheritParams principal_components
#' @inheritParams model_parameters.default
#'
#' @note There is also a
#' [`plot()`-method](https://easystats.github.io/see/articles/parameters.html)
#' for `lavaan` models implemented in the
#' [**see**-package](https://easystats.github.io/see/).
#' @details
#'  For the structural models obtained with **psych**, the following indices
#'  are present:
#'
#' - **Complexity** (\cite{Hoffman's, 1978; Pettersson and Turkheimer,
#'    2010}) represents the number of latent components needed to account for
#'    the observed variables. Whereas a perfect simple structure solution has a
#'    complexity of 1 in that each item would only load on one factor, a
#'    solution with evenly distributed items has a complexity greater than 1.
#'
#' - **Uniqueness** represents the variance that is 'unique' to the
#'    variable and not shared with other variables. It is equal to `1 –
#'    communality` (variance that is shared with other variables). A uniqueness
#'    of `0.20` suggests that `20%` or that variable's variance is not shared
#'    with other variables in the overall factor model. The greater 'uniqueness'
#'    the lower the relevance of the variable in the factor model.
#'
#' - **MSA** represents the Kaiser-Meyer-Olkin Measure of Sampling
#'    Adequacy (\cite{Kaiser and Rice, 1974}) for each item. It indicates
#'    whether there is enough data for each factor give reliable results for the
#'    PCA. The value should be > 0.6, and desirable values are > 0.8
#'    (\cite{Tabachnick and Fidell, 2013}).
#'
#' @examples
#' \donttest{
#' library(parameters)
#' if (require("psych", quietly = TRUE)) {
#'   # Principal Component Analysis (PCA) ---------
#'   pca <- psych::principal(attitude)
#'   model_parameters(pca)
#'
#'   pca <- psych::principal(attitude, nfactors = 3, rotate = "none")
#'   model_parameters(pca, sort = TRUE, threshold = 0.2)
#'
#'   principal_components(attitude, n = 3, sort = TRUE, threshold = 0.2)
#'
#'
#'   # Exploratory Factor Analysis (EFA) ---------
#'   efa <- psych::fa(attitude, nfactors = 3)
#'   model_parameters(efa, threshold = "max", sort = TRUE, labels = as.character(1:ncol(attitude)))
#'
#'
#'   # Omega ---------
#'   omega <- psych::omega(mtcars, nfactors = 3)
#'   params <- model_parameters(omega)
#'   params
#'   summary(params)
#' }
#'
#' # FactoMineR ---------
#' if (require("FactoMineR", quietly = TRUE)) {
#'   model <- FactoMineR::PCA(iris[, 1:4], ncp = 2)
#'   model_parameters(model)
#'   attributes(model_parameters(model))$scores
#'
#'   model <- FactoMineR::FAMD(iris, ncp = 2)
#'   model_parameters(model)
#' }
#' }
#'
#' # lavaan
#'
#' library(parameters)
#'
#' # lavaan -------------------------------------
#' if (require("lavaan", quietly = TRUE)) {
#'
#'   # Confirmatory Factor Analysis (CFA) ---------
#'
#'   structure <- " visual  =~ x1 + x2 + x3
#'                  textual =~ x4 + x5 + x6
#'                  speed   =~ x7 + x8 + x9 "
#'   model <- lavaan::cfa(structure, data = HolzingerSwineford1939)
#'   model_parameters(model)
#'   model_parameters(model, standardize = TRUE)
#'
#'   # filter parameters
#'   model_parameters(
#'     model,
#'     parameters = list(
#'       To = "^(?!visual)",
#'       From = "^(?!(x7|x8))"
#'     )
#'   )
#'
#'   # Structural Equation Model (SEM) ------------
#'
#'   structure <- "
#'     # latent variable definitions
#'       ind60 =~ x1 + x2 + x3
#'       dem60 =~ y1 + a*y2 + b*y3 + c*y4
#'       dem65 =~ y5 + a*y6 + b*y7 + c*y8
#'     # regressions
#'       dem60 ~ ind60
#'       dem65 ~ ind60 + dem60
#'     # residual correlations
#'       y1 ~~ y5
#'       y2 ~~ y4 + y6
#'       y3 ~~ y7
#'       y4 ~~ y8
#'       y6 ~~ y8
#'   "
#'   model <- lavaan::sem(structure, data = PoliticalDemocracy)
#'   model_parameters(model)
#'   model_parameters(model, standardize = TRUE)
#' }
#'
#' @return A data frame of indices or loadings.
#' @references
#' - Kaiser, H.F. and Rice. J. (1974). Little jiffy, mark iv. Educational and
#' Psychological Measurement, 34(1):111–117
#'
#' - Pettersson, E., \& Turkheimer, E. (2010). Item selection, evaluation, and
#' simple structure in personality data. Journal of research in personality,
#' 44(4), 407-420.
#'
#' - Revelle, W. (2016). How To: Use the psych package for Factor Analysis and
#' data reduction.
#'
#' - Tabachnick, B. G., and Fidell, L. S. (2013). Using multivariate statistics
#' (6th ed.). Boston: Pearson Education.
#'
#' - Rosseel Y (2012). lavaan: An R Package for Structural Equation
#'   Modeling. Journal of Statistical Software, 48(2), 1-36.
#'
#' - Merkle EC , Rosseel Y (2018). blavaan: Bayesian Structural Equation
#'   Models via Parameter Expansion. Journal of Statistical Software, 85(4),
#'   1-30. http://www.jstatsoft.org/v85/i04/
#'
#' @export
model_parameters.principal <- function(model,
                                       sort = FALSE,
                                       threshold = NULL,
                                       labels = NULL,
                                       verbose = TRUE,
                                       ...) {

  # n
  n <- model$factors

  # Get summary
  variance <- as.data.frame(unclass(model$Vaccounted))
  data_summary <- .data_frame(
    Component = names(variance),
    Eigenvalues = model$values[1:n],
    Variance = as.numeric(variance["Proportion Var", ])
  )
  if ("Cumulative Var" %in% row.names(variance)) {
    data_summary$Variance_Cumulative <- as.numeric(variance["Cumulative Var", ])
  } else {
    if (ncol(variance) == 1) {
      data_summary$Variance_Cumulative <- as.numeric(variance["Proportion Var", ])
    } else {
      data_summary$Variance_Cumulative <- NA
    }
  }
  data_summary$Variance_Proportion <- data_summary$Variance / sum(data_summary$Variance)

  # Get loadings
  loadings <- as.data.frame(unclass(model$loadings))

  # Format
  loadings <- cbind(data.frame(Variable = row.names(loadings)), loadings)
  row.names(loadings) <- NULL

  # Labels
  if (!is.null(labels)) {
    loadings$Label <- labels
    loadings <- loadings[c("Variable", "Label", names(loadings)[!names(loadings) %in% c("Variable", "Label")])]
    loading_cols <- 3:(n + 2)
  } else {
    loading_cols <- 2:(n + 1)
  }

  # Add information
  loadings$Complexity <- model$complexity
  loadings$Uniqueness <- model$uniquenesses
  loadings$MSA <- attributes(model)$MSA

  # Add attributes
  attr(loadings, "summary") <- data_summary
  attr(loadings, "model") <- model
  attr(loadings, "rotation") <- model$rotation
  attr(loadings, "scores") <- model$scores
  attr(loadings, "additional_arguments") <- list(...)
  attr(loadings, "n") <- n
  attr(loadings, "type") <- model$fn
  attr(loadings, "loadings_columns") <- loading_cols

  # Sorting
  if (isTRUE(sort)) {
    loadings <- .sort_loadings(loadings)
  }

  # Replace by NA all cells below threshold
  if (!is.null(threshold)) {
    loadings <- .filter_loadings(loadings, threshold = threshold)
  }

  # Add some more attributes
  attr(loadings, "loadings_long") <- .long_loadings(loadings, threshold = threshold, loadings_columns = loading_cols)
  # here we match the original columns in the data set with the assigned components
  # for each variable, so we know which column in the original data set belongs
  # to which extracted component...
  attr(loadings, "closest_component") <- .closest_component(loadings, loadings_columns = loading_cols, variable_names = rownames(model$loadings))

  # add class-attribute for printing
  if (model$fn == "principal") {
    class(loadings) <- unique(c("parameters_pca", "see_parameters_pca", class(loadings)))
  } else {
    class(loadings) <- unique(c("parameters_efa", "see_parameters_efa", class(loadings)))
  }

  loadings
}


#' @export
model_parameters.fa <- model_parameters.principal

#' @export
model_parameters.fa.ci <- model_parameters.fa



#' @rdname model_parameters.principal
#' @export
model_parameters.omega <- function(model, verbose = TRUE, ...) {

  # Table of omega coefficients
  table_om <- model$omega.group
  colnames(table_om) <- c("Omega_Total", "Omega_Hierarchical", "Omega_Group")
  table_om$Composite <- row.names(table_om)
  row.names(table_om) <- NULL
  table_om <- table_om[c("Composite", names(table_om)[names(table_om) != "Composite"])]

  # Get summary: Table of Variance
  table_var <- as.data.frame(unclass(model$omega.group))
  table_var$Composite <- rownames(model$omega.group)
  table_var$Total <- table_var$total * 100
  table_var$General <- table_var$general * 100
  table_var$Group <- table_var$group * 100
  table_var <- table_var[c("Composite", "Total", "General", "Group")]

  # colnames(table_var) <- c("Composite", "Total Variance (%)", "Variance due to General Factor (%)", "Variance due to Group Factor (%)")

  # cor.plot(psych::fa.sort(om), main = title)

  out <- table_om
  attr(out, "summary") <- table_var
  class(out) <- c("parameters_omega", class(out))
  out
}

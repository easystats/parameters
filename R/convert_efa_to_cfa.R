#' Conversion between EFA results and CFA model
#'
#' Enables a conversion betwen Exploratory Factor Analysis (EFA) and Confirmatory Factor Analysis (CFA) \code{lavaan}-ready model.
#'
#' @param model An EFA model (e.g., a \code{psych::fa} object).
#' @examples
#' \dontrun{
#' efa <- psych::fa(attitude, nfactors = 3)
#' model_parameters(efa)
#' }
#'
#' @export
#' convert_efa_to_cfa <- function(model, ...){
#'   UseMethod("convert_efa_to_cfa")
#' }
#'
#' #' @export
#' efa_to_cfa <- convert_efa_to_cfa
#'
#'
#'
#' #' @export
#' convert_efa_to_cfa.fa <- function(model, treshold = "max", ...){
#'   if (treshold == "max") {
#'     filtered_loadings <- get_loadings_max(loadings)
#'   } else {
#'     filtered_loadings <- loadings %>%
#'       tidyr::gather_("Component", "Loading", names(loadings)[!names(loadings) %in% c("Item", "N", "Label")]) %>%
#'       filter_("Loading > treshold")
#'   }
#'
#'
#'
#' }
#'
#'
#' #' @keywords internal
#' .efa_to_cfa <- function(params, ...){
#'   attributes(params)
#'   cfa_model <- params %>%
#'     select_("Item", "Component") %>%
#'     group_by_("Component") %>%
#'     summarise_("Observed" = 'paste(Item, collapse=" + ")') %>%
#'     transmute_("Latent_Variable" = 'paste(Component, Observed, sep=" =~ ")') %>%
#'     pull()
#'
#'   cfa_model <- c("#Latent variables", cfa_model) %>%
#'     paste(collapse = "\n")
#'
#'   return(cfa_model)
#' }
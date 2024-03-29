#' @param method The method used in the variable selection. Can be `NULL`
#'   (default), `"forward"` or `"L1"`. See `projpred::varsel`.
#' @param cross_validation Select with cross-validation.
#' @rdname select_parameters
#' @export

select_parameters.stanreg <- function(model,
                                      method = NULL,
                                      cross_validation = FALSE,
                                      ...) {
  insight::check_if_installed("projpred")

  if (cross_validation) {
    message("Cross-validating best parameters...")
    junk <- utils::capture.output(selection <- projpred::cv_varsel(model, method = method, ...))
  } else {
    selection <- projpred::varsel(model, method = method, ...)
  }

  # Visualise
  # varsel_plot(selection, stats = c('elpd', 'rmse'), deltas = TRUE)

  # Extract parameters
  projection <- projpred::project(selection, nv = projpred::suggest_size(selection), ...)
  parameters <- row.names(projection$beta)

  # Reconstruct formula
  formula <- .reconstruct_formula(parameters, model)

  # Update model
  junk <- utils::capture.output(best <- suppressWarnings(stats::update(model, formula = formula, ...)))
  best
}


#' @export
select_parameters.brmsfit <- select_parameters.stanreg


#' @keywords internal
.reconstruct_formula <- function(parameters, model) {
  # # Clean
  # if (utils::tail(parameters, 1) == "sigma") {
  #   parameters <- parameters[1:length(parameters) - 1]
  # }
  # if (parameters[1] == "(Intercept)") {
  #   parameters <- parameters[2:length(parameters)]
  # }
  #
  # # Detect interactions
  # interactions <- parameters[grepl(":", parameters)]
  # if (length(interactions) > 0) {
  #   for (interaction in interactions) {
  #     terms <- unlist(strsplit(interaction, ":", fixed = TRUE))
  #     if (length(terms) == 2) {
  #       if (all(terms %in% parameters)) {
  #         # replace interactions components by interactions
  #         parameters <- parameters[!parameters %in% c(terms, interaction)]
  #         parameters <- c(parameters, paste0(terms, collapse = " * "))
  #       }
  #     }
  #   }
  # }


  formula <- paste(parameters, collapse = " + ")
  formula <- paste(insight::find_response(model), "~", formula)
  formula
}

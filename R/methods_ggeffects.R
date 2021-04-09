#' @export
model_parameters.ggeffects <- function(model, verbose = TRUE, ...) {
  ci <- attributes(model)$ci.lvl
  response_name <- attributes(model)$response.name
  terms <- attributes(model)$terms[-1]

  # exception for survival
  if (attributes(model)$type %in% c("surv", "survival", "cumhaz", "cumulative_hazard")) {
    response_name <- "Time"
  }

  model <- as.data.frame(model)

  # rename columns
  new_colnames <- colnames(model)
  new_colnames[new_colnames == "predicted"] <- "Predicted"
  new_colnames[new_colnames == "std.error"] <- "SE"
  new_colnames[new_colnames == "conf.low"] <- "CI_low"
  new_colnames[new_colnames == "conf.high"] <- "CI_high"
  new_colnames[new_colnames == "group"] <- "Component"
  new_colnames[new_colnames == "facet"] <- "Group"
  new_colnames[new_colnames == "response"] <- "Subgroup"

  colnames(model) <- new_colnames
  model$SE <- NULL

  if (.n_unique(model$Component) == 1) {
    model$Component <- NULL
  }
  if (!is.null(response_name)) {
    colnames(model)[1] <- response_name
  }

  if (length(terms) >= 1) {
    model$Component <- paste0(terms[1], " = ", model$Component)
  }
  if (length(terms) >= 2) {
    model$Group <- paste0(terms[2], " = ", model$Group)
  }
  if (length(terms) >= 3) {
    model$Subgroup <- paste0(terms[3], " = ", model$Subgroup)
  }

  model <- .add_model_parameters_attributes(model, model, ci = ci, verbose = verbose)
  attr(model, "is_ggeffects") <- TRUE
  attr(model, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(model) <- c("parameters_model", "data.frame")
  model
}

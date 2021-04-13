#' @export
model_parameters.ggeffects <- function(model, parameters = NULL, verbose = TRUE, ...) {
  ci <- attributes(model)$ci.lvl
  terms <- attributes(model)$terms[-1]
  focal_term <- attributes(model)$terms[1]
  constant_values <- attributes(model)$constant.values
  title <- attr(model, "title")

  # exception for survival
  if (attributes(model)$type %in% c("surv", "survival", "cumhaz", "cumulative_hazard")) {
    focal_term <- "Time"
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
  if (!is.null(focal_term)) {
    colnames(model)[1] <- focal_term
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

  # filter parameters
  if (!is.null(parameters)) {
    model <- .filter_parameters(model, parameters)
  }

  model <- .add_model_parameters_attributes(model, model, ci = ci, verbose = verbose)

  # special attributes
  attr(model, "is_ggeffects") <- TRUE
  attr(model, "footer_text") <- .generate_ggeffects_footer(constant_values)
  attr(model, "title") <- c(title, "blue")

  attr(model, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(model) <- c("parameters_model", "data.frame")
  model
}


.generate_ggeffects_footer <- function(constant_values) {
  cv <- lapply(constant_values, function(.x) {
    if (is.numeric(.x))
      sprintf("%.2f", .x)
    else
      as.character(.x)
  })
  footer <- NULL

  if (!.is_empty_object(cv)) {
    cv.names <- names(cv)
    cv.space <- max(nchar(cv.names))

    # ignore this string when determining maximum length
    poplev <- which(cv %in% c("NA (population-level)", "0 (population-level)"))
    if (!.is_empty_object(poplev))
      mcv <- cv[-poplev]
    else
      mcv <- cv

    if (!.is_empty_object(mcv))
      cv.space2 <- max(nchar(mcv))
    else
      cv.space2 <- 0

    adjusted_predictors <- paste0(sprintf("* %*s = %*s", cv.space, cv.names, cv.space2, cv), collapse = "\n")
    footer <- paste0("Adjusted for:\n", adjusted_predictors)
  }

  footer
}

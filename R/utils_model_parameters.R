#' @keywords internal
.add_model_parameters_attributes <- function(params, model, ci, exponentiate = FALSE, ...) {
  dot.arguments <- lapply(match.call(expand.dots = FALSE)$`...`, function(x) x)
  info <- insight::model_info(model, verbose = FALSE)

  ## TODO remove is.list() when insight 0.8.3 on CRAN
  if (is.null(info) || !is.list(info)) {
    info <- list(family = "unknown", link_function = "unknown")
  }

  if (is.null(attr(params, "pretty_names", exact = TRUE))) {
    attr(params, "pretty_names") <- format_parameters(model)
  }
  attr(params, "ci") <- ci
  attr(params, "exponentiate") <- exponentiate
  attr(params, "ordinal_model") <- isTRUE(info$is_ordinal) | isTRUE(info$is_multinomial)
  attr(params, "model_class") <- class(model)


  if (inherits(model, c("rma", "rma.uni"))) {
    attr(params, "data") <- insight::get_data(model)
    attr(params, "study_weights") <- 1 / model$vi
  }

  if ("digits" %in% names(dot.arguments)) {
    attr(params, "digits") <- eval(dot.arguments[["digits"]])
  } else {
    attr(params, "digits") <- 2
  }

  if ("ci_digits" %in% names(dot.arguments)) {
    attr(params, "ci_digits") <- eval(dot.arguments[["ci_digits"]])
  } else {
    attr(params, "ci_digits") <- 2
  }

  if ("p_digits" %in% names(dot.arguments)) {
    attr(params, "p_digits") <- eval(dot.arguments[["p_digits"]])
  } else {
    attr(params, "p_digits") <- 3
  }

  params
}



#' @keywords internal
.exponentiate_parameters <- function(params) {
  columns <- grepl(pattern = "^(Coefficient|Std_Coefficient|CI_)", colnames(params))
  if (any(columns)) {
    params[columns] <- exp(params[columns])
    if (all(c("Coefficient", "SE") %in% names(params))) {
      params$SE <- exp(params$SE)
    }
  }
  params
}




#' @importFrom insight clean_parameters
.add_pretty_names <- function(params, model, effects = NULL, component = NULL) {
  attr(params, "model_class") <- class(model)
  clean_params <- insight::clean_parameters(model)

  if (is.null(effects)) {
    effects <- "fixed"
  } else if (effects == "all") {
    effects <- c("fixed", "random")
  }

  if (is.null(component)) {
    component <- "conditional"
  } else if (component == "all") {
    component <- c("conditional", "zi", "zero_inflated", "dispersion")
  }

  clean_params <- clean_params[clean_params$Component %in% component & clean_params$Effects %in% effects, ]
  attr(params, "cleaned_parameters") <- clean_params$Cleaned_Parameter

  params
}

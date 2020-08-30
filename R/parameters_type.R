#' Type of model parameters
#'
#' @param model A statistical model.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' library(parameters)
#'
#' model <- lm(Sepal.Length ~ Petal.Length + Species, data = iris)
#' parameters_type(model)
#'
#' model <- lm(Sepal.Length ~ Species + poly(Sepal.Width, 2), data = iris)
#' parameters_type(model)
#'
#' model <- lm(Sepal.Length ~ Species + poly(Sepal.Width, 2, raw = TRUE), data = iris)
#' parameters_type(model)
#'
#' # Interactions
#' model <- lm(Sepal.Length ~ Sepal.Width * Species, data = iris)
#' parameters_type(model)
#'
#' model <- lm(Sepal.Length ~ Sepal.Width * Species * Petal.Length, data = iris)
#' parameters_type(model)
#'
#' model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)
#' parameters_type(model)
#'
#' model <- lm(Sepal.Length ~ Species / Sepal.Width, data = iris)
#' parameters_type(model)
#'
#'
#' # Complex interactions
#' data <- iris
#' data$fac2 <- ifelse(data$Sepal.Width > mean(data$Sepal.Width), "A", "B")
#' model <- lm(Sepal.Length ~ Species / fac2 / Petal.Length, data = data)
#' parameters_type(model)
#'
#' model <- lm(Sepal.Length ~ Species / fac2 * Petal.Length, data = data)
#' parameters_type(model)
#' @return A data frame.
#' @export
parameters_type <- function(model, ...) {

  # Get info
  params <- data.frame(
    Parameter = insight::find_parameters(model, effects = "fixed", flatten = TRUE),
    stringsAsFactors = FALSE
  )

  # Special case
  if (inherits(model, "polr")) {
    params$Parameter <- gsub("Intercept: ", "", params$Parameter, fixed = TRUE)
  }

  # Special case
  if (inherits(model, "bracl")) {
    params$Parameter <- gsub("(.*):(.*)", "\\2", params$Parameter)
  }

  # Special case
  if (inherits(model, "DirichletRegModel")) {
    cf <- stats::coef(model)
    if (model$parametrization == "common") {
      pattern <- paste0("(", paste(model$varnames, collapse = "|"), ")\\.(.*)")
      params$Parameter <- gsub(pattern, "\\2", names(unlist(cf)))
    } else {
      params$Parameter <- gsub("(.*)\\.(.*)\\.(.*)", "\\3", names(unlist(cf)))
    }
  }


  # Remove "as.factor()", "log()" etc. from parameter names but save original parameter before
  original_parameter <- params$Parameter
  params$Parameter <- .clean_parameter_names(params$Parameter, full = TRUE)

  ## TODO can we get rid of the count_ / zero_ prefix here?
  if (inherits(model, c("zeroinfl", "hurdle", "zerocount"))) {
    params$Parameter <- gsub("^(count_|zero_)", "", params$Parameter)
  }


  data <- insight::get_data(model)
  if (is.null(data)) {
    return(NULL)
  }
  reference <- .list_factors_numerics(data)

  # Get types
  main <- .parameters_type_table(names = params$Parameter, data, reference)
  secondary <- .parameters_type_table(names = main$Secondary_Parameter, data, reference)
  names(secondary) <- paste0("Secondary_", names(secondary))
  names(secondary)[names(secondary) == "Secondary_Secondary_Parameter"] <- "Tertiary_Parameter"

  out <- cbind(params, main, secondary)

  # Deal with nested interactions
  for (i in unique(paste0(out[out$Type == "interaction", "Variable"], out[out$Type == "interaction", "Secondary_Variable"]))) {
    interac <- out[paste0(out$Variable, out$Secondary_Variable) == i, ]
    if (!all(interac$Term %in% out$Parameter)) {
      out[paste0(out$Variable, out$Secondary_Variable) == i, "Type"] <- "nested"
    }
  }
  for (i in unique(out$Secondary_Parameter)) {
    if (!is.na(i) && i %in% out$Parameter) {
      .param_type <- out[!is.na(out$Parameter) & out$Parameter == i, "Type"]
      .param_secondary_type <- out[!is.na(out$Secondary_Parameter) & out$Secondary_Parameter == i, "Secondary_Type"]
      if (length(.param_type) == length(.param_secondary_type) || length(.param_type) == 1) {
        out[!is.na(out$Secondary_Parameter) & out$Secondary_Parameter == i, "Secondary_Type"] <- .param_type
      }
    }
  }

  out$Parameter <- original_parameter
  out
}



#' @keywords internal
.parameters_type_table <- function(names, data, reference) {
  out <- lapply(names, .parameters_type, data = data, reference = reference)
  out <- as.data.frame(do.call(rbind, out), stringsAsFactors = FALSE)
  names(out) <- c("Type", "Link", "Term", "Variable", "Level", "Secondary_Parameter")
  out
}





#' @keywords internal
.parameters_type <- function(name, data, reference) {
  if (grepl(":", name, fixed = TRUE)) {

    # Split
    var <- unlist(strsplit(name, ":", fixed = TRUE))
    if (length(var) > 2) {
      var <- c(utils::tail(var, 1), paste0(utils::head(var, -1), collapse = ":"))
    } else {
      var <- rev(var)
    }

    # Check if any is factor
    types <- unlist(lapply(var, function(x, data, reference) .parameters_type_basic(x, data, reference)[1], data = data, reference = reference))
    link <- ifelse(any("factor" %in% types), "Difference", "Association")
    # Get type
    main <- .parameters_type_basic(var[1], data, reference)
    return(c("interaction", link, main[3], main[4], main[5], var[2]))
  } else {
    .parameters_type_basic(name, data, reference)
  }
}




#' @importFrom utils tail head
#' @keywords internal
.parameters_type_basic <- function(name, data, reference) {
  if (is.na(name)) {
    return(c(NA, NA, NA, NA, NA, NA))
  }

  # parameter type is determined here. for formatting / printing,
  # refer to ".format_parameter()". Make sure that pattern
  # processed here are not "cleaned" (i.e. removed) in
  # ".clean_parameter_names()"

  cleaned_name <- .clean_parameter_names(name, full = TRUE)
  cleaned_ordered_name <- gsub("(.*)((\\.|\\^).*)", "\\1", cleaned_name)

  # Intercept
  if (.in_intercepts(cleaned_name)) {
    return(c("intercept", "Mean", "(Intercept)", NA, NA, NA))

    # Numeric
  } else if (cleaned_name %in% reference$numeric) {
    return(c("numeric", "Association", name, name, NA, NA))

    # Ordered factors
  } else if (is.ordered(data[[cleaned_ordered_name]])) {
    fac <- reference$levels_parent[match(cleaned_name, reference$levels)]
    return(c(
      "ordered",
      "Association",
      name,
      fac,
      .format_ordered(gsub(fac, "", name, fixed = TRUE)),
      NA
    ))

    # Factors
  } else if (cleaned_name %in% reference$levels) {
    fac <- reference$levels_parent[match(cleaned_name, reference$levels)]
    return(c(
      "factor",
      "Difference",
      name,
      fac,
      gsub(fac, "", name, fixed = TRUE),
      NA
    ))

    # Polynomials
  } else if (grepl("poly(", name, fixed = TRUE)) {
    if (grepl(", raw = TRUE", name, fixed = TRUE)) {
      name <- gsub(", raw = TRUE", "", name, fixed = TRUE)
      type <- "poly_raw"
    } else {
      type <- "poly"
    }
    vars <- gsub("poly(", "", name, fixed = TRUE)
    vars <- unlist(strsplit(vars, ", ", fixed = TRUE))
    var <- vars[[1]]
    degree <- vars[[2]]
    degree <- substr(vars[[2]], nchar(vars[[2]]), nchar(vars[[2]]))
    return(c(type, "Association", name, var, degree, NA))

    # Splines
  } else if (grepl("(bs|ns|psline|lspline|rcs)\\(", name)) {
    type <- "spline"
    var <- gsub("(bs|ns|psline|lspline|rcs)\\((.*)\\)(\\d)", "\\2", name)
    if (grepl(",", var, fixed = TRUE)) {
      var <- substr(var, start = 0, stop = regexpr(",", var, fixed = TRUE) - 1)
    }
    degree <- gsub("(bs|ns|psline|lspline|rcs)\\((.*)\\)(\\d)", "\\3", name)
    return(c(type, "Association", name, var, degree, NA))

    # log-transformation
  } else if (grepl("(log|logb|log1p|log2|log10)\\(", name)) {
    type <- "logarithm"
    var <- gsub("(log|logb|log1p|log2|log10)\\((.*)\\)", "\\2", name)
    if (grepl(",", var, fixed = TRUE)) {
      var <- substr(var, start = 0, stop = regexpr(",", var, fixed = TRUE) - 1)
    }
    return(c(type, "Association", name, var, NA, NA))

    # exp-transformation
  } else if (grepl("(exp|expm1)\\(", name)) {
    type <- "exponentiation"
    var <- gsub("(exp|expm1)\\((.*)\\)", "\\2", name)
    if (grepl(",", var, fixed = TRUE)) {
      var <- substr(var, start = 0, stop = regexpr(",", var, fixed = TRUE) - 1)
    }
    return(c(type, "Association", name, var, NA, NA))

    # sqrt-transformation
  } else if (grepl("sqrt\\(", name)) {
    type <- "squareroot"
    var <- gsub("sqrt\\((.*)\\)", "\\1", name)
    if (grepl(",", var, fixed = TRUE)) {
      var <- substr(var, start = 0, stop = regexpr(",", var, fixed = TRUE) - 1)
    }
    return(c(type, "Association", name, var, NA, NA))

    # As Is
  } else if (grepl("^I\\(", name)) {
    type <- "asis"
    var <- gsub("^I\\((.*)\\)", "\\1", name)
    return(c(type, "Association", name, var, NA, NA))

    # Smooth
  } else if (grepl("^s\\(", name)) {
    return(c("smooth", "Association", name, NA, NA, NA))

    # Smooth
  } else if (grepl("^smooth_", name)) {
    return(c("smooth", "Association", gsub("^smooth_(.*)\\[(.*)\\]", "\\2", name), NA, NA, NA))
  } else {
    return(c("unknown", NA, NA, NA, NA, NA))
  }
}



#' @keywords internal
.list_factors_numerics <- function(data) {
  out <- list()
  out$numeric <- names(data[sapply(data, is.numeric)])

  # Ordered factors
  out$ordered <- names(data[sapply(data, is.ordered)])

  # Factors
  out$factor <- names(data[sapply(data, is.factor) | sapply(data, is.character)])

  out$levels <- NA
  out$levels_parent <- NA
  for (fac in out$factor) {
    if (fac %in% out$ordered) {
      levels <- paste0(fac, c(".L", ".Q", ".C", paste0("^", 4:1000))[1:length(unique(data[[fac]]))])
    } else {
      levels <- paste0(fac, unique(data[[fac]]))
    }
    out$levels_parent <- c(out$levels_parent, rep(fac, length(levels)))
    out$levels <- c(out$levels, levels)
  }
  out$levels <- out$levels[!is.na(out$levels)]
  out$levels_parent <- out$levels_parent[!is.na(out$levels_parent)]

  out
}

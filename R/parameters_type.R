#' Type of Model Parameters
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
#' @return A data.frame.
#' @export
parameters_type <- function(model, ...) {

  # Get info
  params <- .data_frame(
    Parameter = c(
      insight::find_parameters(model, effects = "fixed", flatten = TRUE)
    )
  )

  if (inherits(model, "polr")) {
    params$Parameter <- gsub("Intercept: ", "", params$Parameter, fixed = TRUE)
  }

  # remove "as.factor()", "log()" etc. from parameter names,
  # but save original parameter before
  original_parameter <- params$Parameter
  params$Parameter <- .clean_parameter_names(params$Parameter)

  ## TODO can we get rid of the count_ / zero_ prefix here?

  # if (inherits(model, c("zeroinfl", "hurdle", "zerocount"))) {
  #   params$Parameter <- gsub("^(count_|zero_)", "", params$Parameter)
  # }


  data <- insight::get_data(model)
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
      out[!is.na(out$Secondary_Parameter) & out$Secondary_Parameter == i, "Secondary_Type"] <- out[!is.na(out$Parameter) & out$Parameter == i, "Type"]
    }
  }

  out$Parameter <- original_parameter
  # Out
  out
}



#' @keywords internal
.parameters_type_table <- function(names, data, reference) {
  out <- lapply(names, .parameters_type, data = data, reference = reference)
  out <- as.data.frame(do.call(rbind, out), stringsAsFactors = FALSE)
  names(out) <- c("Type", "Term", "Variable", "Level", "Secondary_Parameter")
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

    main <- .parameters_type_basic(var[1], data, reference)
    return(c("interaction", main[2], main[3], main[4], var[2]))
  } else {
    .parameters_type_basic(name, data, reference)
  }
}




#' @importFrom utils tail head
#' @keywords internal
.parameters_type_basic <- function(name, data, reference) {
  if (is.na(name)) {
    return(c(NA, NA, NA, NA, NA))
  }

  cleaned_name <- .clean_parameter_names(name, full = TRUE)

  # Intercept
  if (cleaned_name == "(Intercept)" | cleaned_name == "b_Intercept") {
    return(c("intercept", "(Intercept)", NA, NA, NA))

    # Numeric
  } else if (cleaned_name %in% reference$numeric) {
    return(c("numeric", name, name, NA, NA))

    # Factors
  } else if (cleaned_name %in% reference$levels) {
    fac <- reference$levels_parent[match(cleaned_name, reference$levels)]
    return(c(
      "factor",
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
    return(c(type, name, var, degree, NA))

    # Smooth
  } else if (grepl("^s\\(", name)) {
    return(c("smooth", name, NA, NA, NA))
    # Smooth
  } else if (grepl("^smooth_", name)) {
    return(c("smooth", gsub("^smooth_(.*)\\[(.*)\\]", "\\2", name), NA, NA, NA))
  } else {
    return(c("unknown", NA, NA, NA, NA))
  }
}



#' @keywords internal
.list_factors_numerics <- function(data) {
  out <- list()
  out$numeric <- names(data[sapply(data, is.numeric)])

  # Factors
  out$factor <- names(data[sapply(data, is.factor) | sapply(data, is.character)])

  out$levels <- NA
  out$levels_parent <- NA
  for (fac in out$factor) {
    levels <- paste0(fac, unique(data[[fac]]))
    out$levels_parent <- c(out$levels_parent, rep(fac, length(levels)))
    out$levels <- c(out$levels, levels)
  }
  out$levels <- out$levels[!is.na(out$levels)]
  out$levels_parent <- out$levels_parent[!is.na(out$levels_parent)]

  out
}



.clean_parameter_names <- function(x, full = FALSE) {
  # return if x is empty
  if (is.null(x) || length(x) == 0 || nchar(x) == 0) {
    return("")
  }

  pattern <- if (full) {
    c(
      "as.factor", "factor", "offset", "log1p", "log10", "log2", "log", "lag",
      "diff", "pspline", "poly", "catg", "asis", "matrx", "pol", "strata", "strat",
      "scale", "scored", "interaction", "sqrt", "lsp", "rcs", "pb", "lo", "bs",
      "ns", "t2", "te", "ti", "tt", "mi", "mo", "gp", "s", "I"
    )
  } else {
    c("as.factor", "factor", "catg", "asis", "interaction", "I")
  }

  for (j in 1:length(pattern)) {
    # remove possible  namespace
    x <- sub("(.*)::(.*)", "\\2", x)
    if (pattern[j] == "offset") {
      x <- trimws(unique(sub("offset\\(([^-+ )]*)\\)(.*)", "\\1\\2", x)))
    } else if (pattern[j] == "I") {
      x <- trimws(unique(sub("I\\(((\\w|\\.|\\^)*).*", "\\1", x)))
    } else {
      p <- paste0(pattern[j], "\\(((\\w|\\.)*)\\)(.*)")
      x <- trimws(unique(sub(p, "\\1\\3", x)))
    }
  }

  x
}

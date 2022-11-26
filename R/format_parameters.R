#' @title Parameter names formatting
#' @name format_parameters
#'
#' @description This functions formats the names of model parameters (coefficients)
#'   to make them more human-readable.
#'
#' @param model A statistical model.
#' @param brackets A character vector of length two, indicating the opening and closing brackets.
#' @param ... Currently not used.
#'
#' @section Interpretation of Interaction Terms:
#' Note that the *interpretation* of interaction terms depends on many
#' characteristics of the model. The number of parameters, and overall
#' performance of the model, can differ *or not* between `a * b`
#' `a : b`, and `a / b`, suggesting that sometimes interaction terms
#' give different parameterizations of the same model, but other times it gives
#' completely different models (depending on `a` or `b` being factors
#' of covariates, included as main effects or not, etc.). Their interpretation
#' depends of the full context of the model, which should not be inferred
#' from the parameters table alone - rather, we recommend to use packages
#' that calculate estimated marginal means or marginal effects, such as
#' \CRANpkg{modelbased}, \CRANpkg{emmeans}, \CRANpkg{ggeffects}, or
#' \CRANpkg{marginaleffects}. To raise awareness for this issue, you may use
#' `print(...,show_formula=TRUE)` to add the model-specification to the output
#' of the [`print()`][print.parameters_model] method for `model_parameters()`.
#'
#' @examples
#' model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)
#' format_parameters(model)
#'
#' model <- lm(Sepal.Length ~ Petal.Length + (Species / Sepal.Width), data = iris)
#' format_parameters(model)
#'
#' model <- lm(Sepal.Length ~ Species + poly(Sepal.Width, 2), data = iris)
#' format_parameters(model)
#'
#' model <- lm(Sepal.Length ~ Species + poly(Sepal.Width, 2, raw = TRUE), data = iris)
#' format_parameters(model)
#' @return A (names) character vector with formatted parameter names. The value
#' names refer to the original names of the coefficients.
#' @export
format_parameters <- function(model, ...) {
  UseMethod("format_parameters")
}


#' @rdname format_parameters
#' @export
format_parameters.default <- function(model, brackets = c("[", "]"), ...) {
  # check for valid input
  .is_model_valid(model)

  tryCatch(
    .format_parameter_default(model, brackets = brackets, ...),
    error = function(e) NULL
  )
}


#' @export
format_parameters.parameters_model <- function(model, ...) {
  if (!is.null(attributes(model)$pretty_names)) {
    model$Parameter <- attributes(model)$pretty_names[model$Parameter]
  }
  model
}




# Utilities ---------------------------------------------------------------


.format_parameter_default <- function(model, effects = "fixed", brackets = c("[", "]"), ...) {
  original_names <- names <- insight::find_parameters(model, effects = effects, flatten = TRUE)

  # save some time, if model info is passed as argument
  dot_args <- list(...)
  if (!is.null(dot_args$model_info)) {
    info <- dot_args$model_info
  } else {
    info <- insight::model_info(model, verbose = FALSE)
  }

  ## TODO remove is.list() when insight 0.8.3 on CRAN
  if (is.null(info) || !is.list(info)) {
    info <- list(family = "unknown", link_function = "unknown")
  }

  # quick fix, for multivariate response models, we use
  # info from first model only
  if (insight::is_multivariate(model) && !"is_zero_inflated" %in% names(info)) {
    info <- info[[1]]
  }


  # Type-specific changes
  types <- parameters_type(model)
  if (is.null(types)) {
    return(NULL)
  }
  types$Parameter <- .clean_parameter_names(types$Parameter, full = TRUE)


  # special handling hurdle- and zeroinfl-models ---------------------
  if (isTRUE(info$is_zero_inflated) || isTRUE(info$is_hurdle)) {
    names <- gsub("^(count_|zero_)", "", names)
    types$Parameter <- gsub("^(count_|zero_)", "", types$Parameter)
  }

  # special handling polr ---------------------
  if (inherits(model, "polr")) {
    original_names <- gsub("Intercept: ", "", original_names, fixed = TRUE)
    names <- gsub("Intercept: ", "", names, fixed = TRUE)
  }

  # special handling bracl ---------------------
  if (inherits(model, "bracl")) {
    names <- gsub("(.*):(.*)", "\\2", names)
  }

  # special handling DirichletRegModel ---------------------
  dirich_names <- NULL
  if (inherits(model, "DirichletRegModel")) {
    cf <- stats::coef(model)
    if (model$parametrization == "common") {
      pattern <- paste0("(", paste(model$varnames, collapse = "|"), ")\\.(.*)")
      dirich_names <- names <- gsub(pattern, "\\2", names(unlist(cf)))
    } else {
      dirich_names <- names <- gsub("(.*)\\.(.*)\\.(.*)", "\\3", names(unlist(cf)))
    }
    original_names <- names
    if (!is.null(dirich_names)) {
      types$Parameter <- dirich_names
    }
  }


  # remove "as.factor()", "log()" etc. from parameter names
  names <- .clean_parameter_names(names)


  for (i in seq_len(nrow(types))) {
    name <- types$Parameter[i]

    # No interaction
    if (!types$Type[i] %in% c("interaction", "nested", "simple")) {
      type <- types[i, ]
      names[i] <- .format_parameter(
        name,
        variable = type$Variable,
        type = type$Type,
        level = type$Level,
        brackets = brackets
      )

      # Interaction or nesting
    } else {
      components <- unlist(strsplit(name, ":", fixed = TRUE))
      is_nested <- types$Type[i] == "nested"
      is_simple <- types$Type[i] == "simple"

      for (j in seq_along(components)) {
        if (components[j] %in% types$Parameter) {
          type <- types[types$Parameter == components[j], ]

          ## TODO check if this is ok...

          # for models with multiple response categories, we might have same
          # variable for each response, thus we have multiple rows here,
          # where only one row is required.

          if (nrow(type) > 1) type <- type[1, ]

          components[j] <- .format_parameter(
            components[j],
            variable = type$Variable,
            type = type$Type,
            level = type$Level,
            brackets = brackets
          )
        } else if (components[j] %in% types$Secondary_Parameter) {
          type <- types[!is.na(types$Secondary_Parameter) & types$Secondary_Parameter == components[j], ]
          components[j] <- .format_parameter(
            components[j],
            variable = type[1, ]$Secondary_Variable,
            type = type[1, ]$Secondary_Type,
            level = type[1, ]$Secondary_Level,
            brackets = brackets
          )
        }
      }
      names[i] <- .format_interaction(
        components,
        type = types[i, "Type"],
        is_nested = is_nested,
        is_simple = is_simple,
        ...
      )
    }
  }

  # do some final formatting, like replacing underscores or dots with whitespace.
  names <- gsub("(\\.|_)(?![^\\[]*\\])", " ", names, perl = TRUE)
  # remove double spaces
  names <- gsub("  ", " ", names, fixed = TRUE)

  # "types$Parameter" here is cleaned, i.e. patterns like "log()", "as.factor()"
  # etc. are removed. However, these patterns are needed in "format_table()",
  # code-line x$Parameter <- attributes(x)$pretty_names[x$Parameter]
  # when we use "types$Parameter" here, matching of pretty names does not work,
  # so output will be NA resp. blank fields... Thus, I think we should use
  # the original parameter-names here.

  names(names) <- original_names # types$Parameter
  names
}


#' @keywords internal
.format_parameter <- function(name, variable, type, level, brackets = brackets) {
  # Factors
  if (type == "factor") {
    name <- .format_factor(name = name, variable = variable, brackets = brackets)
  }

  # Polynomials
  if (type %in% c("poly", "poly_raw")) {
    name <- .format_poly(name = name, variable = variable, type = type, degree = level, brackets = brackets)
  }

  # Splines
  if (type == "spline") {
    name <- .format_poly(name = name, variable = variable, type = type, degree = level, brackets = brackets)
  }

  # log-transformation
  if (type == "logarithm") {
    name <- .format_log(name = name, variable = variable, type = type, brackets = brackets)
  }

  # exp-transformation
  if (type == "exponentiation") {
    name <- .format_log(name = name, variable = variable, type = type, brackets = brackets)
  }

  # log-transformation
  if (type == "squareroot") {
    name <- .format_log(name = name, variable = variable, type = type, brackets = brackets)
  }

  # As Is
  if (type == "asis") {
    name <- variable
  }

  # Smooth
  if (type == "smooth") {
    name <- gsub("^smooth_(.*)\\[(.*)\\]", "\\2", name)
    name <- gsub("s(", "Smooth term (", name, fixed = TRUE)
  }

  # Ordered
  if (type == "ordered") {
    name <- paste(variable, level)
  }

  name
}


#' @keywords internal
.format_interaction <- function(components,
                                type,
                                is_nested = FALSE,
                                is_simple = FALSE,
                                interaction_mark = NULL,
                                ...) {
  # sep <- ifelse(is_nested | is_simple, " : ", " * ")
  # sep <- ifelse(is_nested, " / ", " * ")
  # sep <- ifelse(is_simple, " : ", ifelse(is_nested, " / ", " * "))
  if (is.null(interaction_mark)) {
    if (.unicode_symbols()) {
      sep <- "\u00D7"
    } else {
      sep <- "*"
    }
  } else {
    sep <- interaction_mark
  }

  # either use argument, or override with options
  sep <- paste0(" ", getOption("parameters_interaction", insight::trim_ws(sep)), " ")

  if (length(components) > 2) {
    if (type == "interaction") {
      components <- paste0(
        "(",
        paste0(utils::head(components, -1), collapse = sep),
        ")",
        sep,
        utils::tail(components, 1)
      )
    } else {
      components <- paste0(components, collapse = sep)
    }
  } else {
    components <- paste0(components, collapse = sep)
  }
  components
}


# format classes -----------------------------

#' @keywords internal
.format_factor <- function(name, variable, brackets = c("[", "]")) {
  level <- sub(variable, "", name, fixed = TRUE)

  # special handling for "cut()"
  pattern_cut_right <- "^\\((.*),(.*)\\]$"
  pattern_cut_left <- "^\\[(.*),(.*)\\)$"

  if (all(grepl(pattern_cut_right, level))) {
    lower_bounds <- gsub(pattern_cut_right, "\\1", level)
    upper_bounds <- gsub(pattern_cut_right, "\\2", level)
    level <- paste0(as.numeric(lower_bounds) + 1, "-", upper_bounds)
  } else if (all(grepl(pattern_cut_left, level))) {
    lower_bounds <- gsub(pattern_cut_left, "\\1", level)
    upper_bounds <- gsub(pattern_cut_left, "\\2", level)
    level <- paste0(lower_bounds, "-", as.numeric(upper_bounds) - 1)
  }
  paste0(variable, " ", brackets[1], level, brackets[2])
}


#' @keywords internal
.format_poly <- function(name, variable, type, degree, brackets = c("[", "]")) {
  paste0(variable, " ", brackets[1], format_order(as.numeric(degree), textual = FALSE), " degree", brackets[2])
}


#' @keywords internal
.format_log <- function(name, variable, type, brackets = c("[", "]")) {
  paste0(variable, " ", brackets[1], gsub("(.*)\\((.*)\\)", "\\1", name), brackets[2])
}


#' @keywords internal
.format_ordered <- function(degree, brackets = c("[", "]")) {
  switch(degree,
    ".L" = paste0(brackets[1], "linear", brackets[2]),
    ".Q" = paste0(brackets[1], "quadratic", brackets[2]),
    ".C" = paste0(brackets[1], "cubic", brackets[2]),
    paste0(
      brackets[1],
      parameters::format_order(as.numeric(gsub("^", "", degree, fixed = TRUE)), textual = FALSE),
      " degree",
      brackets[2]
    )
  )
}



# replace pretty names with value labels, when present ---------------

.format_value_labels <- function(params, model = NULL) {
  labels <- NULL
  if (is.null(model)) {
    model <- .get_object(params)
  }

  if (!is.null(model)) {
    # get data, but exclude response - we have no need for that label
    mf <- insight::get_data(model)
    resp <- insight::find_response(model, combine = FALSE)
    mf <- mf[, setdiff(colnames(mf), resp), drop = FALSE]

    # return variable labels, and for factors, add labels for each level
    lbs <- lapply(colnames(mf), function(i) {
      vec <- mf[[i]]
      if (is.factor(vec)) {
        variable_label <- attr(vec, "label", exact = TRUE)
        value_labels <- names(attr(vec, "labels", exact = TRUE))
        if (is.null(variable_label)) {
          variable_label <- i
        }
        if (is.null(value_labels)) {
          value_labels <- levels(vec)
        }
        out <- paste0(variable_label, " [", value_labels, "]")
      } else {
        out <- attr(vec, "label", exact = TRUE)
      }
      if (is.null(out)) {
        return(i)
      } else {
        return(out)
      }
    })

    # coefficient names (not labels)
    preds <- lapply(colnames(mf), function(i) {
      if (is.factor(mf[[i]])) {
        i <- paste0(i, levels(mf[[i]]))
      }
      i
    })

    # name elements
    names(lbs) <- names(preds) <- colnames(mf)
    labels <- tryCatch(stats::setNames(unlist(lbs), unlist(preds)), error = function(e) NULL)

    # retrieve pretty names attribute
    pn <- attributes(params)$pretty_names
    # replace former pretty names with labels, if we have any labels
    # (else, default pretty names are returned)
    if (!is.null(labels)) {
      # check if we have any interactions, and if so, create combined labels
      interactions <- pn[grepl(":", names(pn), fixed = TRUE)]
      if (length(interactions)) {
        labs <- c()
        for (i in names(interactions)) {
          # extract single coefficient names from interaction term
          out <- unlist(strsplit(i, ":", fixed = TRUE))
          # combine labels
          labs <- c(labs, paste0(sapply(out, function(l) labels[l]), collapse = " * "))
        }
        # add interaction terms to labels string
        names(labs) <- names(interactions)
        labels <- c(labels, labs)
      }
      # make sure "invalid" labels are ignored
      common_labels <- intersect(names(labels), names(pn))
      pn[common_labels] <- labels[common_labels]
    }
    labels <- pn
  }

  # missing labels return original parameter name (e.g., variance components in mixed models)
  out <- stats::setNames(params$Parameter, params$Parameter)
  labels <- labels[names(labels) %in% params$Parameter]
  out[match(names(labels), params$Parameter)] <- labels

  out
}


# helper -------------------

.unicode_symbols <- function() {
  # symbols only work on windows from R 4.2 and higher
  win_os <- tryCatch(
    {
      si <- Sys.info()
      if (!is.null(si["sysname"])) {
        si["sysname"] == "Windows" || startsWith(R.version$os, "mingw")
      } else {
        FALSE
      }
    },
    error = function(e) {
      TRUE
    }
  )
  l10n_info()[["UTF-8"]] && ((win_os && getRversion() >= "4.2") || (!win_os && getRversion() >= "4.0"))
}

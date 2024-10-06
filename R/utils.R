# small wrapper around this commonly used try-catch
.safe <- function(code, on_error = NULL) {
  if (isTRUE(getOption("easystats_errors", FALSE)) && is.null(on_error)) {
    code
  } else {
    tryCatch(code, error = function(e) on_error)
  }
}


#' help-functions
#' @keywords internal
.data_frame <- function(...) {
  x <- data.frame(..., stringsAsFactors = FALSE)
  rownames(x) <- NULL
  x
}


#' Safe transformation from factor/character to numeric
#'
#' @keywords internal
.factor_to_dummy <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }

  # get unique levels / values
  values <- if (is.factor(x)) {
    levels(x)
  } else {
    stats::na.omit(unique(x))
  }

  dummy <- as.data.frame(do.call(cbind, lapply(values, function(i) {
    out <- rep(0, length(x))
    out[is.na(x)] <- NA
    out[x == i] <- 1
    out
  })))

  colnames(dummy) <- values
  dummy
}


#' @keywords internal
.rename_values <- function(x, old, new) {
  x[x %in% old] <- new
  x
}


#' for models with zero-inflation component, return required component of model-summary
#' @keywords internal
.filter_component <- function(dat, component) {
  switch(component,
    conditional = dat[dat$Component == "conditional", ],
    zi = ,
    zero_inflated = dat[dat$Component == "zero_inflated", ],
    dat
  )
}



# Find log-terms inside model formula, and return "clean" term names
.log_terms <- function(model) {
  x <- insight::find_terms(model, flatten = TRUE)
  gsub("^log\\((.*)\\)", "\\1", grep("^log\\((.*)\\)", x, value = TRUE))
}


# Execute a function but store warnings (https://stackoverflow.com/a/4947528/4198688)
#' @keywords internal
.catch_warnings <- function(expr) {
  myWarnings <- NULL
  wHandler <- function(w) {
    myWarnings <<- c(myWarnings, list(w))
    invokeRestart("muffleWarning")
  }
  val <- withCallingHandlers(expr, warning = wHandler)
  list(out = val, warnings = myWarnings)
}



#' @keywords internal
.get_object <- function(x, attribute_name = "object_name") {
  obj_name <- attr(x, attribute_name, exact = TRUE)
  model <- NULL
  if (!is.null(obj_name)) {
    model <- .safe(get(obj_name, envir = parent.frame()))
    # prevent self reference
    if (is.null(model) || inherits(model, "parameters_model")) {
      model <- .safe(get(obj_name, envir = globalenv()))
    }
    # prevent self reference
    if (is.null(model) || inherits(model, "parameters_model")) {
      model <- .safe(.dynGet(obj_name))
    }
  }
  model
}


.is_semLme <- function(x) {
  all(inherits(x, c("sem", "lme")))
}


.insert_row_at <- function(data, row, index, default_value = NA) {
  # add missing columns
  new_columns <- setdiff(colnames(data), colnames(row))
  if (length(new_columns) > 0) {
    row[new_columns] <- default_value
  }
  # match column order
  row <- row[match(colnames(data), colnames(row))]

  # insert row
  if (index == 1) {
    rbind(row, data)
  } else if (index == (nrow(data) + 1)) {
    rbind(data, row)
  } else {
    rbind(data[1:(index - 1), ], row, data[index:nrow(data), ])
  }
}


.insert_element_at <- function(data, element, index) {
  if (index == 1) {
    c(element, data)
  } else if (index == (length(data) + 1)) {
    c(data, element)
  } else {
    c(data[1:(index - 1)], element, data[index:length(data)])
  }
}


.find_factor_levels <- function(model_data, model = NULL, model_call = NULL) {
  # check whether we have on-the-fly conversion of factors
  if (!is.null(model)) {
    model_terms <- insight::find_terms(model)
  } else if (!is.null(model_call)) { # nolint
    model_terms <- insight::find_terms(model_call)
  } else {
    model_terms <- NULL
  }
  # extract all model terms, we now have "as.factor(term)" etc., if any
  if (!is.null(model_terms$conditional)) {
    # extract variable names from "as.factor(term)" etc.
    factor_terms <- grep("(as\\.factor|factor|as\\.character)", model_terms$conditional, value = TRUE)
    cleaned <- gsub("(as\\.factor|factor|as\\.character)\\((.*)\\)", "\\2", factor_terms)
    # convert on-the-fly factors into real factors
    if (length(cleaned)) {
      for (i in seq_along(cleaned)) {
        model_data[[factor_terms[i]]] <- as.factor(model_data[[cleaned[i]]])
      }
    }
  }
  # extract levels from factors, so we know the reference level
  out <- lapply(colnames(model_data), function(i) {
    v <- model_data[[i]]
    if (is.factor(v)) {
      paste0(i, levels(v))
    } else if (is.character(v)) {
      paste0(i, unique(v))
    } else {
      NULL
    }
  })
  names(out) <- names(model_data)
  insight::compact_list(out)
}


# Almost identical to dynGet(). The difference is that we deparse the expression
# because get0() allows symbol only since R 4.1.0
.dynGet <- function(x,
                    ifnotfound = stop(gettextf("%s not found", sQuote(x)), domain = NA, call. = FALSE),
                    minframe = 1L,
                    inherits = FALSE) {
  x <- insight::safe_deparse(x)
  n <- sys.nframe()
  myObj <- structure(list(.b = as.raw(7)), foo = 47L)
  while (n > minframe) {
    n <- n - 1L
    env <- sys.frame(n)
    r <- get0(x, envir = env, inherits = inherits, ifnotfound = myObj)
    if (!identical(r, myObj)) {
      return(r)
    }
  }
  ifnotfound
}

.deprecated_warning <- function(old, new, verbose = TRUE) {
  if (verbose) {
    insight::format_warning(paste0(
      "Argument `", old,
      "` is deprecated and will be removed in the future. Please use `",
      new,
      "` instead."
    ))
  }
}

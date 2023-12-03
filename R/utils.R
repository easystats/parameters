# small wrapper around this commonly used try-catch
.safe <- function(code, on_error = NULL) {
  tryCatch(code, error = function(e) on_error)
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


.find_factor_levels <- function(data) {
  out <- lapply(colnames(data), function(i) {
    v <- data[[i]]
    if (is.factor(v)) {
      paste0(i, levels(v))
    } else {
      NULL
    }
  })
  names(out) <- names(data)
  insight::compact_list(out)
}


# Almost identical to dynGet(). The difference is that we deparse the expression
# because get0() allows symbol only since R 4.1.0
.dynGet <- function(x,
                    ifnotfound = stop(gettextf("%s not found", sQuote(x)), domain = NA),
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

#' help-functions
#' @keywords internal
.data_frame <- function(...) {
  x <- data.frame(..., stringsAsFactors = FALSE)
  rownames(x) <- NULL
  x
}


#' Recode a variable so its lowest value is beginning with zero
#'
#' @keywords internal
.recode_to_zero <- function(x) {
  # check if factor
  if (is.factor(x) || is.character(x)) {
    # try to convert to numeric
    x <- .factor_to_numeric(x)
  }

  # retrieve lowest category
  minval <- min(x, na.rm = TRUE)
  sapply(x, function(y) y - minval)
}



#' Safe transformation from factor/character to numeric
#' @keywords internal
.factor_to_numeric <- function(x, lowest = NULL) {
  if (is.numeric(x)) {
    return(x)
  }
  if (is.logical(x)) {
    return(as.numeric(x))
  }

  if (anyNA(suppressWarnings(as.numeric(as.character(stats::na.omit(x)))))) {
    if (is.character(x)) {
      x <- as.factor(x)
    }
    x <- droplevels(x)
    levels(x) <- 1:nlevels(x)
  }

  out <- as.numeric(as.character(x))
  if (!is.null(lowest)) {
    difference <- min(out) - lowest
    out <- out - difference
  }
  out
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


#' Find most common occurence
#'
#' @keywords internal
.find_most_common <- function(x) {
  out <- names(sort(table(x), decreasing = TRUE))[1]

  if (is.numeric(x)) out <- as.numeric(out)

  out
}


#' remove empty string from character
#' @keywords internal
.compact_character <- function(x) x[!sapply(x, function(i) nchar(i) == 0 || is.null(i) || any(i == "NULL", na.rm = TRUE))]



#' @keywords internal
.rename_values <- function(x, old, new) {
  x[x %in% old] <- new
  x
}


#' for models with zero-inflation component, return required component of model-summary
#' @keywords internal
.filter_component <- function(dat, component) {
  switch(component,
    "conditional" = dat[dat$Component == "conditional", ],
    "zi" = ,
    "zero_inflated" = dat[dat$Component == "zero_inflated", ],
    dat
  )
}



# Find log-terms inside model formula, and return "clean" term names
.log_terms <- function(model) {
  x <- insight::find_terms(model, flatten = TRUE)
  gsub("^log\\((.*)\\)", "\\1", x[grepl("^log\\((.*)\\)", x)])
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
.safe_deparse <- function(string) {
  paste0(sapply(deparse(string, width.cutoff = 500), trimws, simplify = TRUE), collapse = " ")
}


#' @keywords internal
.n_unique <- function(x, na.rm = TRUE) {
  if (is.null(x)) {
    return(0)
  }
  if (isTRUE(na.rm)) x <- stats::na.omit(x)
  length(unique(x))
}


#' @keywords internal
.get_object <- function(x, attribute_name = "object_name") {
  obj_name <- attr(x, attribute_name, exact = TRUE)
  model <- NULL
  if (!is.null(obj_name)) {
    model <- tryCatch(
      {
        get(obj_name, envir = parent.frame())
      },
      error = function(e) {
        NULL
      }
    )
    if (is.null(model)) {
      model <- tryCatch(
        {
          get(obj_name, envir = globalenv())
        },
        error = function(e) {
          NULL
        }
      )
    }
  }
  model
}


.is_semLme <- function(x) {
  all(inherits(x, c("sem", "lme")))
}


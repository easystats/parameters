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

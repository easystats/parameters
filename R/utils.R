#' help-functions
#' @keywords internal
.data_frame <- function(...) {
  x <- data.frame(..., stringsAsFactors = FALSE)
  rownames(x) <- NULL
  x
}

#' @keywords internal
.clean_confint <- function(ci) {
  estimate_row <- grep(pattern = "^estimate", x = rownames(ci), ignore.case = TRUE)
  if (length(estimate_row)) {
    ci <- ci[-estimate_row, ]
  }

  zi_col <- grep(pattern = "^zi\\.", x = colnames(ci), ignore.case = TRUE)
  if (length(zi_col)) {
    ci <- ci[, -zi_col, drop = FALSE]
  }

  colnames(ci) <- gsub("cond.", "", colnames(ci), fixed = TRUE)
  ci
}






#' Flatten a list
#'
#' @param object A list.
#' @param name Name of column of keys in the case the output is a dataframe.
#' @keywords internal
.flatten_list <- function(object, name = "name") {
  if (length(object) == 1) {
    object[[1]]
  } else if (all(sapply(object, is.data.frame))) {
    if (is.null(names(object))) {
      as.data.frame(t(sapply(object, rbind)))
    } else {
      tryCatch({
        rn <- names(object)
        object <- do.call(rbind, object)
        object[name] <- rn
        object[c(name, setdiff(names(object), name))]
      }, warning = function(w) {
        object
      }, error = function(e) {
        object
      })
    }
  } else {
    object
  }
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
#'
#' @importFrom stats na.omit
#' @keywords internal
.factor_to_numeric <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }

  if (anyNA(suppressWarnings(as.numeric(as.character(stats::na.omit(x)))))) {
    if (is.character(x)) {
      x <- as.factor(x)
    }
    levels(x) <- 1:nlevels(x)
  }

  as.numeric(as.character(x))
}



#' Find most common occurence
#'
#' @keywords internal
.find_most_common <- function(x) {
  out <- names(sort(table(x), decreasing = TRUE))[1]

  if (is.numeric(x)) out <- as.numeric(out)

  out
}



# remove NULL elements from lists
.compact_list <- function(x) x[!sapply(x, function(i) length(i) == 0 || is.null(i) || any(i == "NULL"))]



# remove empty string from character
.compact_character <- function(x) x[!sapply(x, function(i) nchar(i) == 0 || is.null(i) || any(i == "NULL"))]


.rename_values <- function(x, old, new) {
  x[x %in% old] <- new
  x
}


# for models with zero-inflation component, return
# required component of model-summary
.filter_component <- function(dat, component) {
  switch(
    component,
    "conditional" = dat[dat$Component == "conditional", ],
    "zi" = ,
    "zero_inflated" = dat[dat$Component == "zero_inflated", ],
    dat
  )
}



# Find log-terms inside model formula, and return "clean" term names
#' @importFrom insight find_terms
.log_terms <- function(model) {
  x <- insight::find_terms(model, flatten = TRUE)
  gsub("^log\\((.*)\\)", "\\1", x[grepl("^log\\((.*)\\)", x)])
}


# capitalize first character in string
.capitalize <- function(x) {
  capped <- grep("^[A-Z]", x, invert = TRUE)
  substr(x[capped], 1, 1) <- toupper(substr(x[capped], 1, 1))
  x
}

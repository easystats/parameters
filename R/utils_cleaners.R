#' @keywords internal
.clean_parameter_names <- function(x, full = FALSE) {
  # return if x is empty
  if (is.null(x) || length(x) == 0) {
    return("")
  }

  # here we need to capture only those patterns that we do *not* want to format
  # in a particular style. However, these patterns will not be shown in the output
  # from "model_parameters()". If certain patterns contain useful information,
  # remove them here and clean/prepare them in ".parameters_type_basic()".
  # for formatting / printing, refer to ".format_parameter()".

  pattern <- if (full) {
    c(
      "as.factor", "as.numeric", "factor", "offset", "lag", "diff", "catg",
      "asis", "matrx", "pol", "strata", "strat", "scale", "scored",
      "interaction", "lsp", "pb", "lo", "t2", "te", "ti", "tt",
      "mi", "mo", "gp"
    )
  } else {
    c("as.factor", "as.numeric", "factor", "catg", "asis", "interaction")
  }

  for (j in 1:length(pattern)) {
    # remove possible  namespace
    x <- sub("(.*)::(.*)", "\\2", x)
    if (pattern[j] == "offset") {
      x <- trimws(sub("offset\\(([^-+ )]*)\\)(.*)", "\\1\\2", x))
    } else if (pattern[j] == "I") {
      if (full) {
        x <- trimws(sub("I\\(((\\w|\\.)*).*", "\\1", x))
      } else {
        x <- trimws(sub("I\\((.*)\\)(.*)", "\\1", x))
      }
      # some exceptions here...
    } else if (full && pattern[j] == "scale" && any(grepl("scale\\(", x))) {
      x[grepl("scale\\(", x)] <- insight::clean_names(x[grepl("scale\\(", x)])
    } else {
      p <- paste0(pattern[j], "\\(((\\w|\\.)*)\\)(.*)")
      x <- trimws(sub(p, "\\1\\3", x))
    }
  }

  gsub("`", "", x, fixed = TRUE)
}




#' @keywords internal
.remove_backticks_from_string <- function(x) {
  if (is.character(x)) {
    x <- gsub("`", "", x, fixed = TRUE)
  }
  x
}

#' @keywords internal
insight::text_remove_backticks <- function(x) {
  if (is.data.frame(x) && "Parameter" %in% colnames(x)) {
    x$Parameter <- gsub("`", "", x$Parameter, fixed = TRUE)
  }
  x
}

#' @keywords internal
.intercepts <- function() {
  c(
    "(intercept)_zi",
    "intercept (zero-inflated)",
    "intercept",
    "zi_intercept",
    "(intercept)",
    "b_intercept",
    "b_zi_intercept"
  )
}

#' @keywords internal
.in_intercepts <- function(x) {
  tolower(x) %in% .intercepts() | grepl("^intercept", tolower(x))
}

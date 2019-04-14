#' help-functions
#' @keywords internal
data_frame <- function(...) {
  x <- data.frame(..., stringsAsFactors = FALSE)
  rownames(x) <- NULL
  x
}


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

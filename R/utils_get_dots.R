# function to evaluate dots in a tidyselect-style and return
# the variable names as character vector
.dot_variables <- function(dat, dots, verbose = TRUE) {

  columns <- colnames(dat)

  x <- unlist(lapply(dots, function(i) {
    if (grepl(":", i, fixed = TRUE)) {

      tmp <- unlist(strsplit(i, ":", fixed = TRUE))

      start <- if (.is_num_chr(tmp[1]))
        as.numeric(tmp[1])
      else
        which(columns == tmp[1])

      end <- if (.is_num_chr(tmp[2]))
        as.numeric(tmp[2])
      else
        which(columns == tmp[2])

      columns[start:end]

    } else {
      i
    }
  }))

  x <- unlist(lapply(x, function(i) {
    if (.is_num_chr(i))
      columns[as.numeric(i)]
    else if (.is_num_fac(i))
      columns[as.numeric(as.character(i))]
    else
      i
  }))

  not_found <- setdiff(x, columns)

  if (length(not_found) && isTRUE(verbose)) {
    insight::print_color(sprintf(
      "%i variables were not found in the dataset: %s\n",
      length(not_found),
      paste0(not_found, collapse = ", ")
    ),
    color = "red")
  }

  intersect(x, columns)
}

.is_num_chr <- function(x) {
  is.character(x) && !anyNA(suppressWarnings(as.numeric(na.omit(x))))
}

.is_num_fac <- function(x) {
  is.factor(x) && !anyNA(suppressWarnings(as.numeric(levels(x))))
}

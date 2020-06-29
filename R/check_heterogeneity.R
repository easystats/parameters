#' @rdname demean
#' @importFrom insight is_model find_random find_predictors get_data
#' @importFrom stats ave
#' @export
check_heterogeneity <- function(x, select = NULL, group = NULL) {
  if (insight::is_model(x)) {
    group <- insight::find_random(x, split_nested = TRUE, flatten = FALSE)
    if (is.null(group)) {
      stop("Model is no mixed model. Please provide a mixed model, or a data frame and arguments 'select' and 'group'.")
    }
    data <- insight::get_data(x)
    select <- insight::find_predictors(x, effects = "fixed", component = "conditional", flatten = TRUE)
  } else {
    data <- x
  }

  unique_groups <- .n_unique(data[[group]])
  combinations <- expand.grid(select, group)

  result <- mapply(function(predictor, id) {
    # demean predictor
    d <- demean(data, select = predictor, group = id, verbose = FALSE)

    # get new names
    within_name <- paste0(predictor, "_within")

    # check if any within-variable differs from zero. if yes, we have
    # a within-subject effect
    if (any(sum(abs(d[[within_name]]) > 1e-5) > 0)) {
      predictor
    } else {
      NULL
    }
  }, as.character(combinations[[1]]), as.character(combinations[[2]]), SIMPLIFY = FALSE)

  out <- unname(unlist(.compact_list(result)))

  if (is.null(out)) {
    message("No predictor found that could cause heterogeneity bias.")
    return(invisible(NULL))
  }

  class(out) <- c("check_heterogeneity", class(out))

  out
}




#' @export
print.check_heterogeneity <- function(x, ...) {
  cat("Possible heterogeneity bias due to following predictors: ")
  insight::print_color(paste(x, collapse = ", "), "red")
  cat("\n")
  invisible(x)
}

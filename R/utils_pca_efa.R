#' Get Scores from Principal Component Analysis (PCA)
#'
#' `get_scores()` takes `n_items` amount of items that load the most
#' (either by loading cutoff or number) on a component, and then computes their
#' average.
#'
#' @param x An object returned by [principal_components()].
#' @param n_items Number of required (i.e. non-missing) items to build the sum
#' score for an observation. If an observation has more missing values than
#' `n_items` in all items of a (sub) scale, `NA` is returned for that
#' observation, else, the sum score of all (sub) items is calculated. If `NULL`,
#' the value is chosen to match half of the number of columns in a data frame,
#' i.e. no more than 50% missing values are allowed.
#'
#' @details
#' `get_scores()` takes the results from [`principal_components()`] or
#' [`factor_analysis()`] and extracts the variables for each component found by
#' the PCA. Then, for each of these "subscales", row means are calculated (which
#' equals adding up the single items and dividing by the number of items). This
#' results in a sum score for each component from the PCA, which is on the same
#' scale as the original, single items that were used to compute the PCA.
#'
#' @return A data frame with subscales, which are average sum scores for all
#'   items from each component.
#'
#' @examplesIf insight::check_if_installed("psych", quietly = TRUE)
#' pca <- principal_components(mtcars[, 1:7], n = 2, rotation = "varimax")
#'
#' # PCA extracted two components
#' pca
#'
#' # assignment of items to each component
#' closest_component(pca)
#'
#' # now we want to have sum scores for each component
#' get_scores(pca)
#'
#' # compare to manually computed sum score for 2nd component, which
#' # consists of items "hp" and "qsec"
#' (mtcars$hp + mtcars$qsec) / 2
#'
#' @export
get_scores <- function(x, n_items = NULL) {
  subscales <- closest_component(x)
  dataset <- attributes(x)$dataset

  out <- lapply(sort(unique(subscales)), function(.subscale) {
    columns <- names(subscales)[subscales == .subscale]
    items <- dataset[columns]

    if (is.null(n_items)) {
      .n_items <- round(ncol(items) / 2)
    } else {
      .n_items <- n_items
    }

    apply(items, 1, function(i) {
      if (sum(!is.na(i)) >= .n_items) {
        mean(i, na.rm = TRUE)
      } else {
        NA
      }
    })
  })

  out <- as.data.frame(do.call(cbind, out))
  colnames(out) <- sprintf("Component_%i", seq_len(ncol(out)))

  out
}


# model parameters -----------------------------------------------------------------


#' @export
model_parameters.parameters_efa <- function(model, ...) {
  x <- attributes(model)$summary

  if (inherits(model, "parameters_efa")) {
    class(x) <- c("parameters_efa_summary", class(model))
  } else {
    class(x) <- c("parameters_pca_summary", class(model))
  }
  x
}


#' @export
model_parameters.parameters_pca <- model_parameters.parameters_efa


# summary -----------------------------------------------------------------


#' @export
summary.parameters_efa <- function(object, ...) {
  x <- attributes(object)$summary

  cols <- intersect(
    c("Std_Dev", "Eigenvalues", "Variance", "Variance_Cumulative", "Variance_Proportion"),
    colnames(x)
  )

  x <- as.data.frame(t(x[, cols]))
  x <- cbind(data.frame(Parameter = row.names(x), stringsAsFactors = FALSE), x)
  names(x) <- c("Parameter", attributes(object)$summary$Component)
  row.names(x) <- NULL

  if (.is_oblique_rotation(attributes(object)$rotation)) {
    factor_correlations <- attributes(object)$model$Phi
    if (!is.null(factor_correlations)) {
      attr(x, "factor_correlations") <- datawizard::rownames_as_column(
        as.data.frame(factor_correlations),
        var = "Factor"
      )
    }
  }

  if (inherits(object, "parameters_efa")) {
    class(x) <- c("parameters_efa_summary", class(object))
  } else {
    class(x) <- c("parameters_pca_summary", class(object))
  }
  x
}


#' @export
summary.parameters_pca <- summary.parameters_efa


#' @export
summary.parameters_omega <- function(object, ...) {
  class(object) <- c("parameters_omega_summary", "data.frame")
  object
}


# predict -----------------------------------------------------------------


#' @rdname principal_components
#' @export
predict.parameters_efa <- function(object,
                                   newdata = NULL,
                                   names = NULL,
                                   keep_na = TRUE,
                                   verbose = TRUE,
                                   ...) {
  attri <- attributes(object)

  # handle if no data is provided
  if (is.null(newdata)) {
    # check if we have scores attribute - these will be returned directly
    if ("scores" %in% names(attri)) {
      out <- as.data.frame(attri$scores)
      if (isTRUE(keep_na)) {
        out <- .merge_na(object, out, verbose)
      }
    } else if ("dataset" %in% names(attri)) {
      # if we have data, use that for prediction
      d <- attri$data_set
      d <- d[vapply(d, is.numeric, logical(1))]
      out <- as.data.frame(stats::predict(attri$model, newdata = d))
    } else {
      insight::format_error(
        "Could not retrieve data nor model. Please report an issue on {.url https://github.com/easystats/parameters/issues}." # nolint
      )
    }
  } else if (inherits(attri$model, "spca")) {
    # https://github.com/erichson/spca/issues/7
    newdata <- newdata[names(attri$model$center)]
    if (attri$standardize) {
      newdata <- sweep(newdata, MARGIN = 2, STATS = attri$model$center, FUN = "-", check.margin = TRUE)
      newdata <- sweep(newdata, MARGIN = 2, STATS = attri$model$scale, FUN = "/", check.margin = TRUE)
    }
    out <- as.matrix(newdata) %*% as.matrix(attri$model$loadings)
    out <- stats::setNames(as.data.frame(out), paste0("Component", seq_len(ncol(out))))
  } else if (inherits(attri$model, c("psych", "fa", "principal"))) {
    out <- as.data.frame(stats::predict(attri$model, data = newdata[rownames(attri$model$weights)], ...))
  } else {
    out <- as.data.frame(stats::predict(attri$model, newdata = newdata, ...))
  }

  if (!is.null(names)) {
    names(out)[seq_along(names)] <- names
  }
  row.names(out) <- NULL
  out
}

#' @export
predict.parameters_pca <- predict.parameters_efa


.merge_na <- function(object, out, verbose = TRUE) {
  compl_cases <- attributes(object)$complete_cases
  if (is.null(compl_cases)) {
    if (verbose) {
      insight::format_alert(
        "Could not retrieve information about missing data. Returning only complete cases."
      )
    }
  } else {
    original_data <- data.frame(.parameters_merge_id = seq_along(compl_cases))
    out$.parameters_merge_id <- (seq_len(nrow(original_data)))[compl_cases]
    out <- merge(original_data, out, by = ".parameters_merge_id", all = TRUE, sort = TRUE)
    out$.parameters_merge_id <- NULL
  }
  out
}


# print -------------------------------------------------------------------


#' @export
print.parameters_efa_summary <- function(x, digits = 3, ...) {
  # we may have factor correlations
  fc <- attributes(x)$factor_correlations

  if ("Parameter" %in% names(x)) {
    x$Parameter <- c(
      "Eigenvalues", "Variance Explained", "Variance Explained (Cumulative)",
      "Variance Explained (Proportion)"
    )
  } else if ("Component" %in% names(x)) {
    names(x) <- c(
      "Component", "Eigenvalues", "Variance Explained",
      "Variance Explained (Cumulative)", "Variance Explained (Proportion)"
    )
  }

  cat(insight::export_table(
    x,
    digits = digits,
    caption = c("# (Explained) Variance of Components", "blue"),
    format = "text",
    ...
  ))

  if (!is.null(fc)) {
    cat("\n")
    cat(insight::export_table(
      fc,
      digits = digits,
      caption = c("# Factor Correlations", "blue"),
      format = "text",
      ...
    ))
  }


  invisible(x)
}


#' @export
print.parameters_pca_summary <- print.parameters_efa_summary


#' @rdname principal_components
#' @export
print.parameters_efa <- function(x,
                                 digits = 2,
                                 sort = FALSE,
                                 threshold = NULL,
                                 labels = NULL,
                                 ...) {
  # extract attributes
  if (is.null(threshold)) {
    threshold <- attributes(x)$threshold
  }
  cat(.print_parameters_cfa_efa(
    x,
    threshold = threshold,
    sort = sort,
    format = "text",
    digits = digits,
    labels = labels,
    ...
  ))
  invisible(x)
}

#' @export
print.parameters_pca <- print.parameters_efa

#' @export
print.parameters_omega <- print.parameters_efa


#' @export
print.parameters_omega_summary <- function(x, ...) {
  out <- .print_omega_summary(x)
  cat(insight::export_table(out$tables, caption = out$captions, format = "text", ...))
  invisible(x)
}


# print-helper ----------------------


.print_omega_summary <- function(x, format = "text") {
  caption1 <- NULL
  caption2 <- NULL
  caption3 <- NULL

  # extract model
  model <- attributes(x)$model
  if (!is.null(model)) {
    stats <- data.frame(
      Statistic = c("Alpha", "G.6", "Omega (hierarchical)", "Omega (asymptotic H)", "Omega (total)"),
      Coefficient = c(model$alpha, model$G6, model$omega_h, model$omega.lim, model$omega.tot)
    )
    if (format == "text") {
      caption1 <- c("# Omega Statistics", "blue")
    } else {
      caption1 <- "Omega Statistics"
    }
  }

  # extract summary tables
  omega_coefficients <- attributes(x)$omega_coefficients
  variance_summary <- attributes(x)$summary

  # rename columns
  if (!is.null(omega_coefficients)) {
    names(omega_coefficients) <- c(
      "Composite", "Omega (total)", "Omega (hierarchical)", "Omega (group)"
    )
    if (format == "text") {
      caption2 <- c("# Omega Coefficients", "blue")
    } else {
      caption2 <- "Omega Coefficients"
    }
  }
  if (!is.null(variance_summary)) {
    names(variance_summary) <- c(
      "Composite", "Total (%)", "General Factor (%)",
      "Group Factor (%)"
    )
    if (format == "text") {
      caption3 <- c("# Variances", "blue")
    } else {
      caption3 <- "Variances"
    }
  }

  # list for export
  out <- insight::compact_list(list(stats, omega_coefficients, variance_summary))
  captions <- insight::compact_list(list(caption1, caption2, caption3))

  list(tables = out, captions = captions)
}


.print_parameters_cfa_efa <- function(x, threshold, sort, format, digits, labels, ...) {
  # Method
  if (inherits(x, "parameters_pca")) {
    method <- "Principal Component Analysis"
  } else if (inherits(x, "parameters_efa")) {
    method <- "Factor Analysis"
  } else {
    method <- "Omega"
  }

  # Rotation
  rotation_name <- attr(x, "rotation", exact = TRUE)

  # Labels
  if (!is.null(labels)) {
    x$Label <- labels
    x <- x[c("Variable", "Label", names(x)[!names(x) %in% c("Variable", "Label")])]
  }

  # Sorting
  if (isTRUE(sort)) {
    x <- .sort_loadings(x)
  }

  # Replace by NA all cells below threshold
  if (!is.null(threshold)) {
    x <- .filter_loadings(x, threshold = threshold)
  }

  # table caption
  if (is.null(rotation_name) || rotation_name == "none") {
    if (format %in% c("markdown", "html")) {
      table_caption <- sprintf("Loadings from %s (no rotation)", method)
    } else {
      table_caption <- c(sprintf("# Loadings from %s (no rotation)", method), "blue")
    }
  } else if (format %in% c("markdown", "html")) {
    table_caption <- sprintf("Rotated loadings from %s (%s-rotation)", method, rotation_name)
  } else {
    table_caption <- c(sprintf("# Rotated loadings from %s (%s-rotation)", method, rotation_name), "blue")
  }

  # footer
  if (is.null(attributes(x)$type)) {
    footer <- NULL
  } else {
    footer <- c(.text_components_variance(x, sep = ifelse(format %in% c("markdown", "html"), "", "\n")), "yellow")
  }

  # alignment?
  if (is.null(labels)) {
    alignment <- NULL
  } else {
    alignment <- paste(c("ll", rep("r", ncol(x) - 2)), collapse = "")
  }

  insight::export_table(
    x,
    digits = digits,
    format = format,
    caption = table_caption,
    footer = footer,
    align = alignment,
    ...
  )
}


#' @keywords internal
.text_components_variance <- function(x, sep = "") {
  type <- attributes(x)$type
  if (type %in% c("prcomp", "principal", "pca")) {
    type <- "principal component"
  } else if (type == "fa") {
    type <- "latent factor"
  } else if (type %in% c("kmeans", "hclust", "pvclust", "dbscan", "mixture", "pam")) {
    type <- "cluster"
  } else {
    type <- paste0(type, " component")
  }

  if (type == "cluster") {
    cluster_summary <- as.data.frame(x)
    variance <- attributes(x)$variance * 100
  } else {
    cluster_summary <- attributes(x)$summary
    variance <- max(cluster_summary$Variance_Cumulative) * 100
  }

  if (nrow(cluster_summary) == 1) {
    text_variance <- paste0("The unique ", type)
  } else {
    text_variance <- paste0("The ", nrow(cluster_summary), " ", type, "s")
  }

  # rotation
  if (!is.null(attributes(x)$rotation) && attributes(x)$rotation != "none") {
    text_variance <- paste0(text_variance, " (", attributes(x)$rotation, " rotation)")
  }


  text_variance <- paste0(
    text_variance,
    " accounted for ",
    sprintf("%.2f", variance),
    "% of the total variance of the original data"
  )

  if (type == "cluster" || nrow(cluster_summary) == 1) {
    text_variance <- paste0(text_variance, ".")
  } else {
    text_variance <- paste0(
      text_variance,
      " (",
      paste0(cluster_summary$Component,
        " = ",
        sprintf("%.2f", cluster_summary$Variance * 100),
        "%",
        collapse = ", "
      ),
      ")."
    )
  }
  paste0(sep, text_variance, sep)
}


# sort --------------------------------------------------------------------

#' @rdname principal_components
#' @export
sort.parameters_efa <- function(x, ...) {
  .sort_loadings(x)
}


#' @export
sort.parameters_pca <- sort.parameters_efa


#' @keywords internal
.sort_loadings <- function(loadings, cols = NULL) {
  if (is.null(cols)) {
    cols <- attributes(loadings)$loadings_columns
  }

  # Remove variable name column
  x <- loadings[, cols, drop = FALSE]
  row.names(x) <- NULL

  # Initialize clusters
  nitems <- nrow(x)
  loads <- data.frame(item = seq(1:nitems), cluster = rep(0, nitems))

  # first sort them into clusters: Find the maximum for each row and assign it to that cluster
  loads$cluster <- apply(abs(x), 1, which.max)
  ord <- sort(loads$cluster, index.return = TRUE)
  x[1:nitems, ] <- x[ord$ix, ]

  rownames(x)[1:nitems] <- rownames(x)[ord$ix]
  total.ord <- ord$ix

  # now sort column wise so that the loadings that have their highest loading on each cluster
  items <- table(loads$cluster) # how many items are in each cluster?
  first <- 1
  item <- loads$item
  for (i in seq_along(items)) {
    if (items[i] > 0) {
      last <- first + items[i] - 1
      ord <- sort(abs(x[first:last, i]), decreasing = TRUE, index.return = TRUE)
      x[first:last, ] <- x[item[ord$ix + first - 1], ]
      loads[first:last, 1] <- item[ord$ix + first - 1]
      rownames(x)[first:last] <- rownames(x)[ord$ix + first - 1]

      total.ord[first:last] <- total.ord[ord$ix + first - 1]
      first <- first + items[i]
    }
  }

  row_order <- row.names(x)
  loadings <- loadings[as.numeric(as.character(row_order)), ] # Arrange by max
  row.names(loadings) <- NULL

  loadings
}


# Filter --------------------------------------------------------------------


#' @keywords internal
.filter_loadings <- function(loadings, threshold = 0.2, loadings_columns = NULL) {
  if (is.null(loadings_columns)) {
    loadings_columns <- attributes(loadings)$loadings_columns
  }


  if (threshold == "max" || threshold >= 1) {
    if (threshold == "max") {
      for (row in seq_len(nrow(loadings))) {
        maxi <- max(abs(loadings[row, loadings_columns, drop = FALSE]))
        loadings[row, loadings_columns][abs(loadings[row, loadings_columns]) < maxi] <- NA
      }
    } else {
      for (col in loadings_columns) {
        loadings[utils::tail(order(abs(loadings[, col]), decreasing = TRUE), -round(threshold)), col] <- NA
      }
    }
  } else {
    loadings[, loadings_columns][abs(loadings[, loadings_columns]) < threshold] <- NA
  }

  loadings
}


# closest_component -------------------------------------------------------


#' @rdname principal_components
#' @export
closest_component <- function(pca_results) {
  if ("closest_component" %in% names(attributes(pca_results))) {
    attributes(pca_results)$closest_component
  } else {
    .closest_component(pca_results)
  }
}


.closest_component <- function(loadings, loadings_columns = NULL, variable_names = NULL) {
  if (is.matrix(loadings)) loadings <- as.data.frame(loadings)
  if (is.null(loadings_columns)) loadings_columns <- seq_len(ncol(loadings))
  if (is.null(variable_names)) variable_names <- row.names(loadings)
  component_columns <- apply(loadings[loadings_columns], 1, function(i) which.max(abs(i)))
  stats::setNames(component_columns, variable_names)
}

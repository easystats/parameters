# Recompute a cluster bootstrap by hand. The draws come from boot::boot() with
# the same seed and number of clusters. The drawn clusters of `group` are
# stacked in draw order, and each gets a label from its draw position in the
# columns `relabel`. Draws whose refit does not return the parameters of the
# model are dropped.
.refit_cluster_draws <- function(
  model,
  data,
  group,
  iterations,
  seed,
  relabel = group
) {
  clusters <- unique(as.character(data[[group]]))
  set.seed(seed)
  draws <- boot::boot(seq_along(clusters), function(x, i) i, R = iterations)$t
  param_names <- insight::get_parameters(model, verbose = FALSE)$Parameter
  n_params <- length(param_names)
  out <- lapply(seq_len(nrow(draws)), function(r) {
    pieces <- lapply(seq_len(ncol(draws)), function(i) {
      x <- data[as.character(data[[group]]) == clusters[draws[r, i]], , drop = FALSE]
      for (g in relabel) {
        x[[g]] <- paste(i, x[[g]], sep = "_")
      }
      x
    })
    d <- do.call(rbind, pieces)
    for (g in relabel) {
      d[[g]] <- factor(d[[g]], levels = unique(d[[g]]))
    }
    # evaluate the call from the global environment, because the environment of
    # helper files does not see packages that a test attaches
    refit_call <- stats::update(model, data = d, evaluate = FALSE)
    fit <- tryCatch(
      suppressWarnings(suppressMessages(eval(refit_call, list(d = d), globalenv()))),
      error = function(e) NULL
    )
    if (is.null(fit)) {
      return(rep(NA_real_, n_params))
    }
    params <- insight::get_parameters(fit, verbose = FALSE)
    if (identical(params$Parameter, param_names)) {
      params$Estimate
    } else {
      rep(NA_real_, n_params)
    }
  })
  out <- do.call(rbind, out)
  out[stats::complete.cases(out), , drop = FALSE]
}

.expect_cluster_bootstrap <- function(
  model,
  data,
  group,
  relabel = group,
  iterations = 5
) {
  set.seed(42)
  out <- bootstrap_model(model, iterations = iterations)
  expected <- .refit_cluster_draws(
    model,
    data,
    group,
    iterations,
    42,
    relabel = relabel
  )
  testthat::expect_identical(nrow(out), nrow(expected))
  testthat::expect_equal(unname(as.matrix(out)), unname(expected), tolerance = 1e-6)
}

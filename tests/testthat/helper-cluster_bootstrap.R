# Recompute a cluster bootstrap by hand. The draws come from boot::boot() with
# the same seed, number of clusters and `sim`. The drawn clusters are stacked in
# draw order, and each gets a label from its draw position. Draws whose refit
# does not return every parameter are dropped.
.refit_cluster_draws <- function(model, data, group, iterations, seed, sim = "ordinary") {
  clusters <- unique(as.character(data[[group]]))
  set.seed(seed)
  draws <- boot::boot(seq_along(clusters), function(x, i) i, R = iterations, sim = sim)$t
  n_params <- insight::n_parameters(model)
  out <- lapply(seq_len(nrow(draws)), function(r) {
    pieces <- lapply(seq_len(ncol(draws)), function(i) {
      x <- data[as.character(data[[group]]) == clusters[draws[r, i]], , drop = FALSE]
      x[[group]] <- paste(i, x[[group]], sep = "_")
      x
    })
    d <- do.call(rbind, pieces)
    d[[group]] <- factor(d[[group]], levels = unique(d[[group]]))
    fit <- tryCatch(
      suppressWarnings(suppressMessages(stats::update(model, data = d))),
      error = function(e) NULL
    )
    if (is.null(fit)) {
      return(rep(NA_real_, n_params))
    }
    est <- insight::get_parameters(fit, verbose = FALSE)$Estimate
    if (length(est) == n_params) est else rep(NA_real_, n_params)
  })
  out <- do.call(rbind, out)
  out[stats::complete.cases(out), , drop = FALSE]
}

.expect_cluster_bootstrap <- function(model, data, group, sim = "ordinary") {
  set.seed(42)
  out <- bootstrap_model(model, iterations = 20, type = sim)
  expected <- .refit_cluster_draws(model, data, group, 20, 42, sim = sim)
  testthat::expect_identical(nrow(out), nrow(expected))
  testthat::expect_equal(unname(as.matrix(out)), unname(expected), tolerance = 1e-6)
}

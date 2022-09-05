
average_parameters <- function(..., ci = .95, verbose = TRUE) {
  insight::check_if_installed("performance")
  models <- list(...)

  # compute model weights
  aic_values <- sapply(models, performance::performance_aic)
  delta_aic <- aic_values - min(aic_values)
  model_weights <- exp(-0.5 * delta_aic) / sum(exp(-0.5 * delta_aic))

  # residual df's
  residual_dfs <- sapply(models, degrees_of_freedom, method = "residual")

  # data grid for average predictions
  predictions <- lapply(models, function(m) {
    d <- insight::get_datagrid(m)
    new_data <- as.data.frame(lapply(d, function(i) {
      if (is.factor(i)) {
        as.factor(levels(i)[1])
      } else {
        unique(i)[1]
      }
    }))
    insight::get_predicted(n, data = new_data, ci = .95)
  })

  theta_hats <- unlist(predictions)
  se_theta_hats <- sapply(predictions, function(p) {
    attributes(p)$ci_data$SE
  })

  CI_low <- stats::uniroot(
    f = .tailarea, interval = c(-1e+10, 1e+10), theta.hats = theta_hats,
    se.theta.hats = se_theta_hats, model.weights = model_weights, alpha = alpha,
    residual.dfs = residual_dfs, tol = 1e-10
  )$root

  CI_high <- stats::uniroot(
    f = .tailarea, interval = c(-1e+10, 1e+10), theta.hats = theta_hats,
    se.theta.hats = se_theta_hats, model.weights = model_weights, alpha = 1 - alpha,
    residual.dfs = residual_dfs, tol = 1e-10
  )$root

  c(CI_low, CI_high)
}


.tailarea <- function(theta, theta.hats, se.theta.hats, model.weights, alpha, residual.dfs) {
  t.quantiles <- (theta - theta.hats) / se.theta.hats
  sum(model.weights * stats::pt(t.quantiles, df = residual.dfs)) - alpha
}

#' @export
averaged_parameters <- function(..., ci = .95, verbose = TRUE) {
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
      } else if (is.numeric(i)) {
        mean(i, na.rm = TRUE)
      } else {
        unique(i)[1]
      }
    }))
    insight::get_predicted(m, data = new_data, ci = .95, predict = "link")
  })

  theta_hats <- unlist(predictions)
  se_theta_hats <- sapply(predictions, function(p) {
    attributes(p)$ci_data$SE
  })

  alpha <- (1 - ci) / 2

  CI_low <- stats::uniroot(
    f = .tailarea, interval = c(-1e+10, 1e+10), theta_hats = theta_hats,
    se_theta_hats = se_theta_hats, model_weights = model_weights, alpha = alpha,
    residual_dfs = residual_dfs, tol = 1e-10
  )$root

  CI_high <- stats::uniroot(
    f = .tailarea, interval = c(-1e+10, 1e+10), theta_hats = theta_hats,
    se_theta_hats = se_theta_hats, model_weights = model_weights, alpha = 1 - alpha,
    residual_dfs = residual_dfs, tol = 1e-10
  )$root

  c(CI_low, CI_high)
}


.tailarea <- function(theta, theta_hats, se_theta_hats, model_weights, alpha, residual_dfs) {
  t_quantiles <- (theta - theta_hats) / se_theta_hats
  sum(model_weights * stats::pt(t_quantiles, df = residual_dfs)) - alpha
}

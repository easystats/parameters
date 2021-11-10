#' @rdname p_value_betwithin
#' @export
dof_betwithin <- function(model) {
  if (!insight::model_info(model, verbose = FALSE)$is_mixed) {
    stop("Model must be a mixed model.")
  }

  ngrps <- sum(.n_randomeffects(model))
  parameters <- insight::find_parameters(model, effects = "fixed")[["conditional"]]
  within_effects <- unlist(insight::find_random_slopes(model))
  has_intcp <- insight::has_intercept(model)

  ddf_within <- ngrps - n_parameters(model)
  ddf_between <- insight::n_obs(model, disaggregate = TRUE) - ngrps - n_parameters(model)

  if (has_intcp) {
    ddf_between <- ddf_between - 1
    ddf_within <- ddf_within - 1
  }

  within_index <- match(within_effects, parameters)
  ddf <- stats::setNames(1:length(parameters), parameters)

  if (length(within_index) > 0) {
    ddf[match(within_effects, parameters)] <- ddf_within
    ddf[-match(within_effects, parameters)] <- ddf_between
  } else {
    ddf <- ddf_between
  }

  ddf
}

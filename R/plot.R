#' @export
plot.parameters_sem <- function(x, ...) {
  insight::check_if_installed("see")
  NextMethod()
}


#' @export
plot.parameters_model <- function(x, ...) {
  insight::check_if_installed("see")
  NextMethod()
}


#' @export
plot.compare_parameters <- function(x, ...) {
  insight::check_if_installed("see")
  NextMethod()
}


#' @export
plot.parameters_stan <- function(x, ...) {
  insight::check_if_installed("see")
  NextMethod()
}


#' @export
plot.parameters_simulate <- function(x, ...) {
  insight::check_if_installed("see")
  NextMethod()
}


#' @export
plot.parameters_brms_meta <- function(x, ...) {
  insight::check_if_installed("see")
  NextMethod()
}


#' @export
plot.n_factors <- function(x, ...) {
  insight::check_if_installed("see")
  NextMethod()
}


#' @export
plot.parameters_distribution <- function(x, ...) {
  insight::check_if_installed("see")
  NextMethod()
}


#' @export
plot.n_clusters <- function(x, ...) {
  insight::check_if_installed("see")
  NextMethod()
}


#' @export
plot.parameters_pca <- function(x, ...) {
  insight::check_if_installed("see")
  NextMethod()
}


#' @export
plot.parameters_efa <- function(x, ...) {
  insight::check_if_installed("see")
  NextMethod()
}


#' @export
plot.cluster_analysis <- function(x, ...) {
  insight::check_if_installed("see")
  plot(datawizard::visualisation_recipe(x, ...))
}

#' @export
plot.cluster_analysis_summary <- function(x, ...) {
  insight::check_if_installed("see")
  plot(datawizard::visualisation_recipe(x, ...))
}

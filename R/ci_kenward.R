#' @rdname p_value_kenward
#' @export
ci_kenward <- function(model, ci = 0.95, ...) {
  UseMethod("ci_kenward")
}

#' @export
ci_kenward.default <- function(model, ci = 0.95, ...) {
  if (!.check_REML_fit(model)) {
    model <- stats::update(model, . ~ ., REML = TRUE)
  }

  df_kr <- dof_kenward(model)
  out <- lapply(ci, function(i) {
    .ci_dof(
      model = model,
      ci = i,
      dof = df_kr,
      effects = "fixed",
      component = "all",
      method = "kenward",
      se = attr(df_kr, "se", exact = TRUE)
    )
  })
  out <- do.call(rbind, out)
  row.names(out) <- NULL
  out
}

#' @export
ci_kenward.glmmTMB <- function(model, ci = 0.95, ...) {
  if (!.check_REML_fit(model)) {
    model <- stats::update(model, . ~ ., REML = TRUE)
  }

  df_kr <- dof_kenward(model)
  out <- lapply(ci, function(i) {
    .ci_dof(
      model = model,
      ci = i,
      dof = df_kr,
      effects = "fixed",
      component = "conditional", # for glmmTMB, only conditional
      method = "kenward",
      se = attr(df_kr, "se", exact = TRUE)
    )
  })
  out <- do.call(rbind, out)
  row.names(out) <- NULL
  out
}


.ci_kenward_dof <- function(model, ci = 0.95, df_kr) {
  if (inherits(model, "glmmTMB")) {
    component <- "conditional"
  } else {
    component <- "all"
  }
  out <- lapply(ci, function(i) {
    .ci_dof(
      model = model,
      ci = i,
      dof = df_kr$df_error,
      effects = "fixed",
      component = component,
      method = "kenward",
      se = df_kr$SE
    )
  })
  out <- do.call(rbind, out)
  row.names(out) <- NULL
  out
}
